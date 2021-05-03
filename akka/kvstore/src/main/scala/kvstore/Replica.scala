package kvstore

import akka.actor.{Actor, ActorRef, Props}
import kvstore.Arbiter._
import kvstore.Persistence.{Persist, Persisted}

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object Replica {

  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case class FailedPersist(key: String, id: Long)
  case class FailedReplication(id: Long)

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {

  import Replica._
  import Replicator._
  import context.dispatcher

  arbiter ! Join

  private val persistence: ActorRef = context.actorOf(persistenceProps)

  private var keyValue = Map.empty[String, String]

  /** replica -> replicator */
  private var replicatorByReplica = Map.empty[ActorRef, ActorRef]

  /** id -> persist */
  private var awaitingPersistConfirmation = Map.empty[Long, Persist]

  /** id -> set[replicator] */
  private var awaitingReplicationConfirmation = Map.empty[Long, Set[ActorRef]]

  /** id -> sender */
  private var senderWaitingAck = Map.empty[Long, ActorRef]

  private var seqCounter = 0L

  private def nextSeq() = {
    val ret = seqCounter
    seqCounter += 1
    ret
  }

  def resendUnconfirmedPersists(): Unit = {
    if (awaitingPersistConfirmation.nonEmpty)
      awaitingPersistConfirmation.values.foreach(sendPersist)
  }

  override def preStart(): Unit = {
    context.system.scheduler.scheduleAtFixedRate(100 milliseconds, 100 milliseconds)(
      () => resendUnconfirmedPersists()
      )
  }

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  private def handleOperation(op: Operation): Unit = op match {

    case Insert(key, value, id) =>
      keyValue += (key -> value)
      senderWaitingAck += id -> sender()

      sendPersistAndWaitConfirmation(Persist(key, Some(value), id))
      context.system.scheduler.scheduleOnce(1 second, self, FailedPersist(key, id))

      if (replicatorByReplica.nonEmpty) {
        sendReplicateAndWaitConfirmation(key, Some(value), id)
        context.system.scheduler.scheduleOnce(1 second, self, FailedReplication(id))
      }

    case Remove(key, id) =>
      keyValue -= key
      if (replicatorByReplica.nonEmpty) {
        sendReplicateAndWaitConfirmation(key, None, id)
        context.system.scheduler.scheduleOnce(1 second, self, FailedReplication(id))
      }
      sender() ! OperationAck(id)

    case Get(key, id) =>
      sender() ! GetResult(key, keyValue.get(key), id)
  }

  private def sendReplicateAndWaitConfirmation(key: String, value: Option[String], id: Long) = {
    awaitingReplicationConfirmation +=
      id -> (awaitingReplicationConfirmation.getOrElse(id, Set.empty) ++ replicatorByReplica.values)
    replicatorByReplica.values.foreach(_ ! Replicate(key, value, id))
  }

  def getReplicaFromReplicator(replicator: ActorRef): Option[ActorRef] = {
    replicatorByReplica.find { case (_, _replicator) => _replicator == replicator }
                       .map(_._1)
  }

  private val leader: Receive = {

    case op: Operation => handleOperation(op)

    case Persisted(_, id) =>
      awaitingPersistConfirmation -= id
      if (awaitingReplicationConfirmation.getOrElse(id, Set.empty).isEmpty) acknowledgeOperation(id)

    case FailedPersist(_, id) =>
      if (awaitingPersistConfirmation.contains(id)) {
        awaitingPersistConfirmation -= id
        senderWaitingAck.get(id).foreach(_ ! OperationFailed(id))
        senderWaitingAck -= id
      }

    case Replicated(_, id) =>
      val unconfirmedReplicatorsSet = awaitingReplicationConfirmation.getOrElse(id, Set.empty).excl(sender())

      if (unconfirmedReplicatorsSet.isEmpty)
        awaitingReplicationConfirmation -= id
      else
        awaitingReplicationConfirmation += id -> unconfirmedReplicatorsSet

      if (!awaitingPersistConfirmation.contains(id) && unconfirmedReplicatorsSet.isEmpty) acknowledgeOperation(id)

    case FailedReplication(id) =>
      awaitingReplicationConfirmation.get(id) match {
        case Some(set) =>
          if (set.nonEmpty) {
            awaitingReplicationConfirmation -= id
            senderWaitingAck.get(id).foreach(_ ! OperationFailed(id))
            senderWaitingAck -= id
          }

        case None =>
      }

    case Replicas(newReplicaSet) =>
      val currentReplicaSet = replicatorByReplica.keys.toSet
      val removedReplicas = currentReplicaSet.removedAll(newReplicaSet)
      val removedReplicators = removedReplicas.map(replicatorByReplica(_))

      awaitingReplicationConfirmation = awaitingReplicationConfirmation.map {
        case (id, awaitingReplicatorSet) => (id, awaitingReplicatorSet.removedAll(removedReplicators))
      }

      removedReplicators.foreach(context.stop)
      replicatorByReplica --= removedReplicas

      senderWaitingAck.foreach {
        case (id, sender) =>
          if (!awaitingPersistConfirmation.contains(id)
            && awaitingReplicationConfirmation.getOrElse(id, Set.empty).isEmpty) sender ! OperationAck(id)
      }

      val addedReplicaReplicatorPair = newReplicaSet.excl(self)
                                                    .removedAll(replicatorByReplica.keys)
                                                    .map(replica => replica -> context.actorOf(Replicator.props(replica)))

      keyValue.foreach {
        case (key, value) => addedReplicaReplicatorPair.foreach {
          case (_, replicator) => replicator ! Replicate(key, Some(value), -1)
        }
      }

      replicatorByReplica ++= addedReplicaReplicatorPair
  }

  private def acknowledgeOperation(id: Long) = {
    senderWaitingAck.get(id).foreach(_ ! OperationAck(id))
    senderWaitingAck -= id
  }

  private val replica: Receive = {

    case op: Operation => handleOperation(op)

    case Snapshot(key, valueOption, seq) =>
      if (seq == seqCounter) {
        senderWaitingAck += seq -> sender()

        valueOption match {
          case Some(value) => keyValue += (key -> value)
          case None => keyValue -= key
        }

        sendPersistAndWaitConfirmation(Persist(key, valueOption, nextSeq()))

      } else if (seq < seqCounter) {
        sender() ! SnapshotAck(key, seq)
      }

    case Persisted(key, id) =>
      awaitingPersistConfirmation -= id
      senderWaitingAck(id) ! SnapshotAck(key, id)
      senderWaitingAck -= id
  }

  private def sendPersistAndWaitConfirmation(persist: Persist) = {
    awaitingPersistConfirmation += (persist.id -> persist)
    sendPersist(persist)
  }

  private def sendPersist(persist: Persist) = {
    persistence ! persist
  }
}