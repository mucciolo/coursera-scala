package kvstore

import akka.actor.{Actor, ActorRef, Props}

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import context.dispatcher

  /** seq -> (replicateSender, replicate) */
  private var awaitingSnapshotAck = Map.empty[Long, (ActorRef, Replicate)]

  private var seqCounter = 0L

  private def nextSeq() = {
    val ret = seqCounter
    seqCounter += 1
    ret
  }

  override def preStart(): Unit = {
    context.system.scheduler.scheduleAtFixedRate(100 milliseconds, 100 milliseconds)(
      () => resendUnacknowledgedSnapshots()
    )
  }

  private def resendUnacknowledgedSnapshots() = {
      awaitingSnapshotAck foreach {
        case (seq, (_, repl)) => sendSnapshot(repl.key, repl.valueOption, seq)
      }
  }

  def receive: Receive = {

    case repl@Replicate(key, valueOption, _) =>
      val seq = nextSeq()
      sendSnapshot(key, valueOption, seq)
      awaitingSnapshotAck += seq -> (sender(), repl)

    case SnapshotAck(key, seq) =>
      awaitingSnapshotAck.get(seq).foreach {
        case (sender, replicate) => sender ! Replicated(key, replicate.id)
      }
      awaitingSnapshotAck -= seq
  }

  private def sendSnapshot(key: String, valueOption: Option[String], seq: Long) = {
    replica ! Snapshot(key, valueOption, seq)
  }
}
