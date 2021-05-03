/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode.{CopyFinished, CopyTo}
import akka.actor._

import scala.collection.immutable.{HashSet, Queue}

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef

    def id: Int

    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
   * in the tree. The actor at reference `requester` should be notified when
   * this operation is completed.
   */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
   * The actor at reference `requester` should be notified when this operation
   * is completed.
   */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection */
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
   * `result` is true if and only if the element is present in the tree.
   */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}

class BinaryTreeSet extends Actor {

  import BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root: ActorRef = createRoot

  // optional (used to stash incoming operations during garbage collection)
  var pendingQueue: Seq[Operation] = Queue.empty[Operation]

  // optional
  def receive: Receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {

    case op: Operation =>
      root ! op

    case GC =>
      val newRoot = createRoot
      context.become(garbageCollecting(newRoot))
      root ! CopyTo(newRoot)
  }

  // optional
  /** Handles messages while garbage collection is performed.
   * `newRoot` is the root of the new binary tree where we want to copy
   * all non-removed elements into.
   */
  def garbageCollecting(newRoot: ActorRef): Receive = {

    case op: Operation =>
      pendingQueue :+= op

    case CopyFinished =>
      pendingQueue.foreach(newRoot ! _)
      pendingQueue = Queue.empty
      root ! PoisonPill
      root = newRoot
      context.unbecome()

    case GC =>
  }
}

object BinaryTreeNode {

  trait Position

  case object Left extends Position

  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)

  /**
   * Acknowledges that a copy has been completed. This message should be sent
   * from a node to its parent, when this node and all its children nodes have
   * finished being copied.
   */
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(classOf[BinaryTreeNode], elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {

  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees: Map[Position, ActorRef] = Map[Position, ActorRef]()
  var removed: Boolean = initiallyRemoved

  // optional
  def receive: Receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {

    case op@Insert(requester, id, elem) =>

      def addressPosition(pos: Position): Unit = {
        subtrees.get(pos) match {

          case Some(nodeRef) =>
            nodeRef ! op

          case None =>
            subtrees += (pos -> context.actorOf(BinaryTreeNode.props(elem, false)))
            requester ! OperationFinished(id)
        }
      }

      if (elem == this.elem) {
        this.removed = false
        requester ! OperationFinished(id)
      } else if (elem < this.elem)
        addressPosition(Left)
      else
        addressPosition(Right)

    case op@Contains(requester, id, elem) =>

      def addressPosition(pos: Position): Unit = {
        subtrees.get(pos) match {
          case Some(nodeRef) => nodeRef ! op
          case None => requester ! ContainsResult(id, false)
        }
      }

      if (elem == this.elem)
        requester ! ContainsResult(id, !removed)
      else if (elem < this.elem)
        addressPosition(Left)
      else
        addressPosition(Right)

    case op@Remove(requester, id, elem) =>

      def addressPosition(pos: Position): Unit = {
        subtrees.get(pos) match {
          case Some(nodeRef) => nodeRef ! op
          case None => requester ! OperationFinished(id)
        }
      }

      if (elem == this.elem) {
        this.removed = true
        requester ! OperationFinished(id)
      } else if (elem < this.elem)
        addressPosition(Left)
      else
        addressPosition(Right)

    case copyTo@CopyTo(treeNode) =>
      if (!removed) treeNode ! Insert(self, -1, this.elem)

      var expectedChildren = HashSet[ActorRef]()

      subtrees.view.values.foreach {
        nodeRef =>
          expectedChildren += nodeRef
          nodeRef ! copyTo
      }

      if (expectedChildren.isEmpty && removed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expectedChildren, removed))
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
   * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
   */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {

    case OperationFinished(-1) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(expected, true))
      }

    case CopyFinished =>

      val updatedExpected = expected - sender()

      if (updatedExpected.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
      } else {
        context.become(copying(updatedExpected, insertConfirmed))
      }
  }
}