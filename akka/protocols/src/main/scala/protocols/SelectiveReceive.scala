package protocols

import akka.actor.typed.Behavior
import akka.actor.typed.Behavior.{canonicalize, interpretMessage, isUnhandled, validateAsInitial}
import akka.actor.typed.scaladsl._

import scala.reflect.ClassTag

object SelectiveReceive {
  /**
    * @return A behavior that stashes incoming messages unless they are handled
    *         by the underlying `initialBehavior`
    * @param bufferCapacity Maximum number of messages to stash before throwing a `StashOverflowException`
    *                       Note that 0 is a valid size and means no buffering at all (ie all messages should
    *                       always be handled by the underlying behavior)
    * @param initialBehavior Behavior to decorate
    * @tparam T Type of messages
    */
  def apply[T: ClassTag](bufferCapacity: Int, initialBehavior: Behavior[T]): Behavior[T] =
    Behaviors.withStash(bufferCapacity)(intercept(bufferCapacity, _, validateAsInitial(initialBehavior)))

  /**
   * @return A behavior that interprets the incoming messages with the supplied `started`
   *         behavior to compute the next behavior. If the message has been unhandled, it
   *         is stashed in the `buffer`. If the message has been handled, the previously
   *         stashed messages are also sent to the next behavior.
   *
   * @param bufferSize Capacity of the StashBuffer
   * @param buffer     Buffer to stash unhandled messages to
   * @param behaviour  Behavior to decorate. Must be a valid “initial” behavior.
   * @tparam T         Type of messages
   */
  private def intercept[T: ClassTag](bufferSize: Int, buffer: StashBuffer[T], behaviour: Behavior[T]): Behavior[T] =
    Behaviors receive { case (ctx, msg) =>

      val next = interpretMessage(behaviour, ctx, msg)
      val nextCanonicalized = canonicalize(next, behaviour, ctx)

      if (isUnhandled(next)) {
        buffer stash msg
        intercept(bufferSize, buffer, nextCanonicalized)
      } else if (buffer.nonEmpty) {
        buffer unstashAll SelectiveReceive(bufferSize, nextCanonicalized)
      } else {
        intercept(bufferSize, buffer, nextCanonicalized)
      }
    }
}