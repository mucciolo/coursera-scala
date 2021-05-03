package streaming

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.concurrent.Future

object SimpleStreaming extends ExtraStreamOps with SimpleStreamingInterface {

  /** Change each of the streamed elements to their String values */
  def mapToStrings(ints: Source[Int, NotUsed]): Source[String, NotUsed] = ints.map(_.toString)

  /** Filter elements which are even (use the modulo operator: `%`) */
  def filterEvenValues: Flow[Int, Int, NotUsed] = Flow[Int].filter(_ % 2 == 0)

  def filterUsingPreviousFilterFlowAndMapToStrings(ints: Source[Int, NotUsed]): Source[String, NotUsed] =
    mapToStrings(ints.via(filterEvenValues))

  def filterUsingPreviousFlowAndMapToStringsUsingTwoVias(ints: Source[Int, NotUsed], toString: Flow[Int, String, _]): Source[String, NotUsed] =
    ints.via(filterEvenValues).via(toString)

  def firstElementSource(ints: Source[Int, NotUsed]): Source[Int, NotUsed] = ints.take(1)

  def firstElementFuture(ints: Source[Int, NotUsed])(implicit mat: Materializer): Future[Int] =
    firstElementSource(ints).runWith(Sink.head)

  // --- failure handling ---
  /**
   * Recover [[IllegalStateException]] values to a -1 value
   */
  def recoverSingleElement(ints: Source[Int, NotUsed]): Source[Int, NotUsed] =
    ints recover { case _: IllegalStateException => -1 }

  /**
   * Recover [[IllegalStateException]] values to the provided fallback Source
   *
   */
  def recoverToAlternateSource(ints: Source[Int, NotUsed], fallback: Source[Int, NotUsed]): Source[Int, NotUsed] =
    ints.recoverWithRetries(3, { case _: IllegalStateException => fallback})

  /**
   * Provides a Flow that will be able to continue receiving elements from its upstream Source
   * and "sum up" values while the downstream (the Sink to which this Flow will be attached).
   *
   * In this way we are able to keep the upstream running it its nominal rate, while accumulating
   * all information. The downstream will then consume the accumulated values also at its nominal rate,
   * which in this case we know / expect to be slower than the upstream.
   *
   * If the downstream would happen to be faster than the upstream, no such aggregation is needed,
   * and the elements can be passed through directly.
   */
  def sumUntilBackpressureGoesAway: Flow[Int, Int, _] = Flow[Int].conflate(_ + _)

  /**
   * A faster downstream wants to consume elements, yet the upstream is slow at providing them.
   * Provides a Flow that is able to extrapolate "invent" values by repeating the previously emitted value.
   *
   * This could be seen as a "keepLast", where the stage keeps the last observed value from upstream.
   */
  def keepRepeatingLastObservedValue: Flow[Int, Int, _] = Flow[Int].extrapolate(Iterator.continually(_), Option(1))
}