package reductions

import org.scalameter._

import scala.annotation._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
    ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    parenthesesStack(chars, 0) == 0
  }

  @tailrec
  def parenthesesStack(chars: Array[Char], stackHeight: Int): Int = {
    if (stackHeight < 0 || chars.isEmpty)
      stackHeight
    else {
      val inc = if (chars.head == '(') 1
      else if (chars.head == ')') -1
      else 0

      parenthesesStack(chars.tail, stackHeight + inc)
    }
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, to: Int): (Int, Int) = {

      var total = 0
      var i = from

      while (i < to) {
        if (chars(i) == '(')
          total += 1
        else if (chars(i) == ')')
          total -= 1

        i += 1
      }

      (Math.min(0, total), total)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {

      if (until - from <= threshold) {
        traverse(from, until)
      } else {
        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))

        (Math.min(left._1, left._2 + right._1), left._2 + right._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
}