package calculator

object Polynomial extends PolynomialInterface {
	def computeDelta(a: Signal[Double], b: Signal[Double],
					 c: Signal[Double]): Signal[Double] = {
		Signal(b() * b() - 4 * a() * c())
	}

	def computeSolutions(a: Signal[Double], b: Signal[Double],
						 c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
		Signal(
			delta() match {
				case x if x < 0 => Set.empty
				case _ =>
					val twoA = 2 * a()
					val minusB = -b()
					val deltaSqrt = math.sqrt(delta())

					Set((minusB - deltaSqrt) / twoA, (minusB + deltaSqrt) / twoA)
			}
		)
	}
}
