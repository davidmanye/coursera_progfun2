package calculator

object Polynomial {

  //Δ = b² - 4ac
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val av = a()
      val bv = b()
      val cv = c()
      (bv * bv) - 4 * av * cv
    }
  }

  //(-b ± √Δ) / 2a
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val av = a()
      val bv = b()
      val cv = c()
      val deltav = computeDelta(a, b, c)()
      Set(
        (-bv + Math.sqrt(deltav)) / (2 * av),
        (-bv - Math.sqrt(deltav)) / (2 * av)
      )
    }
  }
}
