package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      if(delta() < 0) {
        Set()
      } else if(delta() == 0) {
        Set(- b() / (2 * a()))
      }
      else {
        val sol1 = (- b() + delta()) / (2 * a())
        val sol2 = (- b() - delta()) / (2 * a())
        Set(sol1, sol2)
      }
    )
  }
}
