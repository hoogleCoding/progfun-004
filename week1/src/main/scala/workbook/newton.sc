def abs(x: Double) = if (x < 0) -x else x
def sqrt(x: Double): Double = {
  def goodEnough(guess: Double) = abs(guess * guess - x) / x < 0.001
  def improve(guess: Double) = (guess + x / guess) / 2
  def sqrtIter(guess: Double): Double =
    if (goodEnough(guess)) guess
    else sqrtIter(improve(guess))
  sqrtIter(1)
}
sqrt(2)
sqrt(21)
sqrt(73)
sqrt(1e-6)
sqrt(1e60)
