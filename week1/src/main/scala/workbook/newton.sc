def abs(x: Double) = if (x < 0) -x else x

def goodEnough(guess: Double, x: Double): Boolean = abs(guess * guess - x) / x < 0.001

def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (goodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def sqrt(x: Double): Double = sqrtIter(1, x)

sqrt(2)
sqrt(21)
sqrt(73)
sqrt(1e-6)
sqrt(1e60)
