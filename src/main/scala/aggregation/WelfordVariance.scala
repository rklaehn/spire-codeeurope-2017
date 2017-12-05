package aggregation

// exercise 3c
class WelfordState[T](n: Int, m: T, s: T)

class WelfordVariance {

  /**
    * Takes a sequence of values and computes count, mean and variance
    */
  def variance[T](values: Seq[T]): (Int, T, T) = ???
}
