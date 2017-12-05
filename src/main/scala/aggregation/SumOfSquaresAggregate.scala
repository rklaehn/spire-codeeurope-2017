package aggregation

import spire.algebra._
import spire.implicits._

case class SumOfSquaresAggregate[T](n: Int, sum: T, sumOfSquares: T) {

}

object SumOfSquaresAggregate {

  def empty[T]: SumOfSquaresAggregate[T] = ???

  def apply[T](value: T): SumOfSquaresAggregate[T] = ???

  // exercise 2a
  implicit def eq[T]: Eq[SumOfSquaresAggregate[T]] = ???

  // exercise 2b
  implicit def monoid[T]: Monoid[SumOfSquaresAggregate[T]] = ???

  // exercise 2d
  /**
    * Takes a sequence of values and computes count, mean and variance
    */
  def variance[T](values: Seq[T]): (Int, T, T) = {
    ???
  }
}
