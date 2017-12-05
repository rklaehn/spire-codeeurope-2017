package aggregation

import spire.algebra._
import spire.implicits._

case class SumOfSquaresAggregate[T](n: Int, sum: T, sumOfSquares: T) {

}

object SumOfSquaresAggregate {

  def empty[T]: SumOfSquaresAggregate[T] = ???

  def apply[T](value: T): SumOfSquaresAggregate[T] = ???

  implicit def eq[T]: Eq[SumOfSquaresAggregate[T]] = ???

  implicit def group[T]: Group[SumOfSquaresAggregate[T]] = ???
}
