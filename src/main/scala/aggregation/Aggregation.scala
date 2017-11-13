package aggregation

import spire.syntax.field._
import spire.syntax.nroot._
import spire.sp
import spire.algebra.{Field, Group, NRoot}

case class SumOfSquaresAggregate[@sp T: Field](count: Int, sum: T, sumOfSquares: T) {

  def mean: T = sum / Field.fromInt(count)

  /**
    * Population standard deviation
    */
  def standardDeviation(implicit r: NRoot[T]): T = {
    val n = Field.fromInt(count)
    val mean = this.mean
    (sumOfSquares / n - mean * mean).sqrt()
  }

  def sampleStandardDeviation(implicit r: NRoot[T]): T = {
    val n = Field.fromInt(count)
    val mean = this.mean
    (sumOfSquares / (n - 1) - mean * mean * n / (n - 1)).sqrt()
  }
}

object SumOfSquaresAggregate {

  def empty[@sp T: Field]: SumOfSquaresAggregate[T] =
    SumOfSquaresAggregate(0, Field.zero[T], Field.zero[T])

  def apply[@sp T: Field](value: T): SumOfSquaresAggregate[T] =
    SumOfSquaresAggregate(1, value, value * value)

  implicit def group[T: Field]: Group[SumOfSquaresAggregate[T]] = new Group[SumOfSquaresAggregate[T]] {

    override def empty = SumOfSquaresAggregate.empty[T]

    override def inverse(a: SumOfSquaresAggregate[T]) =
      SumOfSquaresAggregate(-a.count, -a.sum, -a.sumOfSquares)

    override def combine(x: SumOfSquaresAggregate[T], y: SumOfSquaresAggregate[T]) =
      SumOfSquaresAggregate(
        x.count + y.count,
        x.sum + y.sum,
        x.sumOfSquares + y.sumOfSquares)
  }
}
