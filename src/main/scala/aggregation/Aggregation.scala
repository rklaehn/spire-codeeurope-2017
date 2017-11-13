package aggregation

import spire.syntax.field._
import spire.syntax.nroot._
import spire.sp
import spire.algebra.{Field, Group, NRoot}

case class SumOfSquaresAggregate[@sp T](n: Int, sum: T, sumOfSquares: T) {

  def mean(implicit f: Field[T]): T = sum / n

  def variance(implicit f: Field[T]): T = {
    val mean = this.mean
    (sumOfSquares / n - mean * mean)
  }

  def sampleVariance(implicit f: Field[T]): T = {
    val n = Field.fromInt(this.n)
    val mean = this.mean
    (sumOfSquares / (n - 1) - mean * mean * n / (n - 1))
  }

  /**
    * Population standard deviation
    */
  def standardDeviation(implicit r: NRoot[T], f: Field[T]): T = {
    variance.sqrt()
  }

  def sampleStandardDeviation(implicit r: NRoot[T], f: Field[T]): T = {
    sampleVariance.sqrt()
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
      SumOfSquaresAggregate(-a.n, -a.sum, -a.sumOfSquares)

    override def combine(x: SumOfSquaresAggregate[T], y: SumOfSquaresAggregate[T]) =
      SumOfSquaresAggregate(
        x.n + y.n,
        x.sum + y.sum,
        x.sumOfSquares + y.sumOfSquares)
  }
}
