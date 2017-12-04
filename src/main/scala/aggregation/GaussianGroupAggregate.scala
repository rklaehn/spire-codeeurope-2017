package aggregation

import spire.algebra._
import spire.syntax.all._
import spire.sp

// https://izbicki.me/blog/gausian-distributions-are-monoids.html
/**
  * @param n the number of samples
  * @param m the mean
  * @param s the pre-weighted variance (variance * n)
  */
case class GaussianGroupAggregate[@sp T](n: Int, m: T, s: T) {

  def mean = m

  def variance(implicit g: Field[T]): T = s / n

  def sampleVariance(implicit f: Field[T]): T = s / (n - 1)

  def standardDeviation(implicit r: NRoot[T], f: Field[T]) = variance.sqrt()

  def sampleStandardDeviation(implicit r: NRoot[T], f: Field[T]) = sampleVariance.sqrt()
}

object GaussianGroupAggregate {

  def empty[@sp T: AdditiveMonoid]: GaussianGroupAggregate[T] =
    GaussianGroupAggregate(0, AdditiveMonoid.zero[T], AdditiveMonoid.zero[T])

  def apply[@sp T: AdditiveMonoid](value: T): GaussianGroupAggregate[T] =
    GaussianGroupAggregate(1, value, AdditiveMonoid.zero[T])

  implicit def monoid[T: Field]: Monoid[GaussianGroupAggregate[T]] = new Monoid[GaussianGroupAggregate[T]] {

    override def empty = GaussianGroupAggregate.empty[T]

    override def combine(a: GaussianGroupAggregate[T], b: GaussianGroupAggregate[T]) =
      if (b.n == 0) a else if (a.n == 0) b else {
        val u = a.m
        val x = a.s
        val i = a.n

        val v = b.m
        val y = b.s
        val j = b.n

        val n: Int = i + j

        // weighted average of means
        val m: T = (u * i + v * j) / n

        // weighted sum of moments, remember x and y are pre-weighted. No floating point problems here.
        val s0 = x + y

        // for the moment caused by the mean being different, only the difference matters
        // it is crucial to calculate the result in terms of the difference to limit floating point errors
        val vu = v - u

        // intuitively, this contribution is biggest if both are of the same weight ( i ~= j )
        val sd = (i * j * vu * vu) / n

        val s: T = s0 + sd
        GaussianGroupAggregate(n, m, s)
      }
  }

  implicit def eq[T: Eq]: Eq[GaussianGroupAggregate[T]] =
    Eq.instance((a, b) => a.n == b.n && a.m === b.m && a.s === b.s)
}
