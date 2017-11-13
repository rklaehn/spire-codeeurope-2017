package aggregation

import spire.algebra.{Field, Group, NRoot}
import spire.syntax.field._
import spire.syntax.nroot._
import spire.sp

// https://izbicki.me/blog/gausian-distributions-are-monoids.html
case class GaussianGroupAggregate[@sp T](n: Int, m: T, s: T) {
  def mean = m

  def variance(implicit f: Field[T]): T = s / n

  def sampleVariance(implicit f: Field[T]): T = s / (n - 1)

  def standardDeviation(implicit r: NRoot[T], f: Field[T]) = variance.sqrt()

  def sampleStandardDeviation(implicit r: NRoot[T], f: Field[T]) = sampleVariance.sqrt()
}

object GaussianGroupAggregate {

  def empty[@sp T: Field]: GaussianGroupAggregate[T] =
    GaussianGroupAggregate(0, Field.zero[T], Field.zero[T])

  def apply[@sp T: Field](value: T): GaussianGroupAggregate[T] =
    GaussianGroupAggregate(1, value, Field.zero[T])

  implicit def group[T: Field]: Group[GaussianGroupAggregate[T]] = new Group[GaussianGroupAggregate[T]] {

    override def empty = GaussianGroupAggregate.empty[T]

    override def inverse(a: GaussianGroupAggregate[T]) =
      GaussianGroupAggregate(-a.n, -a.m, -a.s)

    override def combine(a: GaussianGroupAggregate[T], b: GaussianGroupAggregate[T]) =
      if (a.n == 0) b else if (b.n == 0) a else {
        val n: Int = a.n + b.n
        // m = m0 + (x - m0) / n
        // s = s0 + (x - m0) * (x - m)
        val m1: T = (a.n * a.m + b.n * b.m) / n
        val s1: T = a.s + b.s + a.n * a.m * a.m + b.n * b.m * b.m - n * m1 * m1
        GaussianGroupAggregate(n, m1, s1)
      }
  }
}

case class MomentsAggregate[T: Field](n: Int, m1: T, m2: T, m3: T, m4: T)

object MomentsAggregate {

  def empty[T: Field]: MomentsAggregate[T] = MomentsAggregate(0, Field[T].zero, Field[T].zero, Field[T].zero, Field[T].zero)
}
