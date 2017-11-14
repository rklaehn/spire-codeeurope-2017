package aggregation

import spire.algebra._
import spire.syntax.field._
import spire.syntax.nroot._
import spire.syntax.eq._
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

  implicit def group[T: Field]: Monoid[GaussianGroupAggregate[T]] = new Monoid[GaussianGroupAggregate[T]] {

    override def empty = GaussianGroupAggregate.empty[T]

    override def combine(a: GaussianGroupAggregate[T], b: GaussianGroupAggregate[T]) =
      if (a.n > b.n) combine0(a, b) else combine0(b, a)

    // this assumes that a.n >= b.n
    private def combine0(a: GaussianGroupAggregate[T], b: GaussianGroupAggregate[T]) =
      if (b.n == 0) a else {
        val n: Int = a.n + b.n

        // m = m0 + (x - m0) / n
        val m: T = a.m + (b.m - a.m) * b.n / n // this now degenerates to the welford method for b.n == 1

        // s = s0 + (x - m0) * (x - m)
        // b.n == 1
        // b.s == 0
        // b.m == x

        val as = a.s
        val bs = b.s
        val am = a.m
        val bm = b.m
        val an = a.n
        val bn = b.n
        // as + bs + an * am * am + bn * bm * bm - n * m * m
        val s: T = a.s + b.s + a.n * a.m * a.m + b.n * b.m * b.m - n * m * m
        GaussianGroupAggregate(n, m, s)
      }
  }

  implicit def eq[T: Eq]: Eq[GaussianGroupAggregate[T]] =
    Eq.instance((a, b) => a.n == b.n && a.m === b.m && a.s === b.s)
}
