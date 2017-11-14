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

  implicit def group[T: Field: Eq]: Monoid[GaussianGroupAggregate[T]] = new Monoid[GaussianGroupAggregate[T]] {

    override def empty = GaussianGroupAggregate.empty[T]

    override def combine(a: GaussianGroupAggregate[T], b: GaussianGroupAggregate[T]) =
      if (a.n > b.n) combine0(a, b) else combine0(b, a)

    // this assumes that a.n >= b.n
    private def combine0(a: GaussianGroupAggregate[T], b: GaussianGroupAggregate[T]) =
      if (b.n == 0) a else {
        val u = a.m
        val x = a.s
        val i = a.n

        val v = b.m
        val y = b.s
        val j = b.n

        val n: Int = i + j

        // m = m0 + (x - m0) / n
        val m: T = u + (v - u) * j / n // this now degenerates to the welford method for b.n == 1

        // s = s0 + (x - m0) * (x - m)
        // a.m == m0 == u
        // a.s == s0 == x
        // a.n       == i
        // b.n == 1  == j
        // b.s == 0  == y
        // b.m == x  == v
        // as + bs + an * am * am + bn * bm * bm - n * m * m
        // x + y + i * u * u + j * v * v - n * m * m
        // val s: T = x + y + i * u * u + j * v * v - (i + j) * (u + (v - u) * j / (i + j)) * (u + (v - u) * j / (i + j))

        // val s: T = x +         y + i * u * u + j * v * v - (i + j) * (u + (v - u) * j / (i + j)) * (u + (v - u) * j / (i + j))

        // val s: T = x +         y + i * u * u + j * v * v - (i + j) * (u + (v - u) * j / (i + j)) * (u + (v - u) * j / (i + j))

        // this one looks pretty good!
        // val f = - (i * j * (u - v) * (u - v) + (j + i) * y) / ((2 * j + i) * (u - v))
        // val s: T = x + f * (v - u + (v - u) * j / (i + j))

        // val f = (y + j * (u * u - 2 * u * v + v * v - i * y)) / ((v - u) * (v - u))
        val vu = v - u
        val s0 = x + y + i * u * u + j * v * v - n * m * m
        val s = if(!vu.isZero) {
          val f = j + (1 - i * j) * y / (vu * vu)
          x + f * (v - u) * (v - m)
        } else {
          s0
        }
        if(!(s -s0).isZero) {
          println(s)
          println(s0)
        }

        GaussianGroupAggregate(n, m, s)
      }
  }

  implicit def eq[T: Eq]: Eq[GaussianGroupAggregate[T]] =
    Eq.instance((a, b) => a.n == b.n && a.m === b.m && a.s === b.s)
}
