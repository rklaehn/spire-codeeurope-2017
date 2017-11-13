package aggregation

import spire.algebra.Field
import spire.sp
import spire.syntax.field._

// https://www.johndcook.com/blog/standard_deviation/
class Welford[@sp T](var n: Int, var m: T, var s: T) {

  def mean = m

  def variance(implicit f: Field[T]): T = {
    val s = this.s
    val n = this.n
    if(n > 1) s / n else Field[T].zero
  }

  def sampleVariance(implicit f: Field[T]): T = {
    val s = this.s
    val n = this.n
    if(n > 1) s / (n - 1) else Field[T].zero
  }

  def add(x: T)(implicit f: Field[T]): Unit = {
    n += 1
    if (n == 1) {
      m = x
      s = Field[T].zero
    } else {
      val m0 = this.m
      val s0 = this.s
      m = m0 + (x - m0) / n
      s = s0 + (x - m0) * (x - m)
    }
  }
}

object Welford {

  def variance[@sp T: Field](values: Seq[T]): (Int, T, T) = {
    val agg = Welford.create[T]()
    for(value <- values) {
      agg.add(value)
    }
    return (agg.n, agg.mean, agg.variance)
  }

  def create[@sp T: Field](): Welford[T] =
    new Welford(0, Field.zero[T], Field.zero[T])

}
