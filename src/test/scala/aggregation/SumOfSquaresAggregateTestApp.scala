package aggregation
import spire.implicits._
import spire.math.Rational

object SumOfSquaresAggregateTestApp extends App {

  def stats(values: Seq[Rational]): Unit = {
    val reference = values.map(SumOfSquaresAggregate.apply[Rational]).qcombine
    val sos = values.map(x => SumOfSquaresAggregate(x.toDouble)).qcombine
    val gga = values.map(x => GaussianGroupAggregate(x.toDouble)).qcombine
    val welford = Welford.variance(values.map(_.toDouble))

    println("Sum of squares")
    println(sos.variance)

    println("Gaussian group")
    println(gga.variance)

    println("Welford")
    println(welford)

    println("Reference")
    println(reference.variance.toDouble)
  }

  val max = 10
  val values = (0 to max).map(x => Rational(x) / max + 1000000)
  stats(values)
//
////  val values = (1 to 3).map(_.toDouble)
//  val aggregates = values.map(SumOfSquaresAggregate.apply[Double])
//  val total = aggregates.qcombine
//
//  println(total.mean)
//  println(total.variance)
//  println(total.sampleVariance)
//
//  val aggregates2 = values.map(GaussianGroupAggregate.apply[Double])
//  val total2 = aggregates2.qcombine
//
//  println(total2.mean)
//  println(total2.variance)
//  println(total2.sampleVariance)
}
