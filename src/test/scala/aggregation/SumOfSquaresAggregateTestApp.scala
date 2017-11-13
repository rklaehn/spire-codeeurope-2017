package aggregation
import spire.implicits._

object SumOfSquaresAggregateTestApp extends App {

  val values = (1 to 5).map(_.toDouble)
  val aggregates = values.map(SumOfSquaresAggregate.apply[Double])
  val total = aggregates.reduce(_ |+| _)

  println(total.standardDeviation)
  println(total.sampleStandardDeviation)

}
