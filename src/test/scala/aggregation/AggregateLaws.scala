package aggregation
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.algebra.{Field, Order, Signed}
import spire.laws.GroupLaws
import spire.math.{Rational, UByte}
import spire.laws.arb._

class AggregateLaws extends FunSuite with Discipline {

  implicit def arbSumOfSquaresAggregate[T: Arbitrary]: Arbitrary[SumOfSquaresAggregate[T]] = Arbitrary(for {
    n <- arbitrary[UByte]
    sum <- arbitrary[T]
    sos <- arbitrary[T]
  } yield SumOfSquaresAggregate(n.toInt, sum, sos))

  implicit def gaussianGroupAggregate[T: Arbitrary: Field: Order: Signed]: Arbitrary[GaussianGroupAggregate[T]] = Arbitrary(for {
    n <- arbitrary[UByte]
    m <- arbitrary[T]
    s <- arbitrary[T]
  } yield {
    if (n.toInt == 0)
      GaussianGroupAggregate.empty[T]
    else
      GaussianGroupAggregate(n.toInt, m, spire.math.abs(s))
  })

  checkAll("GroupLaws[SumOfSquaresAggregate[Rational]].group", GroupLaws[SumOfSquaresAggregate[Rational]].group)
  checkAll("GroupLaws[GaussianGroupAggregate[Rational]].monoid", GroupLaws[GaussianGroupAggregate[Rational]].monoid)

  // import instances for Double
  // import spire.implicits.DoubleAlgebra
  // checkAll("GroupLaws[SumOfSquaresAggregate[Double]].group", GroupLaws[SumOfSquaresAggregate[Double]].group)
  // checkAll("GroupLaws[GaussianGroupAggregate[Double]].monoid", GroupLaws[GaussianGroupAggregate[Double]].monoid)
}
