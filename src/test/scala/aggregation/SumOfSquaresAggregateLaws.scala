package aggregation

import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.laws.GroupLaws
import spire.math.Rational

class SumOfSquaresAggregateLaws extends FunSuite with Discipline {

  implicit def arbSumOfSquaresAggregate[T: Arbitrary]: Arbitrary[SumOfSquaresAggregate[T]] = ???

  // checkAll("", thelaws)
}
