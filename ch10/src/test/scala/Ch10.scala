import org.scalatest._

trait Monoid[A] {
  def op(a1: A, a2: A) : A
  def zero : A
}

class TestChapter10 extends FlatSpec with Matchers {

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }


  val optionMonoid = new Monoid[Option[Int]] {
    def op(a1: Option[Int], a2: Option[Int]) = a1 orElse a2
    val zero = None
  }

  val endoMonoid = new Monoid[Int => Int] {
    def op(a1: Int => Int, a2: Int => Int) = a1 compose a2
    val zero = (a: Int) => a
  }

  // Exercise 10.1
  it should "test Monoid" in {
    intAddition.op(1, intAddition.op(2, 3)) should
      be (intAddition.op(intAddition.op(1, 2), 3))
    intAddition.op(intAddition.zero, 1) should
      be (intAddition.op(1, intAddition.zero))

    intAddition.op(1, intAddition.op(2, 3)) should
      be (intAddition.op(intAddition.op(1, 2), 3))
    intAddition.op(intAddition.zero, 1) should
      be (intAddition.op(1, intAddition.zero))

    booleanOr.op(false, booleanOr.op(true, false)) should
      be (booleanOr.op(false, booleanOr.op(true, false)))
    booleanOr.op(booleanOr.zero, false)
    booleanOr.op(false, booleanOr.zero)
    booleanOr.op(booleanOr.zero, true)
    booleanOr.op(true, booleanOr.zero)

    booleanAnd.op(false, booleanAnd.op(true, false)) should
      be (booleanAnd.op(false, booleanAnd.op(true, false)))
    booleanAnd.op(booleanAnd.zero, false) should 
      be (booleanAnd.op(false, booleanAnd.zero))
    booleanAnd.op(booleanAnd.zero, true) should
      be (booleanAnd.op(true, booleanAnd.zero))
  }

  // Exercise 10.2
  it should "option Monoid" in {

    val monoid = optionMonoid

    monoid.op(Some(1), monoid.op(Some(2), Some(3))) should
      be (monoid.op(Some(1), monoid.op(Some(2), Some(3))))
    monoid.op(monoid.zero, Some(1)) should
      be (monoid.op(Some(1), monoid.zero))
  }

  // Exercise 10.3
  it should "endo Monoid" in {

    val monoid = endoMonoid
    def f1 = (a: Int) => a * 10
    def f2 = (a: Int) => a + 10
    def f3 = (a: Int) => a * 20 + 2

    monoid.op(f1, monoid.op(f2, f3))(10) should
      be (monoid.op(f1, monoid.op(f2, f3))(10))
    monoid.op(monoid.zero, f1)(10) should
      be (monoid.op(f1, monoid.zero)(10))
  }

}
