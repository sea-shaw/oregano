package oregano

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

class UnapplyTests extends AnyFlatSpec {
  it should "match zero capture groups" in {
    val r = r"a"
    "a" should matchPattern { case r(()) => } // TODO: Make this look nicer
  }

  it should "match one capture group" in {
    val r = r"(a)"
    "a" should matchPattern { case r("a") => }
  }

  it should "match multiple capture groups" in {
    val r = r"(a)(b)(c)"
    "abc" should matchPattern { case r("a", "b", "c") => }
  }

  it should "match nested capture groups" in {
    val r = r"(a(b(c)d)e)"
    "abcde" should matchPattern { case r("abcde", "bcd", "c") => }
  }

  it should "match optional patterns" in {
    val r = r"a?"
    "a" should matchPattern { case r(()) => }
    "" should matchPattern { case r(()) => }
  }

  it should "match optional capture groups" in {
    val r = r"(a)?"
    "a" should matchPattern { case r(Some("a")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match nested optional capture groups" in {
    val r = r"(a(b)?)?"
    "" should matchPattern { case r(None) => }
    "a" should matchPattern { case r(Some("a", None)) => }
    "ab" should matchPattern { case r(Some("ab", Some("b"))) => }
  }

  it should "match optional capture group inside optional non-capture group" in {
    val r = r"(?:(a)?b)?"
    "" should matchPattern { case r(None) => }
    "b" should matchPattern { case r(None) => }
    "ab" should matchPattern { case r(Some("a")) => }
  }

  it should "match star capture groups" in {
    val r = r"(a)*"
    "aaaa" should matchPattern { case r(Some("a")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match alternative patterns" in {
    val r = r"a|b"
     "a" should matchPattern { case r(()) => }
     "b" should matchPattern { case r(()) => }
  }

  it should "match alternative capture groups" in {
    val r = r"(a)|(b)"
    "a" should matchPattern { case r(Left("a")) => }
    "b" should matchPattern { case r(Right("b")) => }
  }

  it should "match alternative patterns with capture groups on one side" in {
    val r = r"(a)|b"
    "a" should matchPattern { case r(Some("a")) => }
    "b" should matchPattern { case r(None) => }
  }

  it should "match alternatives with multiple capture groups on either side" in {
    val r = r"(a)(b)|(c)(d)"
    "ab" should matchPattern { case r(Left("a", "b")) => }
    "cd" should matchPattern { case r(Right("c", "d")) => }
  }

  it should "match many chained alternative capture groups" in {
    val r = r"(a)|(b)|(c)|(d)"
    "a" should matchPattern { case r(Left("a")) => }
    "b" should matchPattern { case r(Right(Left("b"))) => }
    "c" should matchPattern { case r(Right(Right(Left("c")))) => }
    "d" should matchPattern { case r(Right(Right(Right("d")))) => }
  }

  it should "match alternative with optional capture group on the left" in {
    val r = r"(a)?|b"
    "a" should matchPattern { case r(Some("a")) => }
    "b" should matchPattern { case r(None) => }
  }

  it should "match alternative with optional capture group on the right" in {
    val r = r"a|(b)?"
    "a" should matchPattern { case r(None) => }
    "b" should matchPattern { case r(Some("b")) => }
  }

  it should "match alternative with optional capture groups on both sides" in {
    val r = r"(a)?|(b)?"
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(Some(Right("b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match alternative with optional capture group on the left and non-optional on the right" in {
    val r = r"(a)?|(b)"
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(Some(Right("b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match alternative with non-optional capture group on the left and optional on the right" in {
    val r = r"(a)|(b)?"
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(Some(Right("b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match 4-way alternative with no capture groups in the middle" in {
    val r = r"(a)|b|c|(d)"
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(None) => }
    "c" should matchPattern { case r(None) => }
    "d" should matchPattern { case r(Some(Right("d"))) => }
  }

  it should "allow non-capturing groups" in {
    val r = r"(?:a)"
    "a" should matchPattern { case r(()) => }
  }

  it should "match capture groups with shared optionality" in {
    val r = r"(?:(a)(b))?"
    "ab" should matchPattern { case r(Some("a", "b")) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match alternative capture groups inside optional" in {
    val r = r"(?:(a)|(b))?"
    "a" should matchPattern { case r(Some(Left("a"))) => }
    "b" should matchPattern { case r(Some(Right("b"))) => }
    "" should matchPattern { case r(None) => }
  }

  it should "match nested alternative capture groups" in {
    val r = r"(?:(a)|(b))|(?:(c)|(d))"
    "a" should matchPattern { case r(Left(Left("a"))) => }
    "b" should matchPattern { case r(Left(Right("b"))) => }
    "c" should matchPattern { case r(Right(Left("c"))) => }
    "d" should matchPattern { case r(Right(Right("d"))) => }
  }

  it should "match one or more alternatives" in {
    val r = r"(?:(a)|(b))+"
    "a" should matchPattern { case r(Left(Left("a"))) => }
    "b" should matchPattern { case r(Left(Right("b"))) => }
    "ba" should matchPattern { case r(Right("a", "b")) => }
    "ab" should matchPattern { case r(Right("a", "b")) => }
  }

  it should "match zero or more alternatives" in {
    val r = r"(?:(a)|(b))*"
    "a" should matchPattern { case r(Some(Left(Left("a")))) => }
    "b" should matchPattern { case r(Some(Left(Right("b")))) => }
    "ba" should matchPattern { case r(Some(Right("a", "b"))) => }
    "ab" should matchPattern { case r(Some(Right("a", "b"))) => }
    "" should matchPattern { case r(None) => }
  }
}
