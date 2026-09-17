/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import cats.collections.{Diet, Range}

import parsley.{Success, Failure}

class ParserTests extends AnyFlatSpec {
  import parser.cls
  import Regex.*
  import QuantifierType.*

  "regex classes" should "parse sets of literals" in {
    cls.parse("[abc]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'c'.toInt)))
    )
    cls.parse("[.]") shouldBe Success(Class(Diet.one('.'.toInt)))
    cls.parse("[^abc]") shouldBe Success(
      Class(
        Diet.fromRange(Range(0, '`'.toInt)).addRange(Range('d'.toInt, 0x1ffff))
      )
    )
    cls.parse("[0^]") shouldBe Success(
      Class(Diet.one('0'.toInt).add('^'.toInt))
    )
    cls.parse("[&]") shouldBe Success(Class(Diet.one('&'.toInt)))
  }

  they should "reject empty sets" in {
    cls.parse("[]") shouldBe a[Failure[?]]
  }

  they should "not treat partial ranges as errors" in {
    cls.parse("[-]") shouldBe Success(Class(Diet.one('-'.toInt)))
    cls.parse("[a-]") shouldBe Success(
      Class(Diet.one('a'.toInt).add('-'.toInt))
    )
    cls.parse("[-a]") shouldBe Success(
      Class(Diet.one('-'.toInt).add('a'.toInt))
    )
  }

  they should "accept ranges otherwise" in {
    cls.parse("[a-c]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'c'.toInt)))
    )
    cls.parse("[!--]") shouldBe Success(
      Class(Diet.fromRange(Range('!'.toInt, '-'.toInt)))
    )
    cls.parse("[\\0141-\\0172]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt)))
    )
  }

  they should "reject ill-formed ranges" in {
    cls.parse("[c-a]") shouldBe a[Failure[?]]
  }

  they should "allow for set unions" in {
    cls.parse("[[a-c][d-f]]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'f'.toInt)))
    )
    cls.parse("[a-z[0-9]]") shouldBe Success(
      Class(
        Diet
          .fromRange(Range('a'.toInt, 'z'.toInt))
          .addRange(Range('0'.toInt, '9'.toInt))
      )
    )
    cls.parse("[a-z0-9]") shouldBe Success(
      Class(
        Diet
          .fromRange(Range('a'.toInt, 'z'.toInt))
          .addRange(Range('0'.toInt, '9'.toInt))
      )
    )
  }

  they should "allow for set intersections" in {
    cls.parse("[&&a]") shouldBe Success(Class(Diet.one('a'.toInt)))
    cls.parse("[a&&]") shouldBe Success(Class(Diet.one('a'.toInt)))
    cls.parse("[a-z&&0-9]") shouldBe Success(Class(Diet.empty))
    cls.parse("[a-z&&a-z]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt)))
    )
    cls.parse("[a-z&&&&&&0-9]") shouldBe Success(Class(Diet.empty))
    cls.parse("[a-z&&&&&&a-z]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt)))
    )
  }

  they should "reject empty intersections" in {
    cls.parse("[&&]") shouldBe a[Failure[?]]
  }

  // this is here for documentation, I expect may of these may not be fixed
  they should "match the weird behaviour with Java" ignore {
    cls.parse("[a&&&]") shouldBe Success(
      Class(Diet.one('&'.toInt).add('a'.toInt))
    )
  }

  they should "accept Kleene stars" in {
    parser.parse("[a]*") shouldBe Success(
      Star(Class(Diet.one('a'.toInt)), Greedy)
    )
    parser.parse("[a-z]*") shouldBe Success(
      Star(Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt))), Greedy)
    )
  }

  they should "handle Groups correctly" in {
    parser.parse("(a)") shouldBe Success(
      Capture(Lit('a'.toInt))
    )
    parser.parse("(a|b)") shouldBe Success(
      Capture(Alt(Lit('a'.toInt), Lit('b'.toInt)))
    )
    parser.parse("(a|b)*") shouldBe Success(
      Star(Capture(Alt(Lit('a'.toInt), Lit('b'.toInt))), Greedy)
    )
    parser.parse("((a|b)*)") shouldBe Success(
      Capture(Star(Capture(Alt(Lit('a'.toInt), Lit('b'.toInt))), Greedy))
    )
  }

  they should "handle predefined classes correctly" in {
    parser.parse("\\d") shouldBe Success(
      Class(Diet.fromRange(Range('0'.toInt, '9'.toInt)))
    )
    parser.parse("\\D") shouldBe Success(
      Class(Regex.AllSet -- Diet.fromRange(Range('0'.toInt, '9'.toInt)))
    )
    parser.parse("\\w") shouldBe Success(
      Class(
        Diet.fromRange(Range('a'.toInt, 'z'.toInt))
        | Diet.fromRange(Range('A'.toInt, 'Z'.toInt))
        | Diet.fromRange(Range('0'.toInt, '9'.toInt))
        | Diet.one('_'.toInt)
      )
    )
    parser.parse("\\W") shouldBe Success(
      Class(
        Regex.AllSet -- (Diet.fromRange(Range('a'.toInt, 'z'.toInt))
        | Diet.fromRange(Range('A'.toInt, 'Z'.toInt))
        | Diet.fromRange(Range('0'.toInt, '9'.toInt))
        | Diet.one('_'.toInt))
      )
    )
    parser.parse("\\s") shouldBe Success(
      Class(
        Diet.one(' '.toInt)
        | Diet.one('\t'.toInt)
        | Diet.one('\n'.toInt)
        | Diet.one('\u000B'.toInt)
        | Diet.one('\r'.toInt)
        | Diet.one('\f'.toInt)
      )
    )
    parser.parse("\\S") shouldBe Success(
      Class(
        Regex.AllSet -- ( Diet.one(' '.toInt)
                        | Diet.one('\t'.toInt)
                        | Diet.one('\n'.toInt)
                        | Diet.one('\u000B'.toInt)
                        | Diet.one('\r'.toInt)
                        | Diet.one('\f'.toInt)
        )
      )
    )
  }

  // TODO: more tests when escape sequences are implemented
}
