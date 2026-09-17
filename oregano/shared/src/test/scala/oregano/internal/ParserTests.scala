/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import oregano.internal.ast.{AST, Greedy}
import oregano.internal.parsing.bridges.allSet
import oregano.internal.parsing.parser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import cats.collections.{Diet, Range}

import parsley.{Success, Failure}

class ParserTests extends AnyFlatSpec {
  given AST = Oregano
  import Oregano.*

  "regex classes" should "parse sets of literals" in {
    parser.parseClass("[abc]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'c'.toInt)))
    )
    parser.parseClass("[.]") shouldBe Success(Class(Diet.one('.'.toInt)))
    parser.parseClass("[^abc]") shouldBe Success(
      Class(
        Diet.fromRange(Range(0, '`'.toInt)).addRange(Range('d'.toInt, 0x1ffff))
      )
    )
    parser.parseClass("[0^]") shouldBe Success(
      Class(Diet.one('0'.toInt).add('^'.toInt))
    )
    parser.parseClass("[&]") shouldBe Success(Class(Diet.one('&'.toInt)))
  }

  they should "reject empty sets" in {
    parser.parseClass("[]") shouldBe a[Failure[?]]
  }

  they should "not treat partial ranges as errors" in {
    parser.parseClass("[-]") shouldBe Success(Class(Diet.one('-'.toInt)))
    parser.parseClass("[a-]") shouldBe Success(
      Class(Diet.one('a'.toInt).add('-'.toInt))
    )
    parser.parseClass("[-a]") shouldBe Success(
      Class(Diet.one('-'.toInt).add('a'.toInt))
    )
  }

  they should "accept ranges otherwise" in {
    parser.parseClass("[a-c]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'c'.toInt)))
    )
    parser.parseClass("[!--]") shouldBe Success(
      Class(Diet.fromRange(Range('!'.toInt, '-'.toInt)))
    )
    parser.parseClass("[\\0141-\\0172]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt)))
    )
  }

  they should "reject ill-formed ranges" in {
    parser.parseClass("[c-a]") shouldBe a[Failure[?]]
  }

  they should "allow for set unions" in {
    parser.parseClass("[[a-c][d-f]]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'f'.toInt)))
    )
    parser.parseClass("[a-z[0-9]]") shouldBe Success(
      Class(
        Diet
          .fromRange(Range('a'.toInt, 'z'.toInt))
          .addRange(Range('0'.toInt, '9'.toInt))
      )
    )
    parser.parseClass("[a-z0-9]") shouldBe Success(
      Class(
        Diet
          .fromRange(Range('a'.toInt, 'z'.toInt))
          .addRange(Range('0'.toInt, '9'.toInt))
      )
    )
  }

  they should "allow for set intersections" in {
    parser.parseClass("[&&a]") shouldBe Success(Class(Diet.one('a'.toInt)))
    parser.parseClass("[a&&]") shouldBe Success(Class(Diet.one('a'.toInt)))
    parser.parseClass("[a-z&&0-9]") shouldBe Success(Class(Diet.empty))
    parser.parseClass("[a-z&&a-z]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt)))
    )
    parser.parseClass("[a-z&&&&&&0-9]") shouldBe Success(Class(Diet.empty))
    parser.parseClass("[a-z&&&&&&a-z]") shouldBe Success(
      Class(Diet.fromRange(Range('a'.toInt, 'z'.toInt)))
    )
  }

  they should "reject empty intersections" in {
    parser.parseClass("[&&]") shouldBe a[Failure[?]]
  }

  // this is here for documentation, I expect may of these may not be fixed
  they should "match the weird behaviour with Java" ignore {
    parser.parseClass("[a&&&]") shouldBe Success(
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
      Class(allSet -- Diet.fromRange(Range('0'.toInt, '9'.toInt)))
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
        allSet -- (Diet.fromRange(Range('a'.toInt, 'z'.toInt))
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
        allSet -- ( Diet.one(' '.toInt)
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
