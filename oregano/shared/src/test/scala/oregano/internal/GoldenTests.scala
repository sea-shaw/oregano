package oregano.internal

import oregano.internal.code
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.golden.GoldenMatchers
import org.scalatest.matchers.should.Matchers.should

class GoldenTests extends AnyFlatSpec with GoldenMatchers {
    val dir = "oregano/shared/src/test/resources"

    behavior of "Catnip regex macro"

    it should "match zero capture groups" in {
        code("a") should matchGolden(s"$dir/zero-capture-groups.golden")
    }

    it should "match one capture group" in {
        code("(a)") should matchGolden(s"$dir/one-capture-group.golden")
    }

    it should "match multiple capture groups" in {
        code("(a)(b)(c)") should matchGolden(s"$dir/multiple-capture-groups.golden")
    }

    it should "match nested capture groups" in {
        code("(a(b(c)d)e)") should matchGolden(s"$dir/nested-capture-groups.golden")
    }

    it should "match optional patterns" in {
        code("a?") should matchGolden(s"$dir/optional-patterns.golden")
    }

    it should "match optional capture groups" in {
        code("(a)?") should matchGolden(s"$dir/optional-capture-groups.golden")
    }

    it should "match nested optional capture groups" in {
        code("(a(b)?)?") should matchGolden(s"$dir/nested-optional-capture-groups.golden")
    }

    it should "match optional capture group inside optional non-capture group" in {
        code("(?:(a)?b)?") should matchGolden(s"$dir/option-inside-option.golden")
    }

    it should "match star capture groups" in {
        code("(a)*") should matchGolden(s"$dir/star-capture-groups.golden")
    }

    it should "match alternative patterns" in {
        code("a|b") should matchGolden(s"$dir/alternative-patterns.golden")
    }

    it should "match alternative capture groups" in {
        code("(a)|(b)") should matchGolden(s"$dir/alternative-capture-groups.golden")
    }

    it should "match alternative patterns with capture groups on one side" in {
        code("(a)|b") should matchGolden(s"$dir/alternative-patterns-with-capture-groups-on-one-side.golden")
    }

    it should "match alternatives with multiple capture groups on either side" in {
        code("(a)(b)|(c)(d)") should matchGolden(s"$dir/alternatives-with-captures-on-either-side.golden")
    }

    it should "match many chained alternative capture groups" in {
        code("(a)|(b)|(c)|(d)") should matchGolden(s"$dir/many-chained-alternative-capture-groups.golden")
    }

    it should "match alternative with optional capture group on the left" in {
        code("(a)?|b") should matchGolden(s"$dir/alt-option-left.golden")
    }

    it should "match alternative with optional capture group on the right" in {
        code("(a)?|b") should matchGolden(s"$dir/alt-option-right.golden")
    }

    it should "match non-capturing groups" in {
        code("(?:a)") should matchGolden(s"$dir/non-capturing-groups.golden")
    }

    it should "match capture groups with shared optionality" in {
        code("(?:(a)(b))?") should matchGolden(s"$dir/capture-groups-with-shared-optionality.golden")
    }

    it should "match optional capture groups inside alternative" in {
        code("(a)?|(b)?") should matchGolden(s"$dir/optional-capture-groups-inside-alternative.golden")
    }

    it should "match alternative capture groups inside optional" in {
        code("(?:(a)|(b))?") should matchGolden(s"$dir/alternative-capture-groups-inside-optional.golden")
    }

    it should "match nested alternative capture groups" in {
        code("(?:(a)|(b))|(?:(c)|(d))") should matchGolden(s"$dir/nested-alternative-capture-groups.golden")
    }

    it should "match one or more alternatives" in {
        code("((a)|(b))+") should matchGolden(s"$dir/one-or-more-alternatives.golden")
    }

    it should "match zero or more alternatives" in {
        code("((a)|(b))*") should matchGolden(s"$dir/zero-or-more-alternatives.golden")
    }
}
