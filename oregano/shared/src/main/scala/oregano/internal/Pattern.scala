/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

//import scala.quoted.*
//import cats.collections.{Diet, Range}
import cats.collections.Diet
import oregano.internal.ast.{AST, Greedy}
import oregano.internal.parsing.parser

enum Pattern {
    case Lit(c: Int)
    case Cat(left: Pattern, right: Pattern)
    case Alt(left: Pattern, right: Pattern)
    case Class(diet: Diet[Int])
    case Rep0(pat: Pattern, idx: Int)
    case Capture(groupIdx: Int, pat: Pattern)

    // UNUSED
    // def optimize: Pattern = this match {
    //     case Cat(ps) => Cat {
    //         ps.flatMap {
    //             case Cat(subs) => subs.map(_.optimize)
    //             case p         => List(p.optimize)
    //         }
    //     }
    //     case Alt(p1, p2) => Alt(p1.optimize, p2.optimize)
    //     case _ => this
    // }
}
object Pattern {
    /*
    given ToExpr[Pattern] {
        def apply(pat: Pattern)(using Quotes): Expr[Pattern] = pat match {
            case Pattern.Lit(c)                 => '{ Pattern.Lit(${ Expr(c) }) }
            case Pattern.Cat(patterns)          => '{ Pattern.Cat(${ Expr(patterns) }) }
            case Pattern.Alt(left, right)       => '{ Pattern.Alt(${ Expr(left) }, ${ Expr(right) }) }
            case Pattern.Class(diet)            => '{ Pattern.Class(${ Expr(diet) }) }
            case Pattern.Rep0(pat, idx)         => '{ Pattern.Rep0(${ Expr(pat) }, ${ Expr(idx) }) }
            case Pattern.Capture(groupIdx, pat) => '{ Pattern.Capture(${ Expr(groupIdx) }, ${ Expr(pat) }) }
        }

    private given ToExpr[Range[Int]] {
        def apply(rng: Range[Int])(using Quotes): Expr[Range[Int]] = '{ Range(${ Expr(rng.start) }, ${ Expr(rng.end) }) }
    }
    private given ToExpr[Diet[Int]] {
        def apply(diet: Diet[Int])(using Quotes): Expr[Diet[Int]] = {
            diet.toIterator.foldLeft('{ Diet.empty[Int] }) { (acc, rng) => '{ $acc.addRange(${Expr(rng)}) }}
        }
    }
    */

    // UNUSED
    // def lit(c: Int): Pattern = Pattern.Lit(c)
    // def concat(ps: Pattern*): Pattern = Pattern.Cat(ps.toList)
    // def alt(p1: Pattern, p2: Pattern): Pattern = Pattern.Alt(p1, p2)
    // def charClass(diet: Diet[Int]): Pattern = Pattern.Class(diet)
    // def rep0(pat: Pattern): Pattern = Pattern.Rep0(pat, 0) // idx is not used here

    def compile(using ast: AST)(regex: ast.Regex[?]): PatternResult = {
        val pat = new PatternBuilder()
        pat.build(regex)
    }

    def checkForNestedLoop(pat: Pattern, seenLoop: Boolean = false): Boolean = pat match {
        case Pattern.Rep0(_, _) if seenLoop => true
        case Pattern.Rep0(p, _)             => checkForNestedLoop(p, true)
        case Pattern.Alt(left, right)       => checkForNestedLoop(left, seenLoop) || checkForNestedLoop(right, seenLoop)
        case Pattern.Cat(left, right)       => checkForNestedLoop(left, seenLoop) || checkForNestedLoop(right, seenLoop)
        case Pattern.Capture(_, p)          => checkForNestedLoop(p, seenLoop)
        case _                              => false
    }

    // for now, protect against nested loops
    def checkFlatControlFlow(pat: Pattern): Boolean = !checkForNestedLoop(pat)

    def compile(regex: String)(using AST): PatternResult = {
        val re = parser.parse(regex).getOrElse(throw IllegalArgumentException(s"Invalid regex: $regex"))
        compile(re)
    }
}

final case class PatternResult(pattern: Pattern, groupCount: Int, flatControlFlow: Boolean, numReps: Int)

class PatternBuilder {
    var nextGroup: Int = 1 // note that 1 is reserved for the whole match, as with other engines
    var numReps = 0 // initially used for caching nested Rep0 loops safely: TODO: doesn't work and isn't neccessary, delete!

    def compile(using ast: AST)(regex: ast.Regex[?]): Pattern = regex match {
        case ast.Lit(c)           => Pattern.Lit(c)
        case ast.Cat(left, right) => Pattern.Cat(compile(left), compile(right))
        case ast.Alt(r1, r2)      => Pattern.Alt(compile(r1), compile(r2))
        case ast.Class(d)         => Pattern.Class(d)
        case ast.Star(r, Greedy)  => {
            val p = compile(r)
            val idx = numReps
            numReps += 1
            Pattern.Rep0(p, idx)
        }
        // Given we use a shared `p`, capture indicies are propogated safely so I believe this to be safe
        // That being said, not doing this could yield a more terse Prog, but I don't have time
        // I'd expect the more terse Prog to be more performant
        case ast.Plus(r, Greedy) => {
            val p = compile(r)
            val idx = numReps
            numReps += 1
            Pattern.Cat(p, Pattern.Rep0(p, idx))
        }
        case ast.Capture(r) => {
            val groupId = nextGroup
            nextGroup += 1
            val p = compile(r)
            Pattern.Capture(groupId, p)
        }
        case ast.NonCapture(flagsOn, flagsOff, r) if flagsOn.isEmpty && flagsOff.isEmpty => compile(r)

        // Dot matches any character except newline, there is a flag to change this, could be handled
        // Could keep a Pattern.Dot, but for now, we can use a class that matches all characters except newline as is default
        case _ => throw IllegalArgumentException(s"Unsupported regex: $regex")
    }

    def build(using ast: AST)(regex: ast.Regex[?]): PatternResult = {
        val pattern = compile(regex)
        val groupCount = nextGroup
        val flatControlFlow = Pattern.checkFlatControlFlow(pattern)
        val numReps = this.numReps + 1
        PatternResult(pattern, groupCount, flatControlFlow, numReps)
    }
}
