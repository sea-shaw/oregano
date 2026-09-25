/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano.internal

import oregano.internal.ast.{AST, Captures, Rep, RepFalse}
import oregano.internal.hchain.HChain
import oregano.internal.parsing.parser
import parsley.{Success, Failure}
import scala.quoted.*

private [oregano] def compileMacro(s: String)(using Quotes): Expr[oregano.Regex[?]] = {
    import quotes.reflect.report
    given AST = Oregano
    parser.parse(s) match
        case Success(ast) => {
            // report.info(s"$ast")
            // val patternResult = Pattern.compile(ast)
            regexCode(ast)
        }
        case Failure(err) => report.errorAndAbort(err)
}

/* Returns a string containing a repesentation of the generated code. Used for
   golden testing. */
private inline def code(inline regex: String): String = ${ codeCode('regex) }
private def codeCode(strExpr: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect.{Position, Printer, asTerm, report}
    strExpr match {
        case Expr(s) => {
            given AST = Oregano
            parser.parse(s) match {
                case Success(ast) => Expr(regexCode(ast).asTerm.show(using Printer.TreeShortCode))
                case Failure(err) => Expr(err)
            }
        }
        case _ => report.errorAndAbort("Regex string must be a compile-time constant", Position.ofMacroExpansion)
    }
}

private def regexCode[F[_ <: Rep] <: HChain](using ast: AST)(regex: ast.Regex[F])(using Quotes): Expr[oregano.Regex[?]] = {
    val PatternResult(p, groupCount, _ /* flatControlFlow */, _) = Pattern.compile(regex)
    // report.info(s"expr: $s\nParsley AST: ${ast.toString}\nPattern: ${patternResult.pattern}, groupCount: ${patternResult.groupCount}")
    lazy val prog = ProgramCompiler.compileRegexp(p, groupCount)
    // report.info(s"Prog:\n$prog")
    //val liftedProgExpr = Expr(prog)
    // println(s"Prog:\n$prog")

    // Default to CPS because opt doesn't work in backtracking prog.
    // TODO: Fix opt in backtracking prog.
    val flatControlFlow = false

    // backtracking matcher stuff
    val backtrackingMatcherWithCapsExpr: Expr[CharSequence => Option[Array[Int]]] =
        if flatControlFlow then BacktrackingProgMatcher.genMatcherWithCaps(prog) else CPSMatcher.genMatcherPatternWithCaps(p, groupCount)

    val backtrackingMatcherExpr: Expr[CharSequence => Boolean] =
        if flatControlFlow then BacktrackingProgMatcher.genMatcher(prog) else CPSMatcher.genMatcherPattern(p)

    val backtrackingPrefixFinderExpr: Expr[(Int, CharSequence) => Int] =
        if flatControlFlow then BacktrackingProgMatcher.genPrefixFind(prog) else CPSMatcher.genPrefixFinderPattern(p, groupCount)
    // useful for debugging:
    // val backtrackCPSMatcherExpr = CPSMatcher.genMatcherPattern(p)
    // val backtrackingCPSMatcherWithCaps = CPSMatcher.genMatcherPatternWithCaps(p, groupCount)
    // val backtrackProgMatcherExpr = BacktrackingProgMatcher.genMatcher(prog)
    // val backtrackProgMatcherWithCaps = BacktrackingProgMatcher.genMatcherWithCaps(prog)

    // Linear matching stuff (TODO: how do we want to do this in practice?)
    /*
    var linearMatcherExpr = StagedMachine.generateStepLoop(prog)
    val willBeLinearStaged = Utils.countNodes(linearMatcherExpr) < 4500 // heuristic
    if (!willBeLinearStaged) report.warning(s"Matcher for $s is too large, will not be linear staged")
        // set linearMatcherExpr to just use runtime
        linearMatcherExpr = '{ (m: RE2Machine, input: CharSequence) =>
        m.matches(input)
    }
    */

    // for testing:
    // val nodeCount = Utils.countNodes(backtrackingMatcherExpr)
    // println(s"regex: $s counted nodes: $nodeCount, numInst: ${prog.numInst}")
    // val nodeCountWithCaps = Utils.countNodes(backtrackProgMatcherWithCaps)
    // println(s"regex: $s counted nodes: $nodeCountWithCaps, numInst: ${prog.numInst}")
    given Type[F] = regex.nodeType.tpe
    regex.tidyFunction(using RepFalse) match {
        case tidy @ ast.TidyFunction(given Type[a]) => '{
            new oregano.Regex[a] {
                //val prog = $liftedProgExpr
                //val re2Machine = RE2Machine(prog)

                override def matches(input: CharSequence): Boolean = $backtrackingMatcherExpr(input)
                // def matches(input: CharSequence): Boolean = regex.matches(input)
                override def matchesWithCaps(input: CharSequence): Option[Array[Int]] = $backtrackingMatcherWithCapsExpr(input)
                /*def matchesLinear(input: CharSequence): Boolean = $linearMatcherExpr(re2Machine, input)*/
                override def findPrefixOf(source: CharSequence): Option[String] = {
                    val matchPos = $backtrackingPrefixFinderExpr(0, source)
                    Option.when(matchPos != -1) {
                        (source.subSequence(0, matchPos).toString)
                    }
                }
                // TODO: tailrec
                override def findFirstIn(source: CharSequence): Option[String] = {
                    val len = source.length
                    var pos = 0
                    while (pos < len) {
                        val matchPos = $backtrackingPrefixFinderExpr(pos, source)
                        if (matchPos != -1) return Some(source.subSequence(pos, matchPos).toString)
                        pos += 1
                    }
                    None
                }

                override def split(toSplit: CharSequence): Array[String] = {
                    val result = scala.collection.mutable.ArrayBuffer.empty[String]
                    val len = toSplit.length
                    var pos = 0
                    var lastEnd = 0

                    while (pos < len) {
                        val matchEnd = $backtrackingPrefixFinderExpr(pos, toSplit)
                        if (matchEnd != -1) {
                            result += toSplit.subSequence(lastEnd, pos).toString
                            lastEnd = matchEnd
                            pos = if matchEnd == pos then pos + 1 else matchEnd
                        }
                        else pos += 1
                    }

                    if (lastEnd != len) result += toSplit.subSequence(lastEnd, len).toString
                    result.toArray
                }

                override def unapply(input: CharSequence): Option[a] = {
                    matchesWithCaps(input).map { caps =>
                        val hchain = ${ regex.getCode(1)(using RepFalse)(using Captures('input, 'caps)) }
                        ${ tidy('hchain) }
                    }
                }
            }
        }
    }
}
