/*
 * Copyright 2024 Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package oregano

import scala.quoted.*

abstract class Regex[Match] {
    def matches(input: CharSequence): Boolean
    def matchesWithCaps(input: CharSequence): Option[Array[Int]]
    //def matchesLinear(input: CharSequence): Boolean
    def findPrefixOf(source: CharSequence): Option[String]
    def findFirstIn(source: CharSequence): Option[String]
    def split(toSplit: CharSequence): Array[String]
    def unapplySeq(input: CharSequence): Option[List[String]]
}

object Regex {
    // Fallback method for runtime regexes
    def runtime(s: String): Regex[List[String]] = new Regex[List[String]] {
        private val compiled = s.r
        //private val patternResult = internal.Pattern.compile(s)
        //private val pattern = patternResult.pattern
        //private val numGroups = patternResult.groupCount
        //private val prog = internal.ProgramCompiler.compileRegexp(pattern, numGroups)
        //private val re2Machine = internal.RE2Machine(prog)
        def matches(input: CharSequence): Boolean = compiled.matches(input)
        def matchesWithCaps(input: CharSequence): Option[Array[Int]] = ???
        //def matchesLinear(input: CharSequence): Boolean = re2Machine.matches(input)
        def findPrefixOf(source: CharSequence): Option[String] = compiled.findPrefixOf(source)
        def findFirstIn(source: CharSequence): Option[String] = compiled.findFirstIn(source)
        def split(toSplit: CharSequence): Array[String] = compiled.split(toSplit)
        def unapplySeq(input: CharSequence): Option[List[String]] = compiled.unapplySeq(input).map(_.collect { case s: String => s })
    }
}

extension (inline sc: StringContext) {
    transparent inline def r(): Regex[?] = ${ isInlineable('sc) }
}

private def isInlineable(regExpr: Expr[StringContext])(using Quotes): Expr[Regex[?]] = {
    import quotes.reflect.{Position, report}
    regExpr match {
        // use the macro, inlineable
        case '{ StringContext.apply( ${ Expr(s) } ) } => internal.compileMacro(s)
        // fallback to runtime compilation
        // case _ => '{ Regex.runtime($regExpr) }
        case _ => report.errorAndAbort("Regex string must be a compile-time constant", Position.ofMacroExpansion)
    }
}

// FIXME: wrong type, not sure how I want to process the typesafe bit yet, ideally avoid duplication, but might have to :(
//transparent inline def regex/*[S <: String & Singleton]*/(inline regex: String): Regex[?] = ${compileMacro('regex)}

// this, annoyingly, has to be here or else the splice above complains that it's in a different scope
private def compileMacro(s: Expr[String])(using Quotes): Expr[Regex[?]] =
  internal.compileMacro(s.valueOrAbort)
