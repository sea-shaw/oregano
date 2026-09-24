package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait CapturingTypes { this: Tidy =>
    /* Type of a capturing node with inner type `F`. */
    protected sealed trait CapturingType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain] { this: NodeType[G] =>
        /* Outside of this scope, `CapturingType` is not a subtype of `NodeType` so
       use `asNodeType` to convert safely. */
        final val asNodeType: NodeType[G] & CapturingType[F, G] = this

        /* Construct an HChain from the capture and the captures of the inner node. */
        def sanitiseCode[R <: Rep: Type](capture: Expr[Option[String]], sanitisedInner: => Expr[F[R]])(using Quotes): SanitiseExpr[G[R]]
        def getCode[R <: Rep: Type](capture: Expr[HSingleton[String]], inner: => Expr[F[R]])(using Quotes): Expr[G[R]]
    }

    protected object CapturingType {
        /* Returns the correct `CapturingType` for the type of `inner`. Only
           possible because of flow typing for GADTs. */
        def apply[F[_ <: Rep] <: HChain](inner: Tidiable[F]): CapturingType[F, ?] = {
            inner.nodeType match {
                case _: HEmptyType       => CapturingSingleton
                case _: HNonEmptyType[_] => CapturingAppend(inner)
            }
        }
    }

    /* (A) */
    private type CapturingSingletonType = Const[HSingleton[String]]
    private object CapturingSingleton extends CapturingType[Const[HEmpty], CapturingSingletonType] with HNonEmptyType[CapturingSingletonType] {
        override def tpe(using Quotes): Type[CapturingSingletonType] = Type.of[CapturingSingletonType]

        override def sanitiseCode[R <: Rep: Type](capture: Expr[Option[String]], sanitisedInner: => Expr[Const[HEmpty][R]])(using Quotes): SanitiseExpr[CapturingSingletonType[R]] = {
            '{
                val cap = $capture
                if (cap.isDefined) {
                    Some(Sanitised(HSingleton(cap.get), true))
                } else {
                    None
                }
            }
        }

        override def getCode[R <: Rep: Type](capture: Expr[HSingleton[String]], inner: => Expr[Const[HEmpty][R]])(using Quotes): Expr[HSingleton[String]] = {
            capture
        }

        override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[String], C], L, ?] = {
            nodes.flattenFunction(TCons(Type.of[String], types)) match {
                case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HSingleton[String], C], L, a] {
                    override def apply(chains: CCons[HSingleton[String], C], leaves: L)(using Quotes): Expr[a] = {
                        val capture = '{ ${ chains.head }.value }
                        flatten(chains.tail, LCons(capture, leaves))
                    }
                }
            }
        }
    }

    /* Type when the inner node contains more capturing groups, e.g. ((A)). */
    private type CapturingAppendType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[HSingleton[String], F[R]]
    private class CapturingAppend[F[_ <: Rep] <: HNonEmpty](inner: Tidiable[F]) extends CapturingType[F, CapturingAppendType[F]] with HNonEmptyType[CapturingAppendType[F]] {
        override def tpe(using Quotes): Type[CapturingAppendType[F]] = {
            given Type[F] = inner.tpe

            Type.of[CapturingAppendType[F]]
        }

        override def sanitiseCode[R <: Rep: Type](capture: Expr[Option[String]], sanitisedInner: => Expr[F[R]])(using Quotes): SanitiseExpr[CapturingAppendType[F][R]] = {
            given Type[F] = inner.tpe

            '{
                val cap = $capture
                if (cap.isDefined) {
                    Some(Sanitised(HAppend(HSingleton(cap.get), $sanitisedInner), true))
                } else {
                    None
                }
            }
        }

        override def getCode[R <: Rep: Type](capture: Expr[HSingleton[String]], getInner: => Expr[F[R]])(using Quotes): Expr[HAppend[HSingleton[String], F[R]]] = {
            given Type[F] = inner.tpe

            '{ HAppend($capture, $getInner) }
        }

        override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[CapturingAppendType[F][R], C], L, ?] = {
            given Type[F] = inner.tpe

            inner.flattenFunction(nodes, TCons(Type.of[String], types)) match {
                case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[CapturingAppendType[F][R], C], L, a] {
                    override def apply(chains: CCons[CapturingAppendType[F][R], C], leaves: L)(using Quotes): Expr[a] = {
                        '{
                            val node = ${ chains.head }
                            ${ flatten(CCons('{ node.right }, chains.tail), LCons('{ node.left.value }, leaves)) }
                        }
                    }
                }
            }
        }
    }
}
