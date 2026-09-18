package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

/* Use `Either` if the node is not repeated and `InclusiveOr` if it is. The
   pattern (A)|(B) can only capture exactly one of A or B, but (?:(A)|(B))+ can
   capture A, B, or both.
   This needs to be oustide the trait otherwise the compiler complains about
   a missing `Type` instance. */
type AltRep[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, R <: Rep, InclusiveOr[+_ <: HChain, +_ <: HChain]] = R match {
    case false => Either[F[R], G[R]]
    case true  => InclusiveOr[F[R], G[R]]
}

trait AltTypes { this: Tidy =>
    type AltSingleton[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[AltRep[F, G, R, InclusiveOr]]
    type AltSingletonOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[AltSingleton[F, G][R]]]

    protected sealed trait AltType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] { this: NodeType[H] =>
        final val asNodeType: NodeType[H] & AltType[F, G, H] = this
        def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]])(using RepType[R])(using Quotes): SanitiseExpr[H[R]]
    }

    protected object AltType {
        def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Tidiable[F], right: Tidiable[G]): AltType[F, G, ?] = {
            (left.nodeType, right.nodeType) match {
                case (_: HEmptyType, _: HEmptyType)                                => AltEmpty
                case (leftType: SingletonOption[f], _: HEmptyType)                 => AltLeftOption(leftType)
                case (_: HEmptyType, rightType: SingletonOption[g])                => AltRightOption(rightType)
                case (_: HNonEmptyType[f], _: HEmptyType)                          => AltLeft(left)
                case (_: HEmptyType, _: HNonEmptyType[g])                          => AltRight(right)
                case (leftType: SingletonOption[f], rightType: SingletonOption[g]) => AltBothOption(leftType, rightType)
                case (leftType: SingletonOption[f], _: HNonEmptyType[g])           => AltBothLeftOption(leftType, right)
                case (_: HNonEmptyType[f], rightType: SingletonOption[g])          => AltBothRightOption(left, rightType)
                case (_: HNonEmptyType[_], _: HNonEmptyType[g])                    => AltBoth(left, right)
            }
        }
    }

    /* A|B */
    private object AltEmpty extends AltType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType {
        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
            sanitiseEmpty
        }
    }

    /* (A)|B */
    private type AltLeftType = SingletonOptionType
    private class AltLeft[F[_ <: Rep] <: HNonEmpty](left: Tidiable[F]) extends AltType[F, Const[HEmpty], AltLeftType[F]] with SingletonOption[F] {
        override def innerType(using Quotes): Type[F] = left.tpe

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltLeftType[F][R]] = {
            given Type[F] = left.tpe
            sanitiseOpt(sanitisedLeft)
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = left.tidyFunction
    }

    /* A|(B) */
    private type AltRightType = SingletonOptionType
    private class AltRight[G[_ <: Rep] <: HNonEmpty](right: Tidiable[G]) extends AltType[Const[HEmpty], G, AltRightType[G]] with SingletonOption[G] {
        override def innerType(using Quotes): Type[G] = right.tpe

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[G[R]])(using RepType[R])(using Quotes): SanitiseExpr[AltRightType[G][R]] = {
            given Type[G] = right.tpe
            sanitiseOpt(sanitisedRight)
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = right.tidyFunction
    }

    /* (A)?|B */
    private type AltLeftOptionType = SingletonOptionType
    private class AltLeftOption[F[_ <: Rep] <: HNonEmpty](leftType: SingletonOption[F]) extends AltType[AltLeftOptionType[F], Const[HEmpty], AltLeftOptionType[F]] with SingletonOption[F] {
        override def innerType(using Quotes): Type[F] = leftType.innerType

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[AltLeftOptionType[F][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltLeftOptionType[F][R]] = {
            sanitisedLeft
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = leftType.tidyInner
    }

    /* A|(B)? */
    private type AltRightOptionType = SingletonOptionType
    private class AltRightOption[G[_ <: Rep] <: HNonEmpty](rightType: SingletonOption[G]) extends AltType[Const[HEmpty], AltRightOptionType[G], AltRightOptionType[G]] with SingletonOption[G] {
        override def innerType(using Quotes): Type[G] = rightType.innerType

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[AltRightOptionType[G][R]])(using RepType[R])(using Quotes): SanitiseExpr[AltRightOptionType[G][R]] = {
            sanitisedRight
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = rightType.tidyInner
    }

    /* (A)?|(B)? */
    private type AltBothOptionType = AltSingletonOption
    private class AltBothOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](leftType: SingletonOption[F], rightType: SingletonOption[G])
        extends AltType[SingletonOptionType[F], SingletonOptionType[G], AltBothOptionType[F, G]]
        with SingletonOption[AltSingleton[F, G]] {
        override def innerType(using Quotes): Type[AltSingleton[F, G]] = {
            given Type[F] = leftType.innerType
            given Type[G] = rightType.innerType
            given Type[InclusiveOr] = inclusiveOrType

            Type.of[AltSingleton[F, G]]
        }

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[SingletonOptionType[F][R]], sanitisedRight: => SanitiseExpr[SingletonOptionType[G][R]])(using rep: RepType[R])(using Quotes): SanitiseExpr[AltBothOptionType[F, G][R]] = {
            given Type[F] = leftType.innerType
            given Type[G] = rightType.innerType
            given Type[InclusiveOr] = inclusiveOrType

            rep match {
                case RepFalse => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(Left(left.get.captures.value.get)))), true))
                    } else if (right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(Right(right.get.captures.value.get)))), true))
                    } else {
                        Some(Sanitised(HSingleton(None), false))
                    }
                }
                case RepTrue => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any && right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromBoth('{ left.get.captures.value.get }, '{ right.get.captures.value.get }) }))), true))
                    } else if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromLeft('{ left.get.captures.value.get }) }))), true))
                    } else if (right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromRight('{ right.get.captures.value.get }) }))), true))
                    } else {
                        Some(Sanitised(HSingleton(None), false))
                    }
                }
            }
        }

        override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
            given Type[F] = leftType.innerType
            given Type[G] = rightType.innerType

            tidyAlt(leftType.tidyInner, rightType.tidyInner)
        }
    }

    /* (A)?|(B) */
    private type AltBothLeftOptionType = AltSingletonOption
    private class AltBothLeftOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](leftType: SingletonOption[F], rightRegex: Tidiable[G])
        extends AltType[SingletonOptionType[F], G, AltBothLeftOptionType[F, G]]
        with SingletonOption[AltSingleton[F, G]] {
        override def innerType(using Quotes): Type[AltSingleton[F, G]] = {
            given Type[F] = leftType.innerType
            given Type[G] = rightRegex.tpe
            given Type[InclusiveOr] = inclusiveOrType

            Type.of[AltSingleton[F, G]]
        }

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[SingletonOptionType[F][R]], sanitisedRight: => SanitiseExpr[G[R]])(using rep: RepType[R])(using Quotes): SanitiseExpr[AltBothLeftOptionType[F, G][R]] = {
            given Type[F] = leftType.innerType
            given Type[G] = rightRegex.tpe
            given Type[InclusiveOr] = inclusiveOrType

            rep match {
                case RepFalse => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(Left(left.get.captures.value.get)))), true))
                    } else if (right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(Right(right.get.captures)))), true))
                    } else {
                        Some(Sanitised(HSingleton(None), false))
                    }
                }
                case RepTrue => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any && right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromBoth('{ left.get.captures.value.get }, '{ right.get.captures }) }))), true))
                    } else if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromLeft('{ left.get.captures.value.get }) }))), true))
                    } else if (right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromRight('{ right.get.captures }) }))), true))
                    } else {
                        Some(Sanitised(HSingleton(None), false))
                    }
                }
            }
        }

        override def tidyInner[R <: Rep: Type](using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
            given Type[F] = leftType.innerType
            given Type[G] = rightRegex.tpe

            tidyAlt(leftType.tidyInner, rightRegex.tidyFunction)
        }
    }

    /* (A)|(B)? */
    private type AltBothRightOptionType = AltSingletonOption
    private class AltBothRightOption[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](leftRegex: Tidiable[F], rightType: SingletonOption[G])
        extends AltType[F, SingletonOptionType[G], AltBothRightOptionType[F, G]]
        with SingletonOption[AltSingleton[F, G]] {
        override def innerType(using Quotes): Type[AltSingleton[F, G]] = {
            given Type[F] = leftRegex.tpe
            given Type[G] = rightType.innerType
            given Type[InclusiveOr] = inclusiveOrType

            Type.of[AltSingleton[F, G]]
        }

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[SingletonOptionType[G][R]])(using rep: RepType[R])(using Quotes): SanitiseExpr[AltBothRightOptionType[F, G][R]] = {
            given Type[F] = leftRegex.tpe
            given Type[G] = rightType.innerType
            given Type[InclusiveOr] = inclusiveOrType

            rep match {
                case RepFalse => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(Left(left.get.captures)))), true))
                    } else if (right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(Right(right.get.captures.value.get)))), true))
                    } else {
                        Some(Sanitised(HSingleton(None), false))
                    }
                }
                case RepTrue => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any && right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromBoth('{ left.get.captures }, '{ right.get.captures.value.get }) }))), true))
                    } else if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromLeft('{ left.get.captures }) }))), true))
                    } else if (right.isDefined && right.get.any) {
                        Some(Sanitised(HSingleton(Some(HSingleton(${ fromRight('{ right.get.captures.value.get }) }))), true))
                    } else {
                        Some(Sanitised(HSingleton(None), false))
                    }
                }
            }
        }

        override def tidyInner[R <: Rep: Type](using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
            given Type[F] = leftRegex.tpe
            given Type[G] = rightType.innerType

            tidyAlt(leftRegex.tidyFunction, rightType.tidyInner)
        }
    }

    /* (A)|(B) */
    private type AltBothType = AltSingleton
    private class AltBoth[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty](left: Tidiable[F], right: Tidiable[G]) extends AltType[F, G, AltBothType[F, G]] with HNonEmptyType[AltBothType[F, G]] {
        override def tpe(using Quotes): Type[AltBothType[F, G]] = {
            given Type[F] = left.tpe
            given Type[G] = right.tpe
            given Type[InclusiveOr] = inclusiveOrType

            Type.of[AltBothType[F, G]]
        }

        override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]])(using rep: RepType[R])(using Quotes): SanitiseExpr[AltBothType[F, G][R]] = {
            given Type[F] = left.tpe
            given Type[G] = right.tpe
            given Type[InclusiveOr] = inclusiveOrType

            rep match {
                case RepFalse => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && left.get.any) {
                        Some(Sanitised(HSingleton(Left(left.get.captures)), true))
                    } else if (right.isDefined) {
                        Some(Sanitised(HSingleton(Right(right.get.captures)), right.get.any))
                    } else {
                        None
                    }
                }
                case RepTrue => '{
                    val left = $sanitisedLeft
                    val right = $sanitisedRight
                    if (left.isDefined && right.isDefined) {
                        Some(Sanitised(HSingleton(${ fromBoth('{ left.get.captures }, '{ right.get.captures }) }), left.get.any || right.get.any))
                    } else if (left.isDefined) {
                        Some(Sanitised(HSingleton(${ fromLeft('{ left.get.captures }) }), left.get.any))
                    } else if (right.isDefined) {
                        Some(Sanitised(HSingleton(${ fromRight('{ right.get.captures }) }), right.get.any))
                    } else {
                        None
                    }
                }
            }
        }

        override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[AltRep[F, G, R, InclusiveOr]], C], L, ?] = {
            given Type[F] = left.tpe
            given Type[G] = right.tpe

            tidyAlt(left.tidyFunction, right.tidyFunction) match {
                case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[a], types)) match {
                    case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[AltBothType[F, G][R], C], L, b] {
                        override def apply(chains: CCons[AltBothType[F, G][R], C], leaves: L)(using Quotes): Expr[b] = {
                            flatten(chains.tail, LCons(tidy(chains.head), leaves))
                        }
                    }
                }
            }
        }
    }

    private def tidyAlt[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type, A, B](tidyLeft: TidyFunction[F[R], A], tidyRight: TidyFunction[G[R], B])(using rep: RepType[R])(using Quotes): TidyFunction[AltSingleton[F, G][R], ?] = {
        given Type[A] = tidyLeft.tpe
        given Type[B] = tidyRight.tpe
        given Type[InclusiveOr] = inclusiveOrType

        rep match {
            case RepFalse => new TidyFunction[AltSingleton[F, G][R], Either[A, B]] {
                override def apply(chain: Expr[AltSingleton[F, G][R]])(using Quotes): Expr[Either[A, B]] = {
                    '{
                        ($chain.value: Either[F[R], G[R]]) match {
                            case Left(left)   => Left(${ tidyLeft('left) })
                            case Right(right) => Right(${ tidyRight('right) })
                        }
                    }
                }
            }
            case RepTrue => new TidyFunction[AltSingleton[F, G][R], InclusiveOr[A, B]] {
                override def apply(chain: Expr[AltSingleton[F, G][R]])(using Quotes): Expr[InclusiveOr[A, B]] = {
                    bimap(tidyLeft(_), tidyRight(_))('{ $chain.value })
                }
            }
        }
    }
}
