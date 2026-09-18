package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Type, Quotes}

type Groups = Array[Int]

/* `Rep` is true if a node is inside a repeated node or false otherwise. */
type Rep = Boolean
/* Use GADTs so the compiler can resolve the type of `R`. */
sealed trait RepType[R <: Rep]
case object RepTrue extends RepType[true]
case object RepFalse extends RepType[false]

type Const[+A] = [_] =>> A

/* Parent trait of `AST` so the internals can be accessed from mixin traits for
   each node's types. `InclusiveOr` and `Regex`/`Tidiable` are path-dependent
   so it's neater for everything to be path-dependent. */
trait Tidy {
    /* Type to use to combine A and B in (?:(A)|(B))+ or similar. */
    type InclusiveOr[+_, +_]

    protected def inclusiveOrType(using Quotes): Type[InclusiveOr]

    /* Construct `InclusiveOr` */
    protected def fromLeft[A: Type](left: Expr[A])(using Quotes): Expr[InclusiveOr[A, Nothing]]
    protected def fromRight[B: Type](right: Expr[B])(using Quotes): Expr[InclusiveOr[Nothing, B]]
    protected def fromBoth[A: Type, B: Type](left: Expr[A], right: Expr[B])(using Quotes): Expr[InclusiveOr[A, B]]

    /* Bimap over `InclusiveOr`. */
    protected def bimap[A: Type, B: Type, C: Type, D: Type](f: Expr[A] => Quotes ?=> Expr[C], g: Expr[B] => Quotes ?=> Expr[D])(expr: Expr[InclusiveOr[A, B]])(using Quotes): Expr[InclusiveOr[C, D]]

    /* Parent of `Regex` abstract class. Allows internals to be accessed from
     mixin traits. `F` is higher kinded since the type may be different if the
     node is repeated (`F[true]`) or not (`F[false]`). */
    abstract class Tidiable[F[_ <: Rep] <: HChain](final val nodeType: NodeType[F]) {
        final def tpe(using Quotes): Type[F] = nodeType.tpe

        /* Returns a function to tidy `F[R]` into `Unit`, a single value, or a tuple.
       Removes all `HChain` types from `F[R]`. `R` is whether or not the node
       is repeated. */
        final def tidyFunction[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = {
            flattenFunction(NNil, TNil) match {
                case flatten @ FlattenFunction(given Type[a]) => new TidyFunction[F[R], a] {
                    override def apply(chain: Expr[F[R]])(using Quotes): Expr[a] = {
                        flatten(CCons(chain, CNil), LNil)
                    }
                }
            }
        }

        /* Returns a function to flatten `chains: C` onto `leaves: L`. `R` is
       whether or not the node is repeated. */
        final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
            nodeType.flattenFunction(nodes, types)
        }
    }

    /* Type of a node. Must be either empty or non-empty, enforced by the sealed
     trait. Each node should have a trait with self-type `NodeType` that defines
     the options for it's own type. */
    sealed trait NodeType[F[_ <: Rep] <: HChain] {
        def tpe(using Quotes): Type[F]

        /* Returns a function to flatten `chains: C` onto `leaves: L`. */
        def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?]
    }

    /* Type of a node with no capture groups. */
    trait HEmptyType extends NodeType[Const[HEmpty]] {
        override final def tpe(using Quotes): Type[Const[HEmpty]] = Type.of[Const[HEmpty]]

        override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HEmpty, C], L, ?] = {
            nodes.flattenFunction(types) match {
                case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HEmpty, C], L, a] {
                    override def apply(chains: CCons[HEmpty, C], leaves: L)(using Quotes): Expr[a] = flatten(chains.tail, leaves)
                }
            }
        }
    }

    /* Type of a node with some capture groups. */
    trait HNonEmptyType[F[_ <: Rep] <: HNonEmpty] extends NodeType[F]

    /* Type of a node with a single `Option` containing all its capturing groups. */
    type SingletonOptionType[F[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HSingleton[Option[F[R]]]
    trait SingletonOption[F[_ <: Rep] <: HNonEmpty] extends HNonEmptyType[SingletonOptionType[F]] {
        def innerType(using Quotes): Type[F]

        override final def tpe(using Quotes) = {
            given Type[F] = innerType
            Type.of[SingletonOptionType[F]]
        }

        /* Tidy function for the inner, non-optional type. Used to handle nested
       optional nodes to prevent types like `Option[Option[A]]`. */
        def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?]

        override final def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[HSingleton[Option[F[R]]], C], L, ?] = {
            given Type[F] = innerType

            tidyInner match {
                case tidy @ TidyFunction(given Type[a]) => nodes.flattenFunction(TCons(Type.of[Option[a]], types)) match {
                    case flatten @ FlattenFunction(given Type[b]) => new FlattenFunction[CCons[SingletonOptionType[F][R], C], L, b] {
                        override def apply(chains: CCons[SingletonOptionType[F][R], C], leaves: L)(using Quotes): Expr[b] = {
                            val optExpr = '{
                                val opt = ${ chains.head }.value
                                if (opt.isDefined) {
                                    Some(${ tidy('{ opt.get }) })
                                } else {
                                    None
                                }
                            }
                            flatten(chains.tail, LCons(optExpr, leaves))
                        }
                    }
                }
            }
        }
    }

    /* HList of AST nodes. Used to construct a flatten function in linear time.
     `C` is the type of an HList of the `HChain` types of each node. */
    protected sealed trait Nodes[C <: Chains] {
        /* Returns a function to flatten `chains: C` onto `leaves: L`. */
        def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[C, L, ?]
    }

    protected case object NNil extends Nodes[CNil] {
        override def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CNil, L, ?] = {
            buildFunction(types) match {
                case build @ BuildFunction(given Type[a]) => new FlattenFunction[CNil, L, a] {
                    override def apply(chains: CNil, leaves: L)(using Quotes): Expr[a] = {
                        build(leaves)
                    }
                }
            }
        }
    }

    protected case class NCons[F[_ <: Rep] <: HChain, R <: Rep: Type, C <: Chains](head: Tidiable[F], rep: RepType[R], tail: Nodes[C]) extends Nodes[CCons[F[R], C]] {
        override def flattenFunction[L <: Leaves](types: Types[L])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
            given RepType[R] = rep
            head.flattenFunction(tail, types)
        }
    }

    /* HList of `Expr`s of `HChain`s. */
    protected sealed trait Chains
    protected type CNil = CNil.type
    protected case object CNil extends Chains
    protected case class CCons[A <: HChain, C <: Chains](head: Expr[A], tail: C) extends Chains

    /* HList of `Type`s. `L` is the type of an HList of `Expr`s of the
     corresponding types.*/
    protected sealed trait Types[L <: Leaves]
    protected case object TNil extends Types[LNil]
    protected case class TCons[A, L <: Leaves](head: Type[A], tail: Types[L]) extends Types[LCons[A, L]]

    /* HList of `Expr`s. */
    protected sealed trait Leaves
    protected type LNil = LNil.type
    protected case object LNil extends Leaves
    protected case class LCons[A, L <: Leaves](head: Expr[A], tail: L) extends Leaves

    /* Function that tidies an `HChain` type into `Unit`, a single value, or a
     tuple. */
    abstract class TidyFunction[A <: HChain, B](using val tpe: Type[B]) {
        def apply(chain: Expr[A])(using Quotes): Expr[B]
    }
    object TidyFunction {
        /* Allows `case TidyFunction(given Type[a]) => ...` syntax so we don't have
       to deal with `tpe.Underlying`. Annoyingly, `given` syntax can only be
       used in `match` expressions, not `val` definitions. */
        def unapply[A <: HChain, B](tidyFunction: TidyFunction[A, B]): Tuple1[Type[B]] = Tuple1(tidyFunction.tpe)
    }

    /* Function that flattens chains of type `C` onto leaves of type `L`,
     returning `Unit`, a single value, or a tuple. */
    protected abstract class FlattenFunction[C <: Chains, L <: Leaves, A](using val tpe: Type[A]) {
        def apply(chains: C, leaves: L)(using Quotes): Expr[A]
    }
    protected object FlattenFunction {
        def unapply[C <: Chains, L <: Leaves, A](flattenFunction: FlattenFunction[C, L, A]): Tuple1[Type[A]] = Tuple1(flattenFunction.tpe)
    }

    /* Constructs `Unit`, a single value, or a tuple from leaves of type L. */
    protected abstract class BuildFunction[L <: Leaves, A](using val tpe: Type[A]) {
        def apply(leaves: L)(using Quotes): Expr[A]
    }
    protected object BuildFunction {
        def unapply[L <: Leaves, A](buildFunction: BuildFunction[L, A]): Tuple1[Type[A]] = Tuple1(buildFunction.tpe)
    }

    /* Implementation of `buildFunction` is source-generated. Generator is in
     project/BuildFunction.scala. Generated code is compiled to
     oregano/jvm/target/scala-3.7.4/src_managed/main/scala/oregano/internal/ast/BuildFunction.scala */
    protected def buildFunction[L <: Leaves](types: Types[L])(using Quotes): BuildFunction[L, ?]

    /* Result of `sanitiseCode` for an empty node. Equivalent to `pure(HEmpty)`
     for the `SanitisedT[_]` applicative. */
    protected final def sanitiseEmpty(using Quotes): SanitiseExpr[HEmpty] = {
        '{ Some(Sanitised(HEmpty, false)) }
    }

    /* Result of `sanitiseCode` for a node with type `HSingleton[Option[_]]` */
    protected final def sanitiseOpt[F[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type](sanitised: SanitiseExpr[F[R]])(using Quotes): SanitiseExpr[SingletonOptionType[F][R]] = {
        '{
            val caps = $sanitised
            if (caps.isDefined) {
                Some(Sanitised(HSingleton(Some(caps.get.captures)), caps.get.any))
            } else {
                Some(Sanitised(HSingleton(None), false))
            }
        }
    }

    protected final def getOpt[F[_ <: Rep] <: HNonEmpty: Type, R <: Rep: Type](sanitised: SanitiseExpr[F[R]])(using Quotes): Expr[SingletonOptionType[F][R]] = {
        '{
            val opt = $sanitised
            HSingleton(if opt.isDefined then Some(opt.get.captures) else None)
        }
    }
}
