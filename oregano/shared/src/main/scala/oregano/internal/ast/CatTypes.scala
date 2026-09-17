package oregano.internal.ast

import oregano.internal.hchain.*
import oregano.internal.sanitised.*
import scala.quoted.{Expr, Quotes, Type}

trait CatTypes { this: Tidy =>
  /* Type of the `Cat` node with left type `F` and right type `G`. */
  protected sealed trait CatType[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain, H[_ <: Rep] <: HChain] { this: NodeType[H] =>
    final val asNodeType: NodeType[H] & CatType[F, G, H] = this
    def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]])(using Quotes): SanitiseExpr[H[R]]
    def getCode[R <: Rep: Type](getLeft: => Expr[F[R]], getRight: => Expr[G[R]])(using Quotes): Expr[H[R]]
  }

  protected object CatType {
    def apply[F[_ <: Rep] <: HChain, G[_ <: Rep] <: HChain](left: Tidiable[F], right: Tidiable[G])(using Quotes): CatType[F, G, ?] = {
      (left.nodeType, right.nodeType) match {
        case (_: HEmptyType, _: HEmptyType) => CatEmpty()
        case (leftType: SingletonOption[f], _: HEmptyType) => {
          given Type[f] = leftType.innerType
          CatLeftOption(leftType)
        }
        case (_: HEmptyType, rightType: SingletonOption[g]) => {
          given Type[g] = rightType.innerType
          CatRightOption(rightType)
        }
        case (leftType: HNonEmptyType[f], _: HEmptyType) => {
          given Type[f] = leftType.tpe
          CatLeft(left)
        }
        case (_: HEmptyType, rightType: HNonEmptyType[g]) => {
          given Type[g] = rightType.tpe
          CatRight(right)
        }
        case (leftType: HNonEmptyType[f], rightType: HNonEmptyType[g]) => {
          given Type[f] = leftType.tpe
          given Type[g] = rightType.tpe
          CatBoth(left, right)
        }
      }
    }
  }

  /* AB */
  private class CatEmpty(using Type[Const[HEmpty]]) extends CatType[Const[HEmpty], Const[HEmpty], Const[HEmpty]] with HEmptyType {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): SanitiseExpr[Const[HEmpty][R]] = {
      sanitiseEmpty
    }

    override def getCode[R <: Rep: Type](getLeft: => Expr[Const[HEmpty][R]], getRight: => Expr[Const[HEmpty][R]])(using Quotes): Expr[HEmpty.type] = {
      '{ HEmpty }
    }
  }

  /* (A)?B */
  private class CatLeftOption[F[_ <: Rep] <: HNonEmpty](leftType: SingletonOption[F])(using Type[F], Type[SingletonOptionType[F]]) extends CatType[SingletonOptionType[F], Const[HEmpty], SingletonOptionType[F]] with SingletonOption[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[SingletonOptionType[F][R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): SanitiseExpr[SingletonOptionType[F][R]] = {
      sanitisedLeft
    }

    override def getCode[R <: Rep: Type](getLeft: => Expr[SingletonOptionType[F][R]], getRight: => Expr[Const[HEmpty][R]])(using Quotes): Expr[HSingleton[Option[F[R]]]] = {
      getLeft
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[F[R], ?] = leftType.tidyInner
  }

  /* A(B)? */
  private class CatRightOption[G[_ <: Rep] <: HNonEmpty](rightType: SingletonOption[G])(using Type[G], Type[SingletonOptionType[G]]) extends CatType[Const[HEmpty], SingletonOptionType[G], SingletonOptionType[G]] with SingletonOption[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[SingletonOptionType[G][R]])(using Quotes): SanitiseExpr[SingletonOptionType[G][R]] = {
      sanitisedRight
    }

    override def getCode[R <: Rep: Type](getLeft: => Expr[Const[HEmpty][R]], getRight: => Expr[SingletonOptionType[G][R]])(using Quotes): Expr[HSingleton[Option[G[R]]]] = {
      getRight
    }

    override def tidyInner[R <: Rep: Type](using RepType[R])(using Quotes): TidyFunction[G[R], ?] = rightType.tidyInner
  }

  /* (A)B */
  private class CatLeft[F[_ <: Rep] <: HNonEmpty](left: Tidiable[F])(using Type[F]) extends CatType[F, Const[HEmpty], F] with HNonEmptyType[F] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[Const[HEmpty][R]])(using Quotes): SanitiseExpr[F[R]] = {
      sanitisedLeft
    }

    override def getCode[R <: Rep: Type](getLeft: => Expr[F[R]], getRight: => Expr[Const[HEmpty][R]])(using Quotes): Expr[F[R]] = {
      getLeft
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[F[R], C], L, ?] = {
      left.flattenFunction(nodes, types)
    }
  }

  /* A(B) */
  private class CatRight[G[_ <: Rep] <: HNonEmpty](right: Tidiable[G])(using Type[G]) extends CatType[Const[HEmpty], G, G] with HNonEmptyType[G] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[Const[HEmpty][R]], sanitisedRight: => SanitiseExpr[G[R]])(using Quotes): SanitiseExpr[G[R]] = {
      sanitisedRight
    }

    override def getCode[R <: Rep: Type](getLeft: => Expr[Const[HEmpty][R]], getRight: => Expr[G[R]])(using Quotes): Expr[G[R]] = {
      getRight
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using RepType[R])(using Quotes): FlattenFunction[CCons[G[R], C], L, ?] = {
      right.flattenFunction(nodes, types)
    }
  }

  /* (A)(B) */
  private type CatBothType[F[_ <: Rep] <: HNonEmpty, G[_ <: Rep] <: HNonEmpty] = [R <: Rep] =>> HAppend[F[R], G[R]]
  private class CatBoth[F[_ <: Rep] <: HNonEmpty: Type, G[_ <: Rep] <: HNonEmpty: Type](left: Tidiable[F], right: Tidiable[G])(using Type[CatBothType[F, G]]) extends CatType[F, G, CatBothType[F, G]] with HNonEmptyType[CatBothType[F, G]] {
    override def sanitiseCode[R <: Rep: Type](sanitisedLeft: => SanitiseExpr[F[R]], sanitisedRight: => SanitiseExpr[G[R]])(using Quotes): SanitiseExpr[CatBothType[F, G][R]] = {
      '{
        val left = $sanitisedLeft
        val right = $sanitisedRight
        if (left.isDefined && right.isDefined) {
          Some(Sanitised(HAppend(left.get.captures, right.get.captures), left.get.any || right.get.any))
        } else {
          None
        }
      }
    }

    override def getCode[R <: Rep: Type](getLeft: => Expr[F[R]], getRight: => Expr[G[R]])(using Quotes): Expr[HAppend[F[R], G[R]]] = {
      '{ HAppend($getLeft, $getRight) }
    }

    override def flattenFunction[C <: Chains, L <: Leaves, R <: Rep: Type](nodes: Nodes[C], types: Types[L])(using rep: RepType[R])(using Quotes): FlattenFunction[CCons[HAppend[F[R], G[R]], C], L, ?] = {
      left.flattenFunction(NCons(right, rep, nodes), types) match {
        case flatten @ FlattenFunction(given Type[a]) => new FlattenFunction[CCons[HAppend[F[R], G[R]], C], L, a] {
          override def apply(chains: CCons[HAppend[F[R], G[R]], C], leaves: L)(using Quotes): Expr[a] = {
            '{
              val node = ${ chains.head }
              ${ flatten(CCons('{ node.left }, CCons('{ node.right }, chains.tail)), leaves) }
            }
          }
        }
      }
    }
  }
}
