package oregano.internal.ast

import scala.quoted.{Expr, Quotes, Type}

object QTypes {
  type Tidy[T <: Tuple] = T match {
    case EmptyTuple => Unit
    case h *: t     => TidyNonEmpty[h, t]
  }

  type TidyNonEmpty[H, T <: Tuple] = T match {
    case EmptyTuple    => H
    case NonEmptyTuple => H *: T
  }

  sealed trait QList[T <: Tuple] {
    def tidyExpr(using Quotes): Expr[Tidy[T]]
  }

  type QNil = QNil.type
  case object QNil extends QList[EmptyTuple] {
    override def tidyExpr(using Quotes): Expr[Unit] = '{ () }
  }

  case class QCons[H, T <: Tuple](expr: Expr[H], tpe: Type[H], tail: QList[T]) extends QList[H *: T] {
    override def tidyExpr(using Quotes): Expr[TidyNonEmpty[H, T]] = tail match {
      case QNil           => expr
      case QCons(_, _, _) => tupleExpr(this)
    }
  }

  def tupleExpr[T <: Tuple](qlist: QList[T])(using Quotes): Expr[T] = qlist match {
    case QNil => '{ EmptyTuple }
    case QCons(e0, given Type[t0], tail0) => tail0 match {
      case QNil => '{ Tuple1($e0) }
      case QCons(e1, given Type[t1], tail1) => tail1 match {
        case QNil => '{ Tuple2($e0, $e1) }
        case QCons(_, _, _) => ???
      }
    }
  }
}
