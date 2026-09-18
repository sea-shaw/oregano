package oregano.internal.ast

import cats.data.Chain
import oregano.internal.sanitised.Sanitised
import scala.quoted.{Expr, Quotes, Type}

trait QTypes { this: Tidy =>

  sealed abstract class QTidy[A](using val tpe: Type[A]) {
    def get(using Captures)(using Quotes): Expr[A]
    def getSanitised(using Captures)(using Quotes): Expr[Option[Sanitised[A]]]
  }
  object QTidy {
    def unapply[A](x: QTidy[A]): Tuple1[Type[A]] = Tuple1(x.tpe)
  }

  sealed trait QChain {
    final def elem(using Quotes): QTidy[?] = ???
    def elems(using Quotes): Chain[QTidy[?]]
  }

  type QEmpty = QEmpty.type
  case object QEmpty extends QChain {
    override def elems(using Quotes): Chain[QTidy[?]] = Chain.nil
  }

  sealed trait QNonEmpty extends QChain
  case class QCapture[A <: QChain](i: Int, inner: A) extends QNonEmpty {
    override def elems(using Quotes): Chain[QTidy[?]] = {
      val head = new QTidy[String] {
        override def get(using caps: Captures)(using Quotes): Expr[String] = {
          caps.capture(caps.startIndex(i), caps.endIndex(i))
        }

        override def getSanitised(using caps: Captures)(using Quotes): Expr[Option[Sanitised[String]]] = {
          '{
            val l = ${ caps.startIndex(i) }
            val u = ${ caps.endIndex(i) }
            if l >= 0 && u >= 0 then Some(Sanitised(${ caps.capture('l, 'u) }, true)) else None
          }
        }
      }

      head +: inner.elems
    }
  }

  case class QAppend[A <: QNonEmpty, B <: QNonEmpty](left: A, right: B) extends QNonEmpty {
    override def elems(using Quotes): Chain[QTidy[?]] = {
      left.elems ++ right.elems
    }
  }

  case class QOption[A <: QNonEmpty](opt: A) extends QNonEmpty {
    override def elems(using Quotes): Chain[QTidy[?]] = {
      opt.elem match {
        case tidy @ QTidy(given Type[a]) => {
          val elem = new QTidy[Option[a]] {
            override def get(using Captures)(using Quotes): Expr[Option[a]] = {
              '{ ${ tidy.getSanitised }.map(_.captures) }
            }

            override def getSanitised(using Captures)(using Quotes): Expr[Option[Sanitised[Option[a]]]] = {
              '{
                ${ tidy.getSanitised } match {
                  case None                           => Some(Sanitised(None, false))
                  case Some(Sanitised(captures, any)) => Some(Sanitised(Some(captures), any))
                }
              }
            }
          }
          Chain.one(elem)
        }
      }
    }
  }

  case class QEither[A <: QNonEmpty, B <: QNonEmpty](left: A, right: B) extends QNonEmpty {
    override def elems(using Quotes): Chain[QTidy[?]] = {
      val elem = (left.elem, right.elem) match {
        case (tidyLeft @ QTidy(given Type[a]), tidyRight @ QTidy(given Type[b])) => new QTidy[Either[a, b]] {
          override def get(using Captures)(using Quotes): Expr[Either[a, b]] = {
            '{ $getSanitised.get.captures }
          }

          override def getSanitised(using Captures)(using Quotes): Expr[Option[Sanitised[Either[a, b]]]] = {
            '{
              val left = ${ tidyLeft.getSanitised }
              if (left.isDefined && left.get.any) {
                Some(Sanitised(Left(left.get.captures), true))
              } else {
                val right = ${ tidyRight.getSanitised }
                if (right.isDefined && right.get.any) {
                  Some(Sanitised(Right(right.get.captures), true))
                } else {
                  None
                }
              }
            }
          }
        }
      }
      Chain.one(elem)
    }
  }

  case class QIor[A <: QNonEmpty, B <: QNonEmpty](left: A, right: B) extends QNonEmpty {
    override def elems(using Quotes): Chain[QTidy[?]] = {
      given Type[InclusiveOr] = inclusiveOrType
      val elem = (left.elem, right.elem) match {
        case (tidyLeft @ QTidy(given Type[a]), tidyRight @ QTidy(given Type[b])) => new QTidy[InclusiveOr[a, b]] {
          override def get(using Captures)(using Quotes): Expr[InclusiveOr[a, b]] = {
            '{ $getSanitised.get.captures }
          }

          override def getSanitised(using Captures)(using Quotes): Expr[Option[Sanitised[InclusiveOr[a, b]]]] = {
            '{
              val left = ${ tidyLeft.getSanitised }
              val right = ${ tidyRight.getSanitised }
              if (left.isDefined && left.get.any && right.isDefined && right.get.any) {
                Some(Sanitised(${ fromBoth('{ left.get.captures }, '{ right.get.captures }) }, true))
              } else if (left.isDefined && left.get.any) {
                Some(Sanitised(${ fromLeft('{ left.get.captures }) }, true))
              } else if (right.isDefined && right.get.any) {
                Some(Sanitised(${ fromRight('{ right.get.captures }) }, true))
              } else {
                None
              }
            }
          }
        }
      }
      Chain.one(elem)
    }
  }
}
