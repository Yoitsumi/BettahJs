package js

import ast._

import scala.language.dynamics

/**
 * Created by Kamil on 27.10.2015.
 */
object AstSugar {

  trait Expr[T] {
    def toExpression(v: T): Expression
  }
  object Expr {
    implicit val `Int is convertable to Expression` = new Expr[Int] {
      override def toExpression(v: Int): Expression = new NumberValue(v)
    }
    implicit val `Double is convertable to Expression` = new Expr[Double] {
      override def toExpression(v: Double): Expression = new NumberValue(v)
    }
    implicit val `Boolean is convertable to Expression` = new Expr[Boolean] {
      override def toExpression(v: Boolean): Expression = new BooleanValue(v)
    }
    implicit val `String is convertable to Expression` = new Expr[String] {
      override def toExpression(v: String): Expression = new StringValue(v)
    }
  }

  class VarSelector(val parent: Expression) extends Dynamic {
    def selectDynamic(name: String) = new VarSelector( Access(parent, name) )
    def applyDynamic(name: String)(params: Expression*) = Call(selectDynamic(name), params, Seq(), None)
    def toExpr = parent
  }

  implicit def varSelector2Expression(vs: VarSelector): Expression = vs.toExpr

  object js$ extends Dynamic {
    def apply(e: Expression) = new VarSelector(e)
    def selectDynamic(name: String) = new VarSelector(VarAccess(name))
    def applyDynamic(name: String)(params: Expression*) = Call(selectDynamic(name), params, Seq(), None)
  }

}
