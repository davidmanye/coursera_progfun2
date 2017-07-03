package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map(entry => (entry._1, Signal(eval(entry._2(), namedExpressions))))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    if (hasCyclic(expr, references, Set())) Double.NaN
    else expr match {
      case Literal(x) => x
      case Ref(name) => eval(getReferenceExpr(name, references), references)
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  def hasCyclic(expr: Expr, references: Map[String, Signal[Expr]], visited: Set[String]): Boolean = {
    expr match {
      case Literal(_) => false
      case Ref(name) => if (visited contains name) true else hasCyclic(getReferenceExpr(name, references), references, visited + name)
      case Plus(a, b) => hasCyclic(a, references, visited) || hasCyclic(a, references, visited)
      case Minus(a, b) => hasCyclic(a, references, visited) || hasCyclic(a, references, visited)
      case Times(a, b) => hasCyclic(a, references, visited) || hasCyclic(a, references, visited)
      case Divide(a, b) => hasCyclic(a, references, visited) || hasCyclic(a, references, visited)
    }
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
