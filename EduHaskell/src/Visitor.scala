package eduhaskell
package visitor


/**
 * This package is here for demonstration purposes only. Whilst
 * the visitor pattern was (and remains) popular in languages
 * without the equivalent of case classes, or for projects
 * in which such classes cannot sensibly be used, Scala provides
 * more straightforward means of defining straightforward
 * properties of syntax trees recursively.
 *
 * In the `Formatting` module we define formatters for expressions and definitions
 * using the `Visitor` pattern. 
 * 
 * You may think that this "double dispatch" programming model is a little arcane when
 * used here; but in other contexts it has the virtue of making "differential" definitions of
 * functions at trees straightforward. By a "differential definition" of function `D`
 * relative to function `BASE` I mean a definition of `D` that inherits much but not
 * all the behaviour of `BASE` then defines only differences. An example of this is the
 * definition of `Detuple` below.
 *
 * On the other hand note that detupling could as easily have been defined by conventional means:
 * {{{
 *   def identity(e: Expr): Expr = e
 *
 *   def detuple(e: Expr): Expr =
 *      e match {
 *        case Apply(l, r) => Apply(detuple(l), detuple(r))
 *        ...
 *        case _ => identity(e)
 *      }
 * }}}
 */

import eduhaskell.syntax._

/**
 *
 * A universallly applicable expression `Visitor` must have definitions of visit methods
 * specified at each of the subtypes (extensions) of the `Expr` trait. It will be an
 * extension of the `ExprVisitor` trait.
 *
 * A number of simple expression and definition are defined in the package `Visitors`, and
 * "pretty" source text formatting of definitions and expressions is defined jointly by
 * `calculator.DefinitionFormat` and `calculator.ExprFormat`.
 *
 */
trait ExprVisitor[T] { thisVisitor =>
  def visit(it: Op): T
  def visit(it: Id): T
  def visit(it: Num): T
  def visit(it: Real): T
  def visit(it: Quote): T
  def visit(it: Tuple): T
  def visit(it: Sequence): T
  def visit(it: Binop): T
  def visit(it: Prefix): T
  def visit(it: Apply): T
  def visit(it: If): T
  def visit(it: Case): T
  def visit(it: Where): T
  def visit(it: PartialFun): T
  def visit(it: Function): T
  def visit(it: Bra): T
  def visit(it: Assoc): T
  // An ExprVisitor can be applied to an expression as if it were a function
  import Dispatch._
  @inline def apply(it: Expr): T = it.visit(thisVisitor)
}

/**
 * See `ExprVisitor`
 */
trait DefinitionVisitor[T] { thisVisitor =>
  def visit(it: Equation): T
  def visit(it: WhereEquation): T
  def visit(it: TypeDef): T
  def visit(it: GuardedEquation): T
  import Dispatch._
  @inline def apply(it: Definition): T = it.visit(thisVisitor)
}

/**
 * In the classical implementation of the visitor pattern a "visitable" trait (say `Expr`) specifies
 * {{{
 *    def visit[T](v: ExprVisitor)[T]
 * }}}
 *
 * Each of its concrete subtypes defines its own `visit` method with the same program text.
 * {{{
 *   def visit[T](v: ExprVisitor)[T] = v.visit(this)
 * }}}
 * The effect of "visit"ing an expression `e` with `v` is that the appropriate (overloaded) method of `v`
 * "visits" `e` without any need for "dynamic dispatching" of the call.
 *
 * But there is a cost: every subtype of `Expr` (and `Definition`) now gets "polluted" with exactly the
 * same boilerplate definition.
 *
 * Here we avoid the clutter by using an implicit class "extension" to the `Expr` type that now dispatches
 * dynamically to the appropriate method of 'v', according to the specific subtype of `expr` at which
 * it is invoked.
 */
object Dispatch {
  implicit class ExprDispatch(val expr: Expr) extends AnyVal {
    def visit[T](v: ExprVisitor[T]):T =
      expr match {
          case it: Op => v.visit(it)
          case it: Id => v.visit(it)
          case it: Num => v.visit(it)
          case it: Real => v.visit(it)
          case it: Quote => v.visit(it)
          case it: Tuple => v.visit(it)
          case it: Sequence => v.visit(it)
          case it: Binop => v.visit(it)
          case it: Prefix => v.visit(it)
          case it: Apply => v.visit(it)
          case it: If => v.visit(it)
          case it: Case => v.visit(it)
          case it: Where => v.visit(it)
          case it: PartialFun => v.visit(it)
          case it: Function => v.visit(it)
          case it: Bra => v.visit(it)
          case it: Assoc => v.visit(it)
      }
  }

  implicit class DefinitionDispatch(val defn: Definition) extends AnyVal {
    def visit[T](v: DefinitionVisitor[T]):T =
      defn match {
          case it: Equation => v.visit(it)
          case it: WhereEquation => v.visit(it)
          case it: TypeDef => v.visit(it)
          case it: GuardedEquation => v.visit(it)
    }
  }
}


/**
 *  Some concrete examples of visitors.
 */
object Visitors {

  /** A "shallow" identity visit: the core of  more interesting transforms */
  trait Identity extends ExprVisitor[Expr] {
    type T = Expr
    def visit(it: Op): T = it
    def visit(it: Id): T = it
    def visit(it: Num): T = it
    def visit(it: Real): T = it
    def visit(it: Quote): T = it
    def visit(it: Tuple): T = it
    def visit(it: Binop): T = it
    def visit(it: Prefix): T = it
    def visit(it: Apply): T = it
    def visit(it: If): T = it
    def visit(it: Case): T = it
    def visit(it: Where): T = it
    def visit(it: PartialFun): T = it
    def visit(it: Function): T = it
    def visit(it: Bra): T = it
    def visit(it: Assoc): T = it
  }

  /**
   * An Expression visit that replaces "tupled" singleton expressions
   * with `Bra`
   */
  trait Detuple extends Identity { Detuple=>
    import Dispatch._
    override def visit(it: Tuple): Expr = {
      import it._
      exprs.length match {
        case 1 => Bra(exprs.head)
        case _ => Tuple(exprs.map(Detuple(_)))
      }
    }
    override def visit(it: Apply): Expr    =  { import it._; Apply(Detuple(l), Detuple(r)) }
    override def visit(it: Binop): Expr    =  { import it._; Binop(op, Detuple(l), Detuple(r)) }
    override def visit(it: Assoc): Expr    =  { import it._; Assoc(op, exprs.map(Detuple(_))) }
    override def visit(it: Sequence): Expr =  Sequence(it.exprs.map(Detuple(_)))
  }

  /**
   * A Definition Visitor that distributes an Expression.Visitor through all relevant forms
   * of definition
   */
  class Distribute(visitor: ExprVisitor[Expr]) extends DefinitionVisitor[Definition] { distribute =>
    import Dispatch._
    def visit(it: Equation): Definition = Equation(it.lhs.visit(visitor), it.rhs.visit(visitor))

    def visit(it: GuardedEquation): Definition =
      GuardedEquation(it.pattern,
                      it.cases.map{case GuardedResult(guard, expr) => GuardedResult(guard.visit(visitor), expr.visit(visitor))},
                      it.definitions.map(_.visit(distribute))
      )

    def visit(it: WhereEquation): Definition =
      WhereEquation(it.lhs.visit(visitor), it.rhs.visit(visitor), it.defs.map(_.visit(distribute)))

    def visit(it: TypeDef): Definition = it
  }

  object DetupleExpression extends Detuple
  object DetupleDefinition extends Distribute(DetupleExpression)

}

/**
 * Formatters defined as visitors.
 */
object Formatting {

  import org.sufrin.util.Layout
  import org.sufrin.util.Layout.Box._
  import visitor.Dispatch._

  object DefinitionFormat extends DefinitionVisitor[Layout.Box] {

    import Layout._



    def visit(it: Equation): Layout.Box = // Group(it.lhs.visit(ExprFormat) <+> text("=") + Nest(2, softNL + it.rhs.visit(ExprFormat)))
      (ExprFormat(it.lhs) <+> "=".text + (softNL + ExprFormat(it.rhs)).nest(2)).group

    def visit(it: WhereEquation): Layout.Box = {
      val body = it.defs.map(DefinitionFormat(_))
      ((ExprFormat(it.lhs) <+> "=".t <+> ExprFormat(it.rhs).group +
             (softNL + "where {".t + softNL + body.sep(";".t + softNL).nest(2) + softNL + "}".t).nest(2))).group
    }

    def visit(it: TypeDef): Layout.Box = text(it.source)

    def visit(it: GuardedEquation): Layout.Box = {
      text(it.source)
    }
  }

  object ExprFormat extends ExprVisitor[Layout.Box] {

    import Layout._

    def visit(it: Id): Layout.Box = text(it.source)

    def visit(it: Num): Layout.Box = text(it.source)

    def visit(it: Real): Layout.Box = text(it.source)

    def visit(it: Quote): Layout.Box = text(it.source)

    def visit(it: Tuple): Layout.Box = {
      val boxes = it.exprs.map(ExprFormat(_)) // (_.visit(ExprFormat))
      bracketed("(".t, boxes, ")".t)
    }

    def visit(it: Binop): Layout.Box =
      (ExprFormat(it.l) + space + text(it.op) + softNL + ExprFormat(it.r)).group
      // it.l.visit(ExprFormat) + space + text(it.op) + softNL + it.r.visit(ExprFormat)

    def visit(it: Prefix): Layout.Box =
      it.op.text <+> ExprFormat(it.e)

    def visit(it: Apply): Layout.Box =
      ExprFormat(it.l) <+> ExprFormat(it.r)

    def visit(it: Bra): Layout.Box =  // text("(") + Nest(2, softNL + it.e.visit(ExprFormat)) + softNL + text(")")
        "(".text + (softNL + ExprFormat(it.e)).nest(2) + softNL + ")".text

    def visit(it: Op): Layout.Box = text(it.source)

    def visit(it: Assoc): Layout.Box = {
      val boxes = it.exprs.map(ExprFormat(_))           //(_.visit(ExprFormat))
      text(it.op) <+> bracketed("(".t, boxes, ")".t)
    }

    def visit(it: If): Layout.Box = {
      it.source.text
    }

    def visit(it: Case): Layout.Box = {
      it.source.text
    }

    def visit(it: Where): Layout.Box = it.source.text // because these expressions are eliminated during parsing

    def visit(it: Sequence): Layout.Box = {
      val boxes = it.exprs.map(ExprFormat(_))           // (_.visit(ExprFormat))
      bracketed("[".t, boxes, "]".t)
    }

    def visit(it: PartialFun): Layout.Box = {
      val pat   = ExprFormat(it.pattern)
      val guard = it.guard match {
        case None => empty
        case Some(guard) => ("| ".t + ExprFormat(guard)).group
      }
      (pat <+> guard).group + softNL + "->".t  <+> (ExprFormat(it.expr).group)
    }

    def visit(it: Function): Layout.Box = {
      // val clauses = it.alts.map(_.visit(ExprFormat).group).sep(softNL + text("| "))
      val clauses = it.alts.map(ExprFormat(_).group).sep(softNL + text("| "))
      ("{ ".t + clauses + softNL + "}".t).group
    }
    
    /** 
     * An expression format, expressed as a box, ready for rendering 
     * at any width
     */
    case class ofExpr(e: Expr) {
      lazy val box = e.visit(ExprFormat)
      def apply(width: Int): String = box.render(width)
    }
  }
}

