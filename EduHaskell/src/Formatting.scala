
  package eduhaskell
  import eduhaskell.syntax._
  import eduhaskell.visitor.Dispatch._
  import eduhaskell.visitor.{DefinitionVisitor, ExprVisitor}
  import org.sufrin.util.Layout.Box._

  /**
   * Formatters defined as visitors.
   */
  object Formatting {

    import org.sufrin.util.Layout
    import org.sufrin.util.Layout.Box._
    import visitor.Dispatch._

    object DefinitionFormat extends DefinitionVisitor[Layout.Box] {

      import Layout._



      def visit(it: Equation): Layout.Box = {// Group(it.lhs.visit(ExprFormat) <+> text("=") + Nest(2, softNL + it.rhs.visit(ExprFormat)))
        (ExprFormat(it.lhs) <+> "=".text + (softNL + ExprFormat(it.rhs)).nest(2)).group
      }

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

      def visit(it: SequenceComprehension): Layout.Box = {
        it.source.text
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