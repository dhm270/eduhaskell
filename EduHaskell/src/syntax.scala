package eduhaskell

object syntax {
    trait TREE {
        val source: String
        override lazy val toString: String = source
    }

    case class Quotation(opening: String, body: String, closing: String) {
        override lazy val toString: String = s"$opening$body$closing"
    }

    sealed trait Expr extends TREE {
        lazy val original: Expr = this
    }

    trait Named {
        val name: String
    }

    case class Op(name: String) extends Named with Expr {
        lazy val source = s"($name)"
        override lazy val original = Id(name)
    }

    case class Id(name: String) extends Named with Expr {
        lazy val source = name
    }

    case class Num(value: Long) extends Expr {
        lazy val source = value.toString
    }

    case class Real(value: Double) extends Expr {
        lazy val source = value.toString
    }

    case class Quote(quote: Quotation) extends Expr {
        lazy val source = quote.toString
    }

    case class Tuple(exprs: Seq[Expr]) extends Expr {
        lazy val source = exprs.mkString("(", ", ", ")")
    }

    case class Sequence(exprs: Seq[Expr]) extends Expr {
        lazy val source = exprs.mkString("[", ", ", "]")

        def containsDoubleDot(): Boolean = {
            this.exprs.exists(_.isInstanceOf[DoubleDot])
        }

        def toArithmeticSequence(): Option[ArithmeticSequence] = {
            if (exprs(0).isInstanceOf[DoubleDot]) return None
            val start = exprs(0)
            var next: Option[Expr] = None
            var end: Option[Expr] = None
            if (exprs(1).isInstanceOf[DoubleDot]) {
                if (exprs.length == 3) {
                    end = Some(exprs(2))
                } else if (exprs.length > 3) {
                    return None
                }
            } else {
                next = Some(exprs(1))
                if (!exprs(2).isInstanceOf[DoubleDot]) return None
                if (exprs.length == 4) {
                    end = Some(exprs(3))
                } else if (exprs.length > 4) {
                    return None
                }
            }
            Some(ArithmeticSequence(start, next, end))
        }
    }

    case class ArithmeticSequence(start: Expr, next: Option[Expr], end: Option[Expr]) extends Expr {
        lazy val source = s"[${start.source}${if (next.isDefined) s",${next.get.source}" else ""}..${if (end.isDefined) end.get.source else ""}]"
    }

    case class DoubleDot() extends Expr {
        lazy val source = ".."
    }

    case class SequenceComprehension(expr: Expr, qualifiers: Seq[SequenceComprehensionQualifier]) extends Expr {
        lazy val source = s"[${expr.source} | ${qualifiers.map(_.source).mkString(", ")}]"
    }

    trait SequenceComprehensionQualifier {
        val source: String
    }
    case class SequenceComprehensionGenerator(pattern: Expr, expr: Expr) extends SequenceComprehensionQualifier {
        lazy val source = s"${pattern.source} <- ${expr.source}"
    }
    case class SequenceComprehensionDefinition(defn: Definition) extends SequenceComprehensionQualifier {
        lazy val source = s"let ${defn.source}"
    }
    case class SequenceComprehensionGuard(expr: Expr) extends SequenceComprehensionQualifier {
        lazy val source = expr.source
    }

    case class Binop(op: String, l: Expr, r: Expr) extends Expr {
        lazy val source = s"${l.source} $op ${r.source}"
    }

    case class Prefix(op: String, e: Expr) extends Expr {
        lazy val source = s"$op ${e.source}"
    }

    case class Apply(l: Expr, r: Expr) extends Expr {
        lazy val source = s"${l.source} ${r.source}"
    }

    case class If(condition: Expr, trueExpr: Expr, falseExpr: Expr) extends Expr {
        lazy val source = s"if ${condition.source} then ${trueExpr.source} else ${falseExpr.source}"
    }

    case class Case(query: Expr, alts: Seq[CaseArm]) extends Expr {
        lazy val source = s"case ${query.source} of { ${alts.map(_.source).mkString("; ")} }"
    }

    case class CaseArm(pattern: Expr, result: Expr) {
        lazy val source = s"${pattern.source} -> ${result.source}"
    }

    case class Where(e: Expr, defs: Seq[Definition]) extends Expr {
        lazy val source = s"${e.source} where ${ defs.map(_.source).mkString("; ") }"
    }

    case class PartialFun(pattern: Expr, guard: Option[Expr], expr: Expr) extends Expr {
        private lazy val guardSource = if (guard.isDefined) s" | ${guard.get} " else ""
        lazy val source = s"${pattern.source}$guardSource->${expr.source}"
    }

    case class Function(alts: Seq[PartialFun]) extends Expr {
        lazy val source = s"${alts.map(_.source).mkString(" | ") }"
    }

    case class FunctionWithClosures(alts: Seq[(Environment, Seq[String], PartialFun)]) extends Expr {
        lazy val source = s"${alts.map(_._3.source).mkString(" | ")}"
    }

    case class Bra(e: Expr) extends Expr {
        lazy val source = s"(${e.source})"
        override lazy val original: Expr = Tuple(List(e))
    }


    sealed trait Definition extends TREE

    case class Equation(lhs: Expr, rhs: Expr) extends Definition {
        lazy val source = s"${lhs.source} = $rhs"
    }

    case class GuardedEquation(pattern: Expr, cases: Seq[GuardedResult], definitions: Seq[Definition]) extends Definition {
        lazy val body = s"${pattern.source} ${cases.map(_.toString).mkString(" | ", " | ", "")}"
        lazy val wheres = definitions.map(_.source) match {
            case Nil => ""
            case clauses => clauses.mkString(" where {", ";", "}")
        }
        lazy val source = body + wheres
    }

    case class GuardedResult(guard: Expr, expr: Expr) {
        override def toString: String = s"$guard = $expr"
    }

    case class TypeDef(symbols: Seq[Expr], rhs: Expr) extends Definition {
        lazy val source = s"${symbols.mkString(", ")} :: $rhs"
    }

    case class WhereEquation(lhs: Expr, rhs: Expr, defs: Seq[Definition]) extends Definition {
        lazy val source = s"${lhs.source} = ${rhs.source} where ${defs.map(_.source).mkString("; ")}"
    }

    def getLeftmostArgument(e: Expr): Expr = {
        e match {
            case Apply(l @ Apply(_, _), r) => getLeftmostArgument(l)
            case Apply(l, r) => l
        }
    }

    def removeLeftmostArgument(e: Expr): Expr = {
        e match {
            case Apply(l @ Apply(_, _), r) => Apply(removeLeftmostArgument(l), r)
            case Apply(_, r) => r
        }
    }

    def createPartialFun(lhs: Expr, guard: Option[Expr], rhs: Expr): PartialFun = {
        val editedLhs = removeLeftmostArgument(lhs)
        PartialFun(editedLhs, guard, rhs)
    }
}

