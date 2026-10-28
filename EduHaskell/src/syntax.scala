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
    }

    case class ArithmeticSequence(start: Expr, next: Option[Expr], end: Option[Expr]) extends Expr {
        lazy val source = s"[${start.source}${if (next.isDefined) s",${next.get.source}" else ""}..${if (end.isDefined) end.get.source else ""}]"
    }

    case class DoubleDot() extends Expr {
        lazy val source = ".."
    }

    case class SequenceComprehension(expr: Expr, qualifiers: Seq[(Expr, Expr)]) extends Expr {
        lazy val source = s"[${expr.source} | ${qualifiers.map({ case (a, b) => a.source + " <- " + b.source }).mkString(", ")}]"
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

    case class Case(query: Expr, alts: Seq[(Expr, Expr)]) extends Expr {
        lazy val source = s"case ${query.source} of { ${alts.map({ case (a, b) => a.source + " -> "  + b.source }).mkString("; ")} }"
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

    case class Bra(e: Expr) extends Expr {
        lazy val source = s"(${e.source})"
        override lazy val original: Expr = Tuple(List(e))
    }

    case class Assoc(op: String, exprs: Seq[Expr]) extends Expr {
        def mkBin(op: String)(l: Expr, r: Expr): Expr = Binop(op, l, r)
        lazy val source = original.source
        override lazy val original = exprs.reduceLeft(mkBin(op))
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
}

