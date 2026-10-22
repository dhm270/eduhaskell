package eduhaskell

import org.sufrin.scalalr._
import org.sufrin.utility._
import LRParser._

import eduhaskell.Scanner._
import eduhaskell.syntax._

class ParserTests extends munit.FunSuite {
    def parse(code: String): TREE = {
        val input = SourceTextCursor(code)
        val scanner = makeScanner(input)

        val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
        
        parser.run(scanner.next).asInstanceOf[INSTRUCTED].instruction.asInstanceOf[TREE]
    }

    def structureMatches(a: TREE, b: TREE): Boolean = {
        a match {
            case Op(name) => {
                b match {
                    case Op(name1) => name == name1
                    case _ => false
                }
            }
            case Id(name) => {
                b match {
                    case Id(name1) => name == name1
                    case _ => false
                }
            }
            case Num(value) => {
                b match {
                    case Num(value1) => value == value1
                    case _ => false
                }
            }
            case Real(value) => {
                b match {
                    case Real(value1) => value == value1
                    case _ => false
                }
            }
            case Quote(quote) => {
                b match {
                    case Quote(quote1) => quote == quote1
                    case _ => false
                }
            }
            case Tuple(exprs) => {
                b match {
                    case Tuple(exprs1) => exprs.zip(exprs1).forall({ case (a, b) => structureMatches(a, b) })
                    case _ => false
                }
            }
            case Sequence(exprs) => {
                b match {
                    case Sequence(exprs1) => exprs.zip(exprs1).forall({ case (a, b) => structureMatches(a, b) })
                    case _ => false
                }
            }
            case SequenceComprehension(expr, qualifiers) => {
                b match {
                    case SequenceComprehension(expr1, qualifiers1) => {
                        if (!structureMatches(expr, expr1)) {
                            false
                        } else {
                            qualifiers.zip(qualifiers1).forall({ case ((a1, b1), (a2, b2)) => structureMatches(a1, a2) && structureMatches(b1, b2) })
                        }
                    }
                    case _ => false
                }
            }
            case Binop(op, l, r) => {
                b match {
                    case Binop(op1, l1, r1) => op == op1 && structureMatches(l, l1) && structureMatches(r, r1)
                    case _ => false
                }
            }
            case Prefix(op, e) => {
                b match {
                    case Prefix(op1, e1) => op == op1 && structureMatches(e, e1)
                    case _ => false
                }
            }
            case Apply(l, r) => {
                b match {
                    case Apply(l1, r1) => structureMatches(l, l1) && structureMatches(r, r1)
                    case _ => false
                }
            }
            case If(condition, trueExpr, falseExpr) => {
                b match {
                    case If (condition1, trueExpr1, falseExpr1) => structureMatches(condition, condition1) && structureMatches(trueExpr, trueExpr1) && structureMatches(falseExpr, falseExpr1)
                    case _ => false
                }
            }
            case Case(query, alts) => {
                b match {
                    case Case(query1, alts1) => {
                        if (!structureMatches(query, query1)) {
                            false
                        } else {
                            alts.zip(alts1).forall({ case ((a1, b1), (a2, b2)) => structureMatches(a1, a2) && structureMatches(b1, b2) })
                        }
                    }
                    case _ => false
                }
            }
            case Where(e, defs) => {
                b match {
                    case Where(e1, defs1) => {
                        structureMatches(e, e1) && defs.zip(defs1).forall({ case (a, b) => structureMatches(a, b) })
                    }
                    case _ => false
                }
            }
            case PartialFun(pattern, guard, expr) => {
                b match {
                    case PartialFun(pattern1, guard1, expr1) => {
                        if (!structureMatches(pattern, pattern1)) {
                            false
                        } else if (!structureMatches(expr, expr1)) {
                            false
                        } else if (guard == None) {
                            guard1 == None
                        } else {
                            structureMatches(guard.get, guard1.get)
                        }
                    }
                    case _ => false
                }
            }
            case Function(alts) => {
                b match {
                    case Function(alts1) => alts.zip(alts1).forall({ case (a, b) => structureMatches(a, b) })
                    case _ => false
                }
            }
            case Bra(e) => {
                b match {
                    case Bra(e1) => structureMatches(e, e1)
                    case _ => false
                }
            }
            case Assoc(op, exprs) => {
                b match {
                    case Assoc(op1, exprs1) => op == op1 && exprs.zip(exprs1).forall({ case (a, b) => structureMatches(a, b)} )
                    case _ => false
                }
            }

            case Equation(lhs, rhs) => {
                b match {
                    case Equation(lhs1, rhs1) => structureMatches(lhs, lhs1) && structureMatches(rhs, rhs1)
                    case _ => false
                }
            }
            case GuardedEquation(pattern, cases, definitions) => {
                b match {
                    case GuardedEquation(pattern1, cases1, definitions1) => {
                        if (!structureMatches(pattern, pattern1)) {
                            false
                        } else if (!definitions.zip(definitions1).forall({ case (a, b) => structureMatches(a, b) })) {
                            false
                        } else {
                            cases.zip(cases1).forall({
                                case (GuardedResult(guard, expr), GuardedResult(guard1, expr1)) => structureMatches(guard, guard1) && structureMatches(expr, expr1)
                                case _ => false
                            })
                        }
                    }
                    case _ => false
                }
            }
            case TypeDef(symbols, rhs) => {
                b match {
                    case TypeDef(symbols1, rhs1) => structureMatches(rhs, rhs1) && symbols.zip(symbols1).forall({ case (a, b) => structureMatches(a, b) })
                    case _ => false
                }
            }
            case WhereEquation(lhs, rhs, defs) => {
                b match {
                    case WhereEquation(lhs1, rhs1, defs1) => structureMatches(lhs, lhs1) && structureMatches(rhs, rhs1) && defs.zip(defs1).forall({ case (a, b) => structureMatches(a, b) })
                    case _ => false
                }
            }

            case _ => false
        }
    }

    test("Parse 1+1") {
        val result = parse("1 + 1")
        val expectedResult = Binop("+", Num(1), Num(1))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse 1+3*5") {
        val result = parse("1 + 3 * 5")
        val expectedResult = Binop("+", Num(1), Binop("*", Num(3), Num(5)))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f x = x + 3") {
        val result = parse("f x = x + 3")
        val expectedResult = Equation(Apply(Id("f"), Id("x")), Binop("+", Id("x"), Num(3)))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f 3") {
        val result = parse("f 3")
        val expectedResult = Apply(Id("f"), Num(3))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse function definition with where clause") {
        val result = parse("f x = g x where { g y = y * 2 }")
        val expectedResult = WhereEquation(
            Apply(Id("f"), Id("x")),
            Apply(Id("g"), Id("x")),
            List(
                Equation(Apply(Id("g"), Id("y")), Binop("*", Id("y"), Num(2)))
            )
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse lambda application") {
        val result = parse("(\\x -> x * 2) 3")
        val expectedResult = Apply(
            Tuple(List(
                Function(List(
                    PartialFun(Id("x"), None, Binop("*", Id("x"), Num(2)))
                ))
            )),
            Num(3)
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse full-line comment") {
        val result = parse(
            """-- Comment
            1 + 1"""
        )
        val expectedResult = Binop("+", Num(1), Num(1))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse inline comment") {
        val result = parse("1 + 1 -- Inline comment")
        val expectedResult = Binop("+", Num(1), Num(1))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse multi-line comment") {
        val result = parse(
            """{-
            Multi-line comment
            -}
            1 + 1"""
        )
        val expectedResult = Binop("+", Num(1), Num(1))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse if-then-else") {
        val result = parse("if x < 3 then x else 3")
        val expectedResult = If(
            Binop("<", Id("x"), Num(3)),
            Id("x"),
            Num(3)
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse let-in") {
        val result = parse("let x = 3 in x + 2")
        val expectedResult = Where(
            Binop("+", Id("x"), Num(2)),
            List(Equation(Id("x"), Num(3)))
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse case statement with one case") {
        val result = parse("case x of 3 -> 4")
        val expectedResult = Case(
            Id("x"),
            List((Num(3), Num(4)))
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse case statement with multiple cases") {
        val result = parse("case x of { 3 -> 4; 5 -> 6 }")
        val expectedResult = Case(
            Id("x"),
            List(
                (Num(3), Num(4)),
                (Num(5), Num(6))
            )
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse list comprehension") {
        val result = parse("[x | x <- y, y <- z]")
        val expectedResult = SequenceComprehension(
            Id("x"),
            List(
                (Id("x"), Id("y")),
                (Id("y"), Id("z"))
            )
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f :: a") {
        val result = parse("f :: a")
        val expectedResult = TypeDef(List(Id("f")), Id("a"))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f :: a -> b") {
        val result = parse("f :: a -> b")
        val expectedResult = TypeDef(List(Id("f")), Binop("->", Id("a"), Id("b")))
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f :: (a -> b) -> c") {
        val result = parse("f :: (a -> b) -> c")
        val expectedResult = TypeDef(
            List(Id("f")),
            Binop("->", Tuple(List(Binop("->", Id("a"), Id("b")))), Id("c"))
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f :: [a]") {
        val result = parse("f :: [a]")
        val expectedResult = TypeDef(
            List(Id("f")),
            Sequence(List(Id("a")))
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f :: [a -> b]") {
        val result = parse("f :: [a -> b]")
        val expectedResult = TypeDef(
            List(Id("f")),
            Sequence(List(
                Binop("->", Id("a"), Id("b"))
            ))
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse f, g :: [a]") {
        val result = parse("f, g :: [a]")
        val expectedResult = TypeDef(
            List(Id("f"), Id("g")),
            Sequence(List(Id("a")))
        )
        assert(structureMatches(result, expectedResult))
    }
}