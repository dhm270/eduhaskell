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
                // TODO
                true
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
                // TODO
                true
            }
            case Where(e, defs) => {
                // TODO
                true
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
            [
                Equation(Apply(Id("g"), Id("y")), Binop("*", Id("y"), Num(2)))
            ]
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Parse lambda application") {
        val result = parse("(\\x -> x * 2) 3")
        val expectedResult = Apply(
            Bra(
                Function([
                    PartialFun(Id("x"), None, Binop("*", Id("x"), Num(2)))
                ])
            ),
            Num(3)
        )
        assert(structureMatches(result, expectedResult))
    }

    test("Full-line comment") {
        val result = parse(
            """-- Comment
            1 + 1"""
        )
        val expectedResult = Binop("+", Num(1), Num(1))
        assert(structureMatches(result, expectedResult))
    }

    test("Inline comment") {
        val result = parse("1 + 1 -- Inline comment")
        val expectedResult = Binop("+", Num(1), Num(1))
        assert(structureMatches(result, expectedResult))
    }
}