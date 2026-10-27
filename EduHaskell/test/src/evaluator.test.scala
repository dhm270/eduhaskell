package eduhaskell

import org.sufrin.scalalr._
import org.sufrin.utility._
import LRParser._

import eduhaskell.Scanner._
import eduhaskell.syntax._
import eduhaskell.Evaluator

class EvaluatorTests extends munit.FunSuite {
    def parse(code: String): TREE = {
        val input = SourceTextCursor(code)
        val scanner = makeScanner(input)

        val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)

        parser.run(scanner.next).asInstanceOf[INSTRUCTED].instruction.asInstanceOf[TREE]
    }

    def parseAndEvaluate(code: String): Value = {
        parseAndEvaluateInEnvironment(Environment.empty, code)
    }

    def parseAndEvaluateInEnvironment(env: Environment, code: String): Value = {
        val parsed = parse(code)
        Evaluator.evaluate(env, parsed.asInstanceOf[Expr])
    }
    
    test("Evaluate 1") {
        val result = parseAndEvaluate("1")
        val expectedResult = IntValue(1)
        assert(result == expectedResult)
    }

    test("Evaluate 1+1") {
        val result = parseAndEvaluate("1+1")
        val expectedResult = IntValue(2)
        assert(result == expectedResult)
    }

    test("Evaluate 1+3*5") {
        val result = parseAndEvaluate("1+3*5")
        val expectedResult = IntValue(16)
        assert(result == expectedResult)
    }

    test("Evaluate variable defined in environment") {
        val env = Environment.empty.define("x", IntValue(3))
        val result = parseAndEvaluateInEnvironment(env, "x")
        val expectedResult = IntValue(3)
        assert(result == expectedResult)
    }

    test("Evaluate if-statement") {
        val result = parseAndEvaluate("if 3 < 4 then 3 else 4")
        val expectedResult = IntValue(3)
        assert(result == expectedResult)
    }

    test("Evaluate if-statement with non-empty environment") {
        val env = Environment.empty.define("x", IntValue(3))
        val result = parseAndEvaluateInEnvironment(env, "if x < 3 then 3 else 4")
        val expectedResult = IntValue(4)
        assert(result == expectedResult)
    }

    test("Evaluate let-in") {
        val result = parseAndEvaluate("let x = 3 in x")
        val expectedResult = IntValue(3)
        assert(result == expectedResult)
    }

    test("Evaluate let-in with function definition") {
        val result = parseAndEvaluate("let f x = x + 2 in f 3")
        val expectedResult = IntValue(5)
        assert(result == expectedResult)
    }

    test("Evaluate let-in recursive function definition") {
        val result = parseAndEvaluate("let fact n = if n == 1 then 1 else n * fact (n - 1) in fact 5")
        val expectedResult = IntValue(120)
        assert(result == expectedResult)
    }

    test("Evaluate `where` recursive function definition") {
        val result = parseAndEvaluate("fact 5 where fact n = if n == 1 then 1 else n * fact (n - 1)")
        val expectedResult = IntValue(120)
        assert(result == expectedResult)
    }
}