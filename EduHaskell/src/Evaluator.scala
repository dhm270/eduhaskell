package eduhaskell

import eduhaskell.syntax._
import eduhaskell.errors._
import eduhaskell.values._
import eduhaskell.Environment

object Evaluator {
    def matchAndExtendEnvironment(env: Environment, conflictingNames: Seq[String], pattern: Expr, comparison: LazyValue): Either[Error, (Environment, Seq[String])] = {
        // If we're able to successfully pattern match, then we return Right((newEnv, names)),
        // where newEnv is the potentially extended environment (containing any
        // necessary new identifier definitions), and names is a list of the new names
        // we have just defined (so we can keep track of them, for conflictingNames).
        var pat = pattern
        pat match {
            case Tuple(exprs) => {
                if (exprs.length == 1) {
                    pat = Bra(exprs(0))
                }
            }
            case _ => {}
        }
        pat match {
            case Id(name) => {
                if (conflictingNames.contains(name)) {
                    Left(ConflictingDefinitionsError(List(name)))
                } else {
                    Right((env.define(name, comparison), List(name)))
                }
            }

            case Num(value) => {
                val got = comparison.get()
                evaluate(got) match {
                    case Right(IntValue(int)) => {
                        if (int == value) {
                            Right((env, List()))
                        } else {
                            Left(PatternMatchError())
                        }
                    }
                    case _ => Left(PatternMatchError())
                }
            }

            case Real(value) => {
                val got = comparison.get()
                evaluate(got) match {
                    case Right(RealValue(real)) => {
                        if (real == value) {
                            Right((env, List()))
                        } else {
                            Left(PatternMatchError())
                        }
                    }
                    case _ => Left(PatternMatchError())
                }
            }

            case Quote(value) => {
                val got = comparison.get()
                evaluate(got) match {
                    case Right(CharValue(c)) => {
                        if (value.opening == "'" && value.body == c.toString) {
                            Right((env, List()))
                        } else {
                            Left(PatternMatchError())
                        }
                    }
                    case Right(c @ ConsValue(_, _)) => {
                        var str = ""
                        var pointer: Value = c
                        var extracting = true
                        var valid = true
                        while (extracting && valid) {
                            pointer match {
                                case NilValue() => extracting = false
                                case ConsValue(l, r) => {
                                    val charGot = l.get()
                                    evaluate(charGot) match {
                                        case Right(CharValue(c)) => str += c
                                        case _ => valid = false
                                    }
                                    evaluate(r.get()) match {
                                        case Right(x) => pointer = x
                                        case _ => valid = false
                                    }
                                }
                            }
                        }
                        if (valid) {
                            if (value.body == str) {
                                Right((env, List()))
                            } else {
                                Left(PatternMatchError())
                            }
                        } else {
                            Left(PatternMatchError())
                        }
                    }
                    case _ => Left(PatternMatchError())
                }
            }

            case Tuple(patternExprs) => {
                val got = comparison.get()
                evaluate(got) match {
                    case Right(TupleValue(lazyVals)) => {
                        if (patternExprs.length == lazyVals.length) {
                            def f(acc: Either[Error, (Environment, Seq[String])], curr: (Expr, LazyValue)): Either[Error, (Environment, Seq[String])] = {
                                curr match {
                                    case (e, l) => {
                                        acc match {
                                            case Right((env, names)) => {
                                                val result = matchAndExtendEnvironment(env, conflictingNames ++ names, e, l)
                                                result match {
                                                    case Right((env1, names1)) => {
                                                        Right((env1, names ++ names1))
                                                    }
                                                    case x => x
                                                }
                                            }
                                            case x => x
                                        }
                                    }
                                    case _ => Left(PatternMatchError())
                                }
                            }
                            val x: Either[Error, (Environment, Seq[String])] = Right((env, List()))
                            patternExprs.zip(lazyVals).foldLeft(x)(f)
                        } else {
                            Left(PatternMatchError())
                        }
                    }
                    case _ => Left(PatternMatchError())
                }
            }

            case Sequence(patternExprs) => {
                val got = comparison.get()
                if (patternExprs.length == 0) {
                    evaluate(got) match {
                        case Right(NilValue()) => Right((env, List()))
                        case _ => Left(PatternMatchError())
                    }
                } else {
                    matchAndExtendEnvironment(env, conflictingNames, Binop(":", patternExprs(0), Sequence(patternExprs.drop(1))), comparison)
                }
            }

            case Binop(":", l, r) => {
                val got = comparison.get()
                evaluate(got) match {
                    case Right(ConsValue(head, tail)) => {
                        val resultAfterHead = matchAndExtendEnvironment(env, conflictingNames, l, head)
                        resultAfterHead match {
                            case Right((envAfterHead, namesAfterHead)) => {
                                val resultAfterTail = matchAndExtendEnvironment(envAfterHead, conflictingNames ++ namesAfterHead, r, tail)
                                resultAfterTail match {
                                    case Right((env, namesAfterTail)) => Right((env, namesAfterHead ++ namesAfterTail))
                                    case x => x
                                }
                            }
                            case x => x
                        }
                    }
                    case _ => Left(PatternMatchError())
                }
            }

            case Bra(e) => matchAndExtendEnvironment(env, conflictingNames, e, comparison)

            case _ => Left(PatternMatchError())
        }
    }

    def patternMatch(env: Environment, conflictingNames: Seq[String], pattern: Expr, guard: Option[Expr], comparison: LazyValue, expr: Expr): EvaluateContinuation = {
        val (_, _, result) = patternMatchAndReturnEnvironment(env, conflictingNames, pattern, guard, comparison, expr)
        result
    }

    def patternMatchAndReturnEnvironment(env: Environment, conflictingNames: Seq[String], pattern: Expr, guard: Option[Expr], comparison: LazyValue, expr: Expr): (Environment, Seq[String], EvaluateContinuation) = {
        // Second argument in the result is the new names which we bound while doing this pattern match.

        // If the pattern is multiple things, then we convert it.
        // Instead, we want to think of this as a function that takes only
        // a single parameter and, if it is defined to have "multiple parameters",
        // then we return a partial function.
        var pat = pattern
        var e = expr
        pat match {
            case Apply(l, r) => {
                val argument = getLeftmostArgument(pat)
                val partialFun = createPartialFun(pat, guard, e)
                pat = argument
                e = partialFun
            }
            case _ => {}
        }
        
        val matchResult: Either[Error, (Environment, Seq[String])] = matchAndExtendEnvironment(env, conflictingNames, pat, comparison)
        matchResult match {
            case Right((newEnv, newNames)) => {
                var guardMet = true
                if (guard.isDefined) {
                    val guardResult = evaluate(newEnv, guard.get)
                    guardResult match {
                        case Right(BooleanValue(true)) => guardMet = true
                        case _ => guardMet = false
                    }
                }
                if (guardMet) {
                    e match {
                        case p: PartialFun => e = FunctionWithClosures(List((newEnv, conflictingNames ++ newNames, p)))
                        case _ => {}
                    }
                    (newEnv, newNames, Closure(newEnv, e))
                } else {
                    (env, List(), Result(Left(PatternMatchError())))
                }
            }
            case x => {
                (env, List(), Result(x.asInstanceOf[Either[Error, Value]]))
            }
        }
    }

    def evaluate(env: Environment, e: Expr): Either[Error, Value] = {
        evaluate(Closure(env, e))
    }

    def evaluate(cont: EvaluateContinuation): Either[Error, Value] = {
        // If `cont` is `Result` then we short-circuit and return immediately.
        cont match {
            case Result(r) => return r
            case _ => {}
        }

        // Try and use iteration rather than recursion where possible
        // to avoid stack overflow.
        val closure = cont.asInstanceOf[Closure]
        var env = closure.env
        var e = closure.e
        var result: Option[Either[Error, Value]] = None

        def handleCont(cont: EvaluateContinuation): Unit = {
            cont match {
                case Closure(env1, e1) => {
                    env = env1
                    e = e1
                }
                case Result(r) => {
                    result = Some(r)
                }
            }
        }

        while (!result.isDefined) {
            e match {
                case Op(name) => return Left(NotImplemented())
                case Id(name) => handleCont(
                    env.find(name).get()
                )

                case Num(value) => return Right(IntValue(value))
                case Real(value) => return Right(RealValue(value))
                case Quote(quote) => return quoteToValue(quote)

                case Tuple(exprs) => {
                    if (exprs.length == 1) {
                        e = exprs(0)
                    } else {
                        def evalAsLazyVal(e: Expr): LazyValue = {
                            LazyValue(() => Closure(env, e))
                        }
                        return Right(TupleValue(exprs.map(evalAsLazyVal(_))))
                    }
                }

                case Sequence(exprs) => handleCont(
                    evaluateSequence(env, exprs)
                )
                case ArithmeticSequence(start, next, end) => handleCont(
                    evaluateArithmeticSequence(env, start, next, end)
                )
                case DoubleDot() => return Left(SyntaxError("`..` cannot appear here"))
                case SequenceComprehension(expr, qualifiers) => handleCont(
                    evaluateSequenceComprehension(env, List(), expr, qualifiers)
                )

                case Binop(op, l, r) => handleCont(
                    evaluateBinop(env, op, l, r)
                )
                case Prefix(op, expr) => handleCont(
                    evaluatePrefix(env, op, expr)
                )
                case Apply(l, r) => handleCont(
                    evaluateApply(env, l, r)
                )
                case If(condition, trueExpr, falseExpr) => handleCont(
                    evaluateIf(env, condition, trueExpr, falseExpr)
                )
                case Case(query, alts) => handleCont(
                    evaluateCase(env, query, alts)
                )

                case Where(expr, defs) => {
                    val extendedEnvironment = env.withDefinitions(defs)
                    extendedEnvironment match {
                        case Right((newEnv, names)) => {
                            env = newEnv
                            e = expr
                        }
                        case x => return x.asInstanceOf[Either[Error, Value]]
                    }
                }

                case PartialFun(pattern, guard, expr) => {
                    def func(lazyValue: LazyValue): EvaluateContinuation = {
                        patternMatch(env, List(), pattern, guard, lazyValue, expr)
                    }
                    return Right(FunctionValue(func))
                }
                case Function(alts) => handleCont(
                    evaluateFunction(env, alts)
                )
                case FunctionWithClosures(closureAlts) => handleCont(
                    evaluateFunctionWithClosures(closureAlts)
                )

                case Bra(expr) => e = expr
            }
        }
        result.get
    }

    def stringToStringValue(str: String): Value = {
        if (str == "") {
            NilValue()
        } else {
            ConsValue(
                LazyValue(() => Result(Right(CharValue(str(0))))),
                LazyValue(() => Result(Right(stringToStringValue(str.drop(1)))))
            )
        }
    }

    def quoteToValue(quote: Quotation): Either[Error, Value] = {
        if (quote.opening != quote.closing) {
            Left(SyntaxError("mismatched single and double quotes"))
        } else if (quote.opening == "\"") {
            Right(stringToStringValue(quote.body))
        } else if (quote.body.length > 1) {
            Left(SyntaxError("single quotes define a Character type, so must only contain a single character"))
        } else {
            Right(CharValue(quote.body(0)))
        }
    }

    def evaluateSequence(env: Environment, exprs: Seq[Expr]): EvaluateContinuation = {
        if (exprs.exists(_.isInstanceOf[DoubleDot])) {
            if (exprs(0).isInstanceOf[DoubleDot]) return Result(Left(SyntaxError("malformed arithmetic sequence")))
            val start = exprs(0)
            var next: Option[Expr] = None
            var end: Option[Expr] = None
            if (exprs(1).isInstanceOf[DoubleDot]) {
                if (exprs.length == 3) {
                    end = Some(exprs(2))
                } else if (exprs.length > 3) {
                    return Result(Left(SyntaxError("malformed arithmetic sequence")))
                }
            } else {
                next = Some(exprs(1))
                if (!exprs(2).isInstanceOf[DoubleDot]) return Result(Left(SyntaxError("malformed arithmetic sequence")))
                if (exprs.length == 4) {
                    end = Some(exprs(3))
                } else if (exprs.length > 4) {
                    return Result(Left(SyntaxError("malformed arithmetic sequence")))
                }
            }
            evaluateArithmeticSequence(env, start, next, end)
        } else if (exprs.length == 0) {
            Result(Right(NilValue()))
        } else {
            val head = exprs(0)
            val tail = exprs.drop(1)
            Closure(env, Binop(":", head, Sequence(tail)))
        }
    }

    def generateIntArithmeticSequence(start: Long, step: Long, end: Option[Long]): Either[Error, Value] = {
        if (end.isDefined && ((step >= 0 && start > end.get) || (step < 0 && start < end.get))) {
            Right(NilValue())
        } else {
            lazy val restOfSequence = generateIntArithmeticSequence(start + step, step, end)
            Right(ConsValue(
                LazyValue(() => Result(Right(IntValue(start)))),
                LazyValue(() => Result(restOfSequence))
            ))
        }
    }

    def generateRealArithmeticSequence(start: Double, step: Double, end: Option[Double]): Either[Error, Value] = {
        if (end.isDefined && ((step >= 0 && start > end.get) || (step < 0 && start < end.get))) {
            Right(NilValue())
        } else {
            lazy val restOfSequence = generateRealArithmeticSequence(start + step, step, end)
            Right(ConsValue(
                LazyValue(() => Result(Right(RealValue(start)))), LazyValue(() => Result(restOfSequence))
            ))
        }
    }

    def evaluateArithmeticSequence(env: Environment, start: Expr, next: Option[Expr], end: Option[Expr]): EvaluateContinuation = {
        val startValue = evaluate(env, start)
        startValue match {
            case Right(s @ IntValue(int)) => {
                val start = int
                var step: Long = 1
                if (next.isDefined) {
                    val nextValue = evaluate(env, next.get)
                    nextValue match {
                        case Right(IntValue(next)) => step = next - start
                        case Right(r) => return Result(Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, next is of type ${r.getType()}")))
                        case x => return Result(x)
                    }
                }
                var extractedEnd: Option[Long] = None
                if (end.isDefined) {
                    val endValue = evaluate(env, end.get)
                    endValue match {
                        case Right(IntValue(endInt)) => extractedEnd = Some(endInt)
                        case Right(r) => return Result(Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, end is of type ${r.getType()}")))
                        case x => return Result(x)
                    }
                }
                Result(generateIntArithmeticSequence(start, step, extractedEnd))
            }
            case Right(s @ RealValue(real)) => {
                val start = real
                var step: Double = 1.0
                if (next.isDefined) {
                    val nextValue = evaluate(env, next.get)
                    nextValue match {
                        case Right(RealValue(next)) => step = next - start
                        case Right(r) => return Result(Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, next is of type ${r.getType()}")))
                        case x => return Result(x)
                    }
                }
                var extractedEnd: Option[Double] = None
                if (end.isDefined) {
                    val endValue = evaluate(env, end.get)
                    endValue match {
                        case Right(RealValue(endReal)) => extractedEnd = Some(endReal)
                        case Right(r) => return Result(Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, end is of type ${r.getType()}")))
                        case x => return Result(x)
                    }
                }
                Result(generateRealArithmeticSequence(start, step, extractedEnd))
            }
            case Right(s) => Result(Left(TypeError(s"cannot declare an arithmetic sequence of type ${s.getType()}")))
            case x => Result(x)
        }
    }

    def evaluateSequenceComprehension(env: Environment, conflictingNames: Seq[String], expr: Expr, qualifiers: Seq[SequenceComprehensionQualifier]): EvaluateContinuation = {
        // Recursively enumerate all items of the list.
        // That means we're OK with receiving no qualifiers (might be from a recursive call).
        if (qualifiers.length == 0) {
            lazy val cont = Closure(env, expr)
            return Result(Right(ConsValue(
                LazyValue(() => cont),
                LazyValue(() => Result(Right(NilValue())))
            )))
        }

        // Otherwise, we need to do something with the first qualifier.
        val qualifierInQuestion = qualifiers(0)
        val remainingQualifiers = qualifiers.drop(1)
        
        qualifierInQuestion match {
            case SequenceComprehensionGenerator(pattern, generatorExpr) => {
                def generateOver(generator: Either[Error, Value]): Either[Error, Value] = {
                    generator match {
                        case Right(ConsValue(lazyHead, lazyTail)) => {
                            val maybeExtendedEnvironment = matchAndExtendEnvironment(env, conflictingNames, pattern, lazyHead)
                            maybeExtendedEnvironment match {
                                case Right((extendedEnvironment, newNames)) => {
                                    val seqForThisValue = evaluateSequenceComprehension(extendedEnvironment, conflictingNames ++ newNames, expr, remainingQualifiers)
                                    def concatWithNextSeq(seqForThisValue: Either[Error, Value]): Either[Error, Value] = {
                                        seqForThisValue match {
                                            case Right(ConsValue(h, t)) => {
                                                val next = LazyValue(
                                                    () => Result(
                                                        concatWithNextSeq(evaluate(t.get()))
                                                    )
                                                )
                                                Right(ConsValue(h, next))
                                            }
                                            case Right(NilValue()) => generateOver(evaluate(lazyTail.get()))
                                            case Left(_) => seqForThisValue
                                        }
                                    }
                                    seqForThisValue match {
                                        case Result(s) => concatWithNextSeq(s)
                                    }
                                }
                                case x => x.asInstanceOf[Either[Error, Value]]
                            }
                        }
                        case Right(NilValue()) => Right(NilValue())
                        case Right(x) => Left(TypeError(s"cannot qualify a sequence comprehension using an expression of type ${x.getType()}"))
                        case x => x
                    }
                }
                Result(generateOver(evaluate(env, generatorExpr)))
            }
            case SequenceComprehensionDefinition(defn) => {
                val extendedEnvironment = env.withDefinition(defn)
                extendedEnvironment match {
                    case Right((newEnv, names)) => {
                        if (conflictingNames.intersect(names).length == 0) {
                            evaluateSequenceComprehension(newEnv, conflictingNames ++ names, expr, remainingQualifiers)
                        } else {
                            Result(Left(ConflictingDefinitionsError(conflictingNames.intersect(names))))
                        }
                    }
                    case x => Result(x.asInstanceOf[Either[Error, Value]])
                }
            }
            case SequenceComprehensionGuard(guard) => {
                val conditionValue = evaluate(env, guard)
                conditionValue match {
                    case Right(BooleanValue(bool)) => {
                        if (bool) {
                            evaluateSequenceComprehension(env, conflictingNames, expr, remainingQualifiers)
                        } else {
                            Result(Right(NilValue()))
                        }
                    }
                    case Left(_) => Result(conditionValue)
                }
            }
        }
    }

    def evaluateBinop(env: Environment, op: String, l: Expr, r: Expr): EvaluateContinuation = {
        def ok(value: Value): EvaluateContinuation = {
            Result(Right(value))
        }
        def bad(err: Error): EvaluateContinuation = {
            Result(Left(err))
        }

        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)

        // First check if we're dealing with an operator where we can do short-circuiting,
        // i.e. we don't necessarily need to evaluate both operands.
        op match {
            case "||" => {
                lValue match {
                    case Right(BooleanValue(false)) => {
                        rValue match {
                            case r @ Right(BooleanValue(_)) => return Result(r)
                            case Left(_) => return Result(rValue)
                        }
                    }
                    case Right(BooleanValue(true)) => return ok(BooleanValue(true))
                    case Left(_) => return Result(lValue)
                }
            }
            case "&&" => {
                lValue match {
                    case Right(BooleanValue(true)) => {
                        rValue match {
                            case r @ Right(BooleanValue(_)) => return Result(r)
                            case Left(_) => return Result(rValue)
                        }
                    }
                    case Right(BooleanValue(false)) => return ok(BooleanValue(false))
                    case Left(_) => return Result(lValue)
                }
            }
            case _ => {}
        }

        // For anything else, we certainly need to evaluate both operands.
        // Therefore, we might as well check at this point whether there's an
        // error we need to propagate. Saves us doing it separately in each other case.

        lValue match {
            case Right(_) => {}
            case Left(_) => return Result(lValue)
        }
        rValue match {
            case Right(_) => {}
            case Left(_) => return Result(rValue)
        }

        op match {
            case "+" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(IntValue(int1 + int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(RealValue(real1 + real2))
                        }
                    }
                }
            }
            case "-" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(IntValue(int1 - int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(RealValue(real1 - real2))
                        }
                    }
                }
            }
            case "*" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(IntValue(int1 * int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(RealValue(real1 * real2))
                        }
                    }
                }
            }
            case "/" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => if (int2 == 0) { bad(DivisionByZero()) } else { ok(IntValue(int1 / int2)) }
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => if (real2 == 0) { bad(DivisionByZero()) } else { ok(RealValue(real1 / real2)) }
                        }
                    }
                }
            }
            case "^" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(IntValue(Math.pow(int1, int2).intValue))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(RealValue(Math.pow(real1, real2)))
                        }
                    }
                }
            }
            case ":" => {
                ok(ConsValue(LazyValue(() => Result(lValue)), LazyValue(() => Result(rValue))))
            }
            case "==" => valuesEqual(lValue, rValue)
            case "<" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(BooleanValue(int1 < int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(BooleanValue(real1 < real2))
                        }
                    }
                }
            }
            case "<=" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(BooleanValue(int1 <= int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(BooleanValue(real1 <= real2))
                        }
                    }
                }
            }
            case ">=" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(BooleanValue(int1 >= int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(BooleanValue(real1 >= real2))
                        }
                    }
                }
            }
            case ">" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => ok(BooleanValue(int1 > int2))
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => ok(BooleanValue(real1 > real2))
                        }
                    }
                }
            }
        }
    }

    def valuesEqual(lValue: Either[Error, Value], rValue: Either[Error, Value]): EvaluateContinuation = {
        def ok(value: Value): EvaluateContinuation = {
            Result(Right(value))
        }
        def bad(err: Error): EvaluateContinuation = {
            Result(Left(err))
        }
        lValue match {
            case Right(l @ IntValue(int1)) => {
                rValue match {
                    case Right(IntValue(int2)) => ok(BooleanValue(int1 == int2))
                }
            }
            case Right(l @ RealValue(real1)) => {
                rValue match {
                    case Right(RealValue(real2)) => ok(BooleanValue(real1 == real2))
                }
            }
            case Right(l @ CharValue(char1)) => {
                rValue match {
                    case Right(CharValue(char2)) => ok(BooleanValue(char1 == char2))
                }
            }
            case Right(l @ BooleanValue(bool1)) => {
                rValue match {
                    case Right(BooleanValue(bool2)) => ok(BooleanValue(bool1 == bool2))
                }
            }
            case Right(l @ FunctionValue(_)) => {
                bad(TypeError("functions cannot be compared"))
            }
            case Right(l @ ConsValue(head1, tail1)) => {
                rValue match {
                    case Right(ConsValue(head2, tail2)) => {
                        val headEqualityResult = valuesEqual(evaluate(head1.get()), evaluate(head2.get()))
                        evaluate(headEqualityResult) match {
                            case Right(BooleanValue(true)) => valuesEqual(evaluate(tail1.get()), evaluate(tail2.get()))
                            case x => Result(x)
                        }
                    }
                    case Right(NilValue()) => ok(BooleanValue(false))
                }
            }
            case Right(l @ NilValue()) => {
                rValue match {
                    case Right(NilValue()) => ok(BooleanValue(true))
                    case Right(ConsValue(_, _)) => ok(BooleanValue(false))
                }
            }
            case Right(l @ TupleValue(vals1)) => {
                rValue match {
                    case Right(r @ TupleValue(vals2)) => {
                        def f(acc: EvaluateContinuation, curr: (LazyValue, LazyValue)): EvaluateContinuation = {
                            evaluate(acc) match {
                                case Right(BooleanValue(true)) => {
                                    curr match {
                                        case (l, r) => {
                                            val lValue = evaluate(l.get())
                                            val rValue = evaluate(r.get())
                                            valuesEqual(lValue, rValue)
                                        }
                                        case _ => ok(BooleanValue(false))
                                    }
                                }
                                case _ => acc
                            }
                        }
                        val x: EvaluateContinuation = ok(BooleanValue(true))
                        vals1.zip(vals2).foldLeft(x)(f)
                    }
                }
            }
        }
    }

    def evaluatePrefix(env: Environment, op: String, e: Expr): EvaluateContinuation = {
        lazy val eValue = evaluate(env, e)
        op match {
            case "-" => {
                eValue match {
                    case Right(IntValue(int)) => Result(Right(IntValue(-int)))
                    case Right(RealValue(real)) => Result(Right(RealValue(-real)))
                    case Right(x) => Result(Left(TypeError(s"cannot negate a value of type ${x.getType()}")))
                    case x => Result(x)
                }
            }
        }
    }

    def evaluateApply(env: Environment, l: Expr, r: Expr): EvaluateContinuation = {
        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)
        lValue match {
            case Right(FunctionValue(func)) => {
                def f(): EvaluateContinuation = {
                    Closure(env, r)
                }
                func(LazyValue(f))
            }
            case Right(x) => Result(Left(TypeError(s"cannot call a value of type ${x.getType()}")))
            case x => Result(x)
        }
    }

    def evaluateIf(env: Environment, condition: Expr, trueExpr: Expr, falseExpr: Expr): EvaluateContinuation = {
        val conditionValue = evaluate(env, condition)
        conditionValue match {
            case Right(BooleanValue(bool)) => {
                if (bool) {
                    Closure(env, trueExpr)
                } else {
                    Closure(env, falseExpr)
                }
            }
            case Right(x) => Result(Left(TypeError(s"if-statement condition is of type ${x.getType()}, but it should be ${BooleanValue(true).getType()}")))
            case x => Result(x)
        }
    }

    def evaluateCase(env: Environment, query: Expr, alts: Seq[CaseArm]): EvaluateContinuation = {
        def altToPartialFun(alt: CaseArm): PartialFun = {
            PartialFun(alt.pattern, None, alt.result)
        }
        val partialFuns = alts.map(altToPartialFun)
        evaluateApply(env, Function(partialFuns), query)
    }

    def evaluateFunction(env: Environment, alts: Seq[PartialFun]): EvaluateContinuation = {
        val modifiedAlts = alts.map(alt => (env, List(), alt))
        evaluateFunctionWithClosures(modifiedAlts)
    }

    def evaluateFunctionWithClosures(alts: Seq[(Environment, Seq[String], PartialFun)]): EvaluateContinuation = {
        if (alts.length == 0) return Result(Left(PatternMatchError()))
        def func(lazyValue: LazyValue): EvaluateContinuation = {
            alts(0) match {
                case (env, _, PartialFun(Apply(_, _), _, _)) => {
                    // i.e. there is another argument after this one, so we will certainly
                    // return another function rather than an actual value
                    def f(acc: Seq[(Environment, Seq[String], PartialFun)], curr: (Environment, Seq[String], PartialFun)): Seq[(Environment, Seq[String], PartialFun)] = {
                        // Find all possible pattern matches
                        curr match {
                            case (env, names, PartialFun(pattern, guard, expr)) => {
                                val (newEnv, newNames, result) = patternMatchAndReturnEnvironment(env, names, pattern, guard, lazyValue, expr)
                                result match {
                                    case Result(Left(PatternMatchError())) => acc
                                    case _ => {
                                        // Remove the first argument from the partial function, since we've already evaluated it
                                        val editedPartialFun = createPartialFun(pattern, guard, expr)
                                        acc ++ List((newEnv, names ++ newNames, editedPartialFun.asInstanceOf[PartialFun]))
                                    }
                                }
                            }
                        }
                    }
                    val x: Seq[(Environment, Seq[String], PartialFun)] = List()
                    val matches = alts.foldLeft(x)(f)
                    // In the line below, `env` is a dummy value.
                    // The actual clause in `evaluate` doesn't use it.
                    Closure(env, FunctionWithClosures(matches))
                }
                case (_, _, _) => {
                    // this is the last argument, so we will return a non-function
                    def f(acc: EvaluateContinuation, curr: (Environment, Seq[String], PartialFun)): EvaluateContinuation = {
                        evaluate(acc) match {
                            case Right(_) => acc // we have already successfully pattern matched
                            case Left(PatternMatchError()) => {
                                // i.e. we haven't successfully matched yet, so we keep going
                                curr match {
                                    case (env, names, PartialFun(pattern, guard, expr)) => {
                                        patternMatch(env, names, pattern, guard, lazyValue, expr)
                                    }
                                }
                            }
                            case Left(_) => acc
                        }
                    }
                    val x: EvaluateContinuation = Result(Left(PatternMatchError()))
                    alts.foldLeft(x)(f)
                }
            }
        }
        Result(Right(FunctionValue(func)))
    }
}