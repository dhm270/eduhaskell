package eduhaskell

import eduhaskell.syntax._
import eduhaskell.errors._
import eduhaskell.Environment

object Evaluator {
    def withDefinition(env: Environment, defn: Definition): Environment = {
        defn match {
            case Equation(lhs, rhs) => {
                lhs match {
                    case Id(name) => {
                        lazy val newEnv: Environment = {
                            lazy val value = evaluate(newEnv, rhs)
                            env.lazyDefine(name, () => value)
                        }
                        newEnv
                    }
                    case Apply(l, r) => {
                        val exprs = flattenApply(lhs)
                        exprs(0) match {
                            case Id(funcName) => {
                                val asPartialFun = createPartialFun(lhs, rhs)
                                lazy val newEnv: Environment = {
                                    lazy val value = evaluate(newEnv, asPartialFun)
                                    env.lazyDefine(funcName, () => value)
                                }
                                newEnv
                            }
                        }
                    }
                }
            }
        }
    }

    def withDefinitions(env: Environment, defs: Seq[Definition]): Environment = {
        defs.foldLeft(env)((acc, curr) => withDefinition(acc, curr))
    }

    def getLeftmostArgument(e: Expr): Expr = {
        e match {
            case Apply(Apply(l1, l2), r) => getLeftmostArgument(Apply(l1, l2))
            case Apply(l, r) => l
        }
    }

    def createPartialFun(lhs: Expr, rhs: Expr): Expr = {
        def removeFirst(e: Expr): Expr = {
            e match {
                case Apply(Apply(l1, l2), r) => Apply(removeFirst(Apply(l1, l2)), r)
                case Apply(_, r) => r
            }
        }
        val editedLhs = removeFirst(lhs)
        PartialFun(editedLhs, None, rhs)
    }

    def flattenApply(e: Expr): List[Expr] = {
        e match {
            case Apply(l, r) => {
                flattenApply(l) ++ List(r)
            }
            case _ => List(e)
        }
    }

    def evaluate(env: Environment, e: Expr): Either[Error, Value] = {
        e match {
            case Op(name) => Left(NotImplemented())
            case Id(name) => env.find(name)

            case Num(value) => Right(IntValue(value))
            case Real(value) => Right(RealValue(value))
            case Quote(quote) => Right(StringValue(quote))

            case Tuple(exprs) => {
                if (exprs.length == 1) {
                    evaluate(env, exprs(0))
                } else {
                    // TODO
                    Left(NotImplemented())
                }
            }

            case Sequence(exprs) => evaluateSequence(env, exprs)
            case ArithmeticSequence(start, next, end) => evaluateArithmeticSequence(env, start, next, end)
            case DoubleDot() => Left(SyntaxError("`..` cannot appear here"))
            case SequenceComprehension(expr, qualifiers) => Left(NotImplemented())

            case Binop(op, l, r) => evaluateBinop(env, op, l, r)
            case Prefix(op, e) => evaluatePrefix(env, op, e)
            case Apply(l, r) => evaluateApply(env, l, r)
            case If(condition, trueExpr, falseExpr) => evaluateIf(env, condition, trueExpr, falseExpr)

            case Case(query, alts) => Left(NotImplemented())

            case Where(e, defs) => evaluate(withDefinitions(env, defs), e)

            case PartialFun(pattern, guard, expr) => {
                pattern match {
                    case Id(name) => {
                        def func(value: Value): Either[Error, Value] = {
                            evaluate(env.define(name, value), expr)
                        }
                        Right(FunctionValue(func))
                    }
                    case Apply(l, r) => {
                        val argument = getLeftmostArgument(pattern)
                        argument match {
                            case Id(name) => {
                                def func(value: Value): Either[Error, Value] = {
                                    evaluate(env.define(name, value), createPartialFun(pattern, expr))
                                }
                                Right(FunctionValue(func))
                            }
                        }
                    }
                }
            }
            case Function(alts) => {
                // TODO: do something about when there is more than one of these
                if (alts.length == 1) {
                    evaluate(env, alts(0))
                } else {
                    Left(NotImplemented())
                }
            }

            case Bra(e) => evaluate(env, e)
            case Assoc(op, exprs) => Left(NotImplemented())
        }
    }

    def evaluateSequence(env: Environment, exprs: Seq[Expr]): Either[Error, Value] = {
        if (exprs.exists(_.isInstanceOf[DoubleDot])) {
            if (exprs(0).isInstanceOf[DoubleDot]) return Left(SyntaxError("malformed arithmetic sequence"))
            val start = exprs(0)
            var next: Option[Expr] = None
            var end: Option[Expr] = None
            if (exprs(1).isInstanceOf[DoubleDot]) {
                if (exprs.length == 3) {
                    end = Some(exprs(2))
                } else if (exprs.length > 3) {
                    return Left(SyntaxError("malformed arithmetic sequence"))
                }
            } else {
                next = Some(exprs(1))
                if (!exprs(2).isInstanceOf[DoubleDot]) return Left(SyntaxError("malformed arithmetic sequence"))
                if (exprs.length == 4) {
                    end = Some(exprs(3))
                } else if (exprs.length > 4) {
                    return Left(SyntaxError("malformed arithmetic sequence"))
                }
            }
            evaluateArithmeticSequence(env, start, next, end)
        } else {
            var remaining = exprs.reverse
            var result: Value = NilValue()
            while (remaining.length > 0) {
                lazy val itemValue = evaluate(env, remaining(0))
                itemValue match {
                    case Right(v) => result = ConsValue(v, result)
                    case x => return x
                }
                remaining = remaining.drop(1)
            }
            Right(result)
        }
    }

    def generateIntArithmeticSequence(start: Long, step: Long, end: Option[Long]): Either[Error, Value] = {
        if (end.isDefined && start > end.get) {
            Right(NilValue())
        } else {
            lazy val restOfSequence = generateIntArithmeticSequence(start + step, step, end)
            for { r <- restOfSequence } yield ConsValue(IntValue(start), r)
        }
    }

    def generateRealArithmeticSequence(start: Double, step: Double, end: Option[Double]): Either[Error, Value] = {
        if (end.isDefined && start > end.get) {
            Right(NilValue())
        } else {
            lazy val restOfSequence = generateRealArithmeticSequence(start + step, step, end)
            for { r <- restOfSequence } yield ConsValue(RealValue(start), r)
        }
    }

    def evaluateArithmeticSequence(env: Environment, start: Expr, next: Option[Expr], end: Option[Expr]): Either[Error, Value] = {
        val startValue = evaluate(env, start)
        startValue match {
            case Right(s @ IntValue(int)) => {
                val start = int
                var step: Long = 1
                if (next.isDefined) {
                    val nextValue = evaluate(env, next.get)
                    nextValue match {
                        case Right(IntValue(next)) => step = next - start
                        case Right(r) => return Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, next is of type ${r.getType()}"))
                        case x => return x
                    }
                }
                var extractedEnd: Option[Long] = None
                if (end.isDefined) {
                    val endValue = evaluate(env, end.get)
                    endValue match {
                        case Right(IntValue(endInt)) => extractedEnd = Some(endInt)
                        case Right(r) => return Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, end is of type ${r.getType()}"))
                        case x => return x
                    }
                }
                generateIntArithmeticSequence(start, step, extractedEnd)
            }
            case Right(s @ RealValue(real)) => {
                val start = real
                var step: Double = 1.0
                if (next.isDefined) {
                    val nextValue = evaluate(env, next.get)
                    nextValue match {
                        case Right(RealValue(next)) => step = next - start
                        case Right(r) => return Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, next is of type ${r.getType()}"))
                        case x => return x
                    }
                }
                var extractedEnd: Option[Double] = None
                if (end.isDefined) {
                    val endValue = evaluate(env, end.get)
                    endValue match {
                        case Right(RealValue(endReal)) => extractedEnd = Some(endReal)
                        case Right(r) => return Left(TypeError(s"mismatched types in arithmetic sequence declaration: start is of type ${s.getType()}, end is of type ${r.getType()}"))
                        case x => return x
                    }
                }
                generateRealArithmeticSequence(start, step, extractedEnd)
            }
            case Right(s) => Left(TypeError(s"cannot declare an arithmetic sequence of type ${s.getType()}"))
            case x => x
        }
    }

    def evaluateBinop(env: Environment, op: String, l: Expr, r: Expr): Either[Error, Value] = {
        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)
        op match {
            case "+" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(IntValue(int1 + int2))
                            case Right(r) => Left(TypeError(s"operands of + have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(real1 + real2))
                            case Right(r) => Left(TypeError(s"operands of + have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(TypeError(s"lhs operand of + has type ${x.getType()}, which is illegal"))
                    case x => x
                }
            }
            case "-" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(IntValue(int1 - int2))
                            case Right(r) => Left(TypeError(s"operands of - have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(real1 - real2))
                            case Right(r) => Left(TypeError(s"operands of - have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(TypeError(s"lhs operand of - has type ${x.getType()}, which is illegal"))
                    case x => x
                }
            }
            case "*" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(IntValue(int1 * int2))
                            case Right(r) => Left(TypeError(s"operands of * have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(real1 * real2))
                            case Right(r) => Left(TypeError(s"operands of * have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(TypeError(s"lhs operand of * is of type ${x.getType()}, which is illegal"))
                    case x => x
                }
            }
            case "/" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => if (int2 == 0) { Left(DivisionByZero()) } else { Right(IntValue(int1 / int2)) }
                            case Right(r) => Left(TypeError(s"operands of / have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => if (real2 == 0) { Left(DivisionByZero()) } else { Right(RealValue(real1 / real2)) }
                            case Right(r) => Left(TypeError(s"operands of / have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(TypeError(s"lhs operand of / is of type ${x.getType()}, which is illegal"))
                    case x => x
                }
            }
            case "^" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(IntValue(Math.pow(int1, int2).intValue))
                            case Right(r) => Left(TypeError(s"operands of ^ have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(Math.pow(real1, real2)))
                            case Right(r) => Left(TypeError(s"operands of ^ have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(TypeError(s"lhs operand of ^ is of type ${x.getType()}, which is illegal"))
                    case x => x
                }
            }
            case ":" => {
                // TODO: some kind of type checking
                for {
                    l <- lValue
                    r <- rValue
                } yield ConsValue(l, r)
            }
            case "==" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 == int2))
                            case Right(r) => Left(TypeError(s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 == real2))
                            case Right(r) => Left(TypeError(s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ StringValue(quote1)) => {
                        rValue match {
                            case Right(StringValue(quote2)) => Right(BooleanValue(quote1.body == quote2.body))
                            case Right(r) => Left(TypeError(s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case "<" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 < int2))
                            case Right(r) => Left(TypeError(s"operands of < have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 < real2))
                            case Right(r) => Left(TypeError(s"operands of < have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case ">" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 > int2))
                            case Right(r) => Left(TypeError(s"operands of > have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 > real2))
                            case Right(r) => Left(TypeError(s"operands of > have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case "||" => {
                lValue match {
                    case Right(l @ BooleanValue(bool1)) => {
                        rValue match {
                            case Right(BooleanValue(bool2)) => Right(BooleanValue(bool1 || bool2))
                            case Right(r) => Left(TypeError(s"operands of || have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case "&&" => {
                lValue match {
                    case Right(l @ BooleanValue(bool1)) => {
                        rValue match {
                            case Right(BooleanValue(bool2)) => Right(BooleanValue(bool1 && bool2))
                            case Right(r) => Left(TypeError(s"operands of && have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
        }
    }

    def evaluatePrefix(env: Environment, op: String, e: Expr): Either[Error, Value] = {
        lazy val eValue = evaluate(env, e)
        op match {
            case "-" => {
                eValue match {
                    case Right(IntValue(int)) => Right(IntValue(-int))
                    case Right(RealValue(real)) => Right(RealValue(-real))
                    case Right(x) => Left(TypeError(s"cannot negate a value of type ${x.getType()}"))
                    case x => x
                }
            }
        }
    }

    def evaluateApply(env: Environment, l: Expr, r: Expr): Either[Error, Value] = {
        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)
        lValue match {
            case Right(FunctionValue(func)) => {
                rValue match {
                    case Right(r) => func(r)
                    case x => x
                }
            }
            case Right(x) => Left(TypeError(s"cannot call a value of type ${x.getType()}"))
            case x => x
        }
    }

    def evaluateIf(env: Environment, condition: Expr, trueExpr: Expr, falseExpr: Expr): Either[Error, Value] = {
        val conditionValue = evaluate(env, condition)
        conditionValue match {
            case Right(BooleanValue(bool)) => if (bool) { evaluate(env, trueExpr) } else { evaluate(env, falseExpr) }
            case Right(x) => Left(TypeError(s"if-statement condition is of type ${x.getType()}, but it should be ${BooleanValue(true).getType()}"))
            case x => x
        }
    }
}