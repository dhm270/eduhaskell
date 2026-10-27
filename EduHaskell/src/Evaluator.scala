package eduhaskell

import eduhaskell.syntax._
import eduhaskell.Environment

object Evaluator {
    def withDefinition(env: Environment, defn: Definition): Environment = {
        defn match {
            case Equation(lhs, rhs) => {
                lhs match {
                    case Id(name) => {
                        lazy val value = evaluate(env, rhs)
                        env.define(name, value)
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

    def evaluate(env: Environment, e: Expr): Value = {
        e match {
            case Id(name) => env.find(name)

            case Num(value) => IntValue(value)
            case Real(value) => RealValue(value)
            case Quote(quote) => StringValue(quote)

            case Tuple(exprs) => {
                if (exprs.length == 1) {
                    evaluate(env, exprs(0))
                } else {
                    // TODO
                    BooleanValue(false)
                }
            }

            case Sequence(exprs) => {
                var remaining = exprs.reverse
                var result: Value = NilValue()
                while (remaining.length > 0) {
                    lazy val itemValue = evaluate(env, remaining(0))
                    result = ConsValue(itemValue, result)
                    remaining = remaining.drop(1)
                }
                result
            }
            case ArithmeticSequence(start, next, end) => evaluateArithmeticSequence(env, start, next, end)
            // case SequenceComprehension(expr, qualifiers) => ...

            case Binop(op, l, r) => evaluateBinop(env, op, l, r)
            case Prefix(op, e) => evaluatePrefix(env, op, e)
            case Apply(l, r) => evaluateApply(env, l, r)
            case If(condition, trueExpr, falseExpr) => evaluateIf(env, condition, trueExpr, falseExpr)

            // case Case(query, alts) => ...

            case Where(e, defs) => evaluate(withDefinitions(env, defs), e)

            case PartialFun(pattern, guard, expr) => {
                pattern match {
                    case Id(name) => {
                        def func(value: Value): Value = {
                            evaluate(env.define(name, value), expr)
                        }
                        FunctionValue(func)
                    }
                    case Apply(l, r) => {
                        val argument = getLeftmostArgument(pattern)
                        argument match {
                            case Id(name) => {
                                def func(value: Value): Value = {
                                    evaluate(env.define(name, value), createPartialFun(pattern, expr))
                                }
                                FunctionValue(func)
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
                    BooleanValue(false)
                }
            }

            case Bra(e) => evaluate(env, e)
        }
    }

    def generateIntArithmeticSequence(start: Long, step: Long, end: Option[Long]): Value = {
        if (end.isDefined && start > end.get) {
            NilValue()
        } else {
            lazy val restOfSequence = generateIntArithmeticSequence(start + step, step, end)
            ConsValue(IntValue(start), restOfSequence)
        }
    }

    def generateRealArithmeticSequence(start: Double, step: Double, end: Option[Double]): Value = {
        if (end.isDefined && start > end.get) {
            NilValue()
        } else {
            lazy val restOfSequence = generateRealArithmeticSequence(start + step, step, end)
            ConsValue(RealValue(start), restOfSequence)
        }
    }

    def evaluateArithmeticSequence(env: Environment, start: Expr, next: Option[Expr], end: Option[Expr]): Value = {
        val startValue = evaluate(env, start)
        startValue match {
            case IntValue(int) => {
                val start = int
                var step: Long = 1
                if (next.isDefined) {
                    val nextValue = evaluate(env, next.get)
                    nextValue match {
                        case IntValue(next) => step = next - start
                    }
                }
                var extractedEnd: Option[Long] = None
                if (end.isDefined) {
                    val endValue = evaluate(env, end.get)
                    endValue match {
                        case IntValue(endInt) => extractedEnd = Some(endInt)
                    }
                }
                generateIntArithmeticSequence(start, step, extractedEnd)
            }
            case RealValue(real) => {
                val start = real
                var step: Double = 1.0
                if (next.isDefined) {
                    val nextValue = evaluate(env, next.get)
                    nextValue match {
                        case RealValue(next) => step = next - start
                    }
                }
                var extractedEnd: Option[Double] = None
                if (end.isDefined) {
                    val endValue = evaluate(env, end.get)
                    endValue match {
                        case RealValue(endReal) => extractedEnd = Some(endReal)
                    }
                }
                generateRealArithmeticSequence(start, step, extractedEnd)
            }
        }
    }

    def evaluateBinop(env: Environment, op: String, l: Expr, r: Expr): Value = {
        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)
        op match {
            case "+" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => IntValue(int1 + int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => RealValue(real1 + real2)
                        }
                    }
                }
            }
            case "-" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => IntValue(int1 - int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => RealValue(real1 - real2)
                        }
                    }
                }
            }
            case "*" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => IntValue(int1 * int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => RealValue(real1 * real2)
                        }
                    }
                }
            }
            case "/" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => IntValue(int1 / int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => RealValue(real1 / real2)
                        }
                    }
                }
            }
            case "^" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => IntValue(Math.pow(int1, int2).intValue)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => RealValue(Math.pow(real1, real2))
                        }
                    }
                }
            }
            case ":" => {
                // TODO: some kind of type checking
                ConsValue(lValue, rValue)
            }
            case "==" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => BooleanValue(int1 == int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => BooleanValue(real1 == real2)
                        }
                    }
                    case StringValue(quote1) => {
                        rValue match {
                            case StringValue(quote2) => BooleanValue(quote1.body == quote2.body)
                        }
                    }
                }
            }
            case "<" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => BooleanValue(int1 < int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => BooleanValue(real1 < real2)
                        }
                    }
                }
            }
            case ">" => {
                lValue match {
                    case IntValue(int1) => {
                        rValue match {
                            case IntValue(int2) => BooleanValue(int1 > int2)
                        }
                    }
                    case RealValue(real1) => {
                        rValue match {
                            case RealValue(real2) => BooleanValue(real1 > real2)
                        }
                    }
                }
            }
            case "||" => {
                lValue match {
                    case BooleanValue(bool1) => {
                        rValue match {
                            case BooleanValue(bool2) => BooleanValue(bool1 || bool2)
                        }
                    }
                }
            }
            case "&&" => {
                lValue match {
                    case BooleanValue(bool1) => {
                        rValue match {
                            case BooleanValue(bool2) => BooleanValue(bool1 && bool2)
                        }
                    }
                }
            }
        }
    }

    def evaluatePrefix(env: Environment, op: String, e: Expr): Value = {
        lazy val eValue = evaluate(env, e)
        op match {
            case "-" => {
                eValue match {
                    case IntValue(int) => IntValue(-int)
                    case RealValue(real) => RealValue(-real)
                }
            }
        }
    }

    def evaluateApply(env: Environment, l: Expr, r: Expr): Value = {
        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)
        lValue match {
            case FunctionValue(func) => func(rValue)
        }
    }

    def evaluateIf(env: Environment, condition: Expr, trueExpr: Expr, falseExpr: Expr): Value = {
        val conditionValue = evaluate(env, condition)
        conditionValue match {
            case BooleanValue(bool) => if (bool) { evaluate(env, trueExpr) } else { evaluate(env, falseExpr) }
        }
    }
}