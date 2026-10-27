package eduhaskell

import eduhaskell.syntax._
import eduhaskell.Environment

object Evaluator {
    def withDefinition(env: Environment, defn: Definition): Environment = {
        defn match {
            case Equation(lhs, rhs) => {
                if (lhs.length == 1) {
                    lhs(0) match {
                        case Id(name) => env.define(name, evaluate(env, rhs))
                    }
                } else if (lhs.length > 1) {
                    lhs(0) match {
                        case Id(funcName) => {
                            val asLambda = PartialFun(lhs.drop(1), None, rhs)
                            lazy val lambdaValue = evaluate(env, asLambda)
                            env.define(funcName, lambdaValue)
                        }
                    }
                } else {
                    env
                }
            }
        }
    }

    def withDefinitions(env: Environment, defs: Seq[Definition]): Environment = {
        defs.foldLeft(env)((acc, curr) => withDefinition(acc, curr))
    }

    def evaluate(env: Environment, e: TREE): Value = {
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

            // case Sequence(exprs) => ...
            // case SequenceComprehension(expr, qualifiers) => ...

            case Binop(op, l, r) => evaluateBinop(env, op, l, r)
            case Prefix(op, e) => evaluatePrefix(env, op, e)
            case Apply(l, r) => evaluateApply(env, l, r)
            case If(condition, trueExpr, falseExpr) => evaluateIf(env, condition, trueExpr, falseExpr)

            // case Case(query, alts) => ...

            case Where(e, defs) => evaluate(withDefinitions(env, defs), e)

            case PartialFun(pattern, guard, expr) => {
                if (pattern.length == 1) {
                    pattern(0) match {
                        case Id(name) => {
                            def func(value: Value): Value = {
                                evaluate(env.define(name, value), expr)
                            }
                            FunctionValue(func)
                        }
                    }
                } else {
                    pattern(0) match {
                        case Id(name) => {
                            def func(value: Value): Value = {
                                evaluate(env.define(name, value), PartialFun(pattern.drop(1), guard, expr))
                            }
                            FunctionValue(func)
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