package eduhaskell

import eduhaskell.syntax._
import eduhaskell.Environment

object Evaluator {
    def evaluate(env: Environment, e: TREE): Value = {
        e match {
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

            case Binop(op, l, r) => evaluateBinop(env, op, l, r)
            case Prefix(op, e) => evaluatePrefix(env, op, e)
            case If(condition, trueExpr, falseExpr) => evaluateIf(env, condition, trueExpr, falseExpr)

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

    def evaluateIf(env: Environment, condition: Expr, trueExpr: Expr, falseExpr: Expr): Value = {
        val conditionValue = evaluate(env, condition)
        conditionValue match {
            case BooleanValue(bool) => if (bool) { evaluate(env, trueExpr) } else { evaluate(env, falseExpr) }
        }
    }
}