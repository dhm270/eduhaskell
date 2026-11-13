package eduhaskell

import eduhaskell.syntax._
import eduhaskell.errors._

trait Type {
    val source: String
}

case class IntegerType() extends Type {
    val source = "Integer"
}

case class RealType() extends Type {
    val source = "Real"
}

case class CharacterType() extends Type {
    val source = "Character"
}

case class BooleanType() extends Type {
    val source = "Boolean"
}

case class TupleType(subtypes: Seq[Type]) extends Type {
    val source = subtypes.map(_.source).mkString("(", ", ", ")")
}

case class SequenceType(subtype: Type) extends Type {
    val source = s"[${subtype.source}]"
}

case class FunctionType(argument: Type, output: Type) extends Type {
    val source = s"${argument.source} -> ${output.source}"
}

object TypeChecker {
    def getFirstError(types: Seq[Either[Error, Type]]): Either[Error, Type] = {
        def f(acc: Either[Error, Type], curr: Either[Error, Type]): Either[Error, Type] = {
            acc match {
                case Left(_) => acc
                case Right(_) => {
                    curr match {
                        case Left(_) => curr
                        case Right(_) => acc
                    }
                }
            }
        }
        val x: Either[Error, Type] = Right(BooleanType())
        types.foldLeft(x)(f)
    }

    def ifNoTypeErrors(unextractedTypes: Seq[Either[Error, Type]], callback: Seq[Type] => Either[Error, Type]): Either[Error, Type] = {
        val firstTypeError = getFirstError(unextractedTypes)
        firstTypeError match {
            case Right(_) => {
                // i.e. no type errors were found
                def extract(x: Either[Error, Type]): Type = {
                    x match {
                        case Right(_type) => _type
                    }
                }
                val extractedTypes = unextractedTypes.map(extract)
                callback(extractedTypes)
            }
            case Left(_) => firstTypeError
        }
    }

    def getType(env: Environment, e: Expr): Either[Error, Type] = {
        e match {
            case Op(name) => Left(NotImplemented())
            case Id(name) => Left(NotImplemented())

            case Num(_) => Right(IntegerType())
            case Real(_) => Right(RealType())
            case Quote(quote) => {
                if (quote.opening == "'") {
                    Right(CharacterType())
                } else {
                    Right(SequenceType(CharacterType()))
                }
            }
            case Tuple(exprs) => {
                if (exprs.length == 1) {
                    getType(env, exprs(0))
                } else {
                    val subtypes = exprs.map(expr => getType(env, expr))
                    ifNoTypeErrors(subtypes, subtypesExtracted => Right(TupleType(subtypesExtracted)))
                }
            }
            case Sequence(exprs) => {
                val subtypes = exprs.map(expr => getType(env, expr))
                ifNoTypeErrors(subtypes, subtypesExtracted => {
                    if (subtypesExtracted.length == 1) {
                        Right(SequenceType(subtypesExtracted(0)))
                    } else {
                        val comparisonType = subtypesExtracted(0)
                        val otherTypes = subtypesExtracted.drop(1)
                        val allEqual = otherTypes.forall(_ == comparisonType)
                        if (allEqual) {
                            Right(SequenceType(comparisonType))
                        } else {
                            Left(TypeError("elements of a sequence must all be of the same type"))
                        }
                    }
                })
            }
            case ArithmeticSequence(start, next, end) => {
                val startType = getType(env, start)
                startType match {
                    case Left(_) => return startType
                    case Right(_) => {}
                }
                if (next.isDefined) {
                    val nextType = getType(env, next.get)
                    nextType match {
                        case Left(_) => return nextType
                        case Right(_) => {}
                    }
                    if (nextType != startType) return Left(TypeError("elements of a sequence must all be of the same type"))
                }
                if (end.isDefined) {
                    val endType = getType(env, end.get)
                    endType match {
                        case Left(_) => return endType
                        case Right(_) => {}
                    }
                    if (endType != startType) return Left(TypeError("elements of a sequence must all be of the same type"))
                }
                startType
            }
            case SequenceComprehension(expr, qualifiers) => Left(NotImplemented())

            case Binop(op, l, r) => getTypeOfBinop(env, op, l, r)
            case Prefix(op, e) => getTypeOfPrefix(env, op, e)
            case Apply(l, r) => getTypeOfApply(env, l, r)
            case If(condition, trueExpr, falseExpr) => {
                val conditionType = getType(env, condition)
                conditionType match {
                    case Right(BooleanType()) => {
                        val trueType = getType(env, trueExpr)
                        val falseType = getType(env, falseExpr)
                        ifNoTypeErrors(List(trueType, falseType), extractedTypes => {
                            if (extractedTypes(0) == extractedTypes(1)) {
                                Right(extractedTypes(0))
                            } else {
                                Left(TypeError("both arms of an if-statement must be of the same type"))
                            }
                        })
                    }
                    case Right(_) => {
                        Left(TypeError(s"condition of an if-statement must be of type ${BooleanType().source}"))
                    }
                    case Left(_) => conditionType
                }
            }

            case Case(query, alts) => Left(NotImplemented())

            case Where(expr, defs) => Left(NotImplemented())

            case PartialFun(pattern, guard, expr) => Left(NotImplemented())
            case Function(alts) => {
                val altTypes = alts.map(alt => getType(env, alt))
                if (altTypes.length == 1) {
                    altTypes(0)
                } else {
                    ifNoTypeErrors(altTypes, altTypesExtracted => {
                        val comparisonType = altTypesExtracted(0)
                        val otherTypes = altTypesExtracted.drop(1)
                        val allEqual = otherTypes.forall(_ == comparisonType)
                        if (allEqual) {
                            Right(comparisonType)
                        } else {
                            Left(TypeError("all possible paths in a given function must be of the same type"))
                        }
                    })
                }
            }
            case FunctionWithClosures(closureAlts) => {
                val altTypes = closureAlts.map({ case (closureEnv, _, alt) => getType(closureEnv, alt) })
                if (altTypes.length == 1) {
                    altTypes(0)
                } else {
                    ifNoTypeErrors(altTypes, altTypesExtracted => {
                        val comparisonType = altTypesExtracted(0)
                        val otherTypes = altTypesExtracted.drop(1)
                        val allEqual = otherTypes.forall(_ == comparisonType)
                        if (allEqual) {
                            Right(comparisonType)
                        } else {
                            Left(TypeError("all possible paths in a given function must be of the same type"))
                        }
                    })
                }
            }
            case Bra(e) => getType(env, e)
            case Assoc(op, exprs) => Left(NotImplemented())
            
            case x => Left(TypeError(s"cannot get the type of an expression of type ${x.getClass()}"))
        }
    }

    def getTypeOfBinop(env: Environment, op: String, l: Expr, r: Expr): Either[Error, Type] = {
        val lType = getType(env, l)
        val rType = getType(env, r)
        def e(): Either[Error, Type] = {
            var lSource = ""
            var rSource = ""
            lType match {
                case Right(t) => lSource = t.source
                case x => return x
            }
            rType match {
                case Right(t) => rSource = t.source
                case x => return x
            }
            Left(TypeError(
                s"operands of ${op} have mismatched types: lhs is of type ${lSource}, rhs is of type ${rSource}"
            ))
        }
        def e1(): Either[Error, Type] = {
            var lSource = ""
            var rSource = ""
            lType match {
                case Right(t) => lSource = t.source
                case x => return x
            }
            rType match {
                case Right(t) => rSource = t.source
                case x => return x
            }
            Left(TypeError(
                s"${op} cannot be applied to values of type ${lSource} and ${rSource}"
            ))
        }
        op match {
            case "+" | "-" | "*" | "/" | "^" => {
                lType match {
                    case Right(IntegerType()) => {
                        rType match {
                            case Right(IntegerType()) => Right(IntegerType())
                            case Right(r) => e()
                            case x => x
                        }
                    }
                    case Right(RealType()) => {
                        rType match {
                            case Right(RealType()) => Right(RealType())
                            case Right(r) => e()
                            case x => x
                        }
                    }
                    case Right(x) => e1()
                    case x => x
                }
            }
            case ":" => {
                rType match {
                    case Right(SequenceType(subtype)) => {
                        if (lType == Right(subtype)) {
                            Right(SequenceType(subtype))
                        } else {
                            e1()
                        }
                    }
                    case _ => e1()
                }
            }
            case "==" => {
                if (lType == rType) {
                    Right(BooleanType())
                } else {
                    e()
                }
            }
            case "<" | "<=" | ">=" | ">" => {
                lType match {
                    case Right(IntegerType()) => {
                        rType match {
                            case Right(IntegerType()) => Right(BooleanType())
                            case Right(r) => e()
                            case x => x
                        }
                    }
                    case Right(RealType()) => {
                        rType match {
                            case Right(RealType()) => Right(BooleanType())
                            case Right(r) => e()
                            case x => x
                        }
                    }
                    case Right(_) => e1()
                    case x => x
                }
            }
            case "||" | "&&" => {
                lType match {
                    case Right(BooleanType()) => {
                        rType match {
                            case Right(BooleanType()) => Right(BooleanType())
                            case Right(_) => e1()
                            case x => x
                        }
                    }
                    case Right(_) => e1()
                    case x => x
                }
            }
        }
    }

    def getTypeOfPrefix(env: Environment, op: String, e: Expr): Either[Error, Type] = {
        val eType = getType(env, e)
        op match {
            case "-" => {
                eType match {
                    case Right(IntegerType()) => Right(IntegerType())
                    case Right(RealType()) => Right(RealType())
                    case Right(x) => Left(TypeError(s"cannot negate a value of type ${x.source}"))
                }
            }
        }
    }
    
    def getTypeOfApply(env: Environment, l: Expr, r: Expr): Either[Error, Type] = {
        val lType = getType(env, l)
        val rType = getType(env, r)
        lType match {
            case Right(f @ FunctionType(argument, output)) => {
                rType match {
                    case Right(t) => {
                        if (argument == t) {
                            Right(output)
                        } else {
                            Left(TypeError(s"cannot call a function of type ${f.source} with an argument of type ${t.source}"))
                        }
                    }
                    case x => x
                }
            }
            case Right(t) => {
                Left(TypeError(s"cannot call an expression of type ${t.source}"))
            }
            case x => x
        }
    }

    def unify(a: Type, b: Type): Either[Error, Type] = {
        Left(NotImplemented())
    }
}