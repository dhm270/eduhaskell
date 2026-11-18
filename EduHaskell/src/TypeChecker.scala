package eduhaskell

import eduhaskell.syntax._
import eduhaskell.errors._
import eduhaskell.types._

object TypeChecker {
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
                    ifNoTypeErrors[Type](subtypes, subtypesExtracted => Right(TupleType(subtypesExtracted)))
                }
            }
            case Sequence(exprs) => {
                val subtypes = exprs.map(expr => getType(env, expr))
                ifNoTypeErrors[Type](subtypes, subtypesExtracted => {
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
                        ifNoTypeErrors[Type](List(trueType, falseType), extractedTypes => {
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
                    ifNoTypeErrors[Type](altTypes, altTypesExtracted => {
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
                    ifNoTypeErrors[Type](altTypes, altTypesExtracted => {
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
                        val unificationResult = unify(argument, t)
                        unificationResult match {
                            case Right((unifiedType, subst)) => Right(output.substitute(subst))
                            case Left(_) => Left(TypeError(s"cannot call a function of type ${f.source} with an argument of type ${t.source}"))
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

    def typesEqual(a: Type, b: Type): Boolean = {
        a match {
            case IntegerType() | RealType() | CharacterType() | BooleanType() | GenericType(_) => a == b
            case TupleType(subtypes) => {
                b match {
                    case TupleType(subtypes1) => {
                        subtypes.zip(subtypes1).forall({ case (a, b) => typesEqual(a, b) })
                    }
                    case _ => false
                }
            }
            case SequenceType(subtype) => {
                b match {
                    case SequenceType(subtype1) => typesEqual(subtype, subtype1)
                    case _ => false
                }
            }
            case FunctionType(argument, output) => {
                b match {
                    case FunctionType(argument1, output1) => {
                        typesEqual(argument, argument1) && typesEqual(output, output1)
                    }
                    case _ => false
                }
            }
        }
    }

    def getTypeVars(t: Type): Seq[String] = {
        t match {
            case IntegerType() | RealType() | CharacterType() | BooleanType() => List()
            case TupleType(subtypes) => subtypes.flatMap(getTypeVars)
            case SequenceType(subtype) => getTypeVars(subtype)
            case FunctionType(argument, output) => {
                getTypeVars(argument) ++ getTypeVars(output)
            }
            case GenericType(name) => List(name)
        }
    }

    def unionSubstitutions(substA: Substitution, substB: Substitution): Substitution = {
        // TODO: is this definitely correct?
        substA ++ substB
    }

    def unify(inputA: Type, inputB: Type): Either[Error, (Type, Substitution)] = {
        if (typesEqual(inputA, inputB)) return Right((inputA, List()))
        
        // If successful, returns the unified type and also the substitution map used,
        // if applicable.
        var a = inputA
        var b = inputB
        b match {
            case GenericType(_) => {
                a = inputB
                b = inputA
            }
            case _ => {}
        }
        def e(): Either[Error, (Type, Substitution)] = {
            Left(TypeError(s"cannot unify types ${a.source} and ${b.source}"))
        }
        a match {
            case IntegerType() | RealType() | CharacterType() | BooleanType() => {
                if (a == b) {
                    Right(a, List())
                } else {
                    e()
                }
            }
            case TupleType(subtypesA) => {
                b match {
                    case TupleType(subtypesB) => {
                        if (subtypesA.length != subtypesB.length) return Left(TypeError(s"${a.source} and ${b.source} are tuple types of different arities, so are not unifiable"))

                        var subst: Substitution = List()
                        def addToSubst(subst1: Substitution): Unit = {
                            subst = unionSubstitutions(subst, subst1)
                        }
                        val unifiedSubtypes = subtypesA.zip(subtypesB).map({ case (a, b) => {
                            val result = unify(a.substitute(subst), b.substitute(subst))
                            result match {
                                case Left(_) => {}
                                case Right((r, subst1)) => addToSubst(subst1)
                            }
                            result
                        }})
                        ifNoTypeErrors[(Type, Substitution)](
                            unifiedSubtypes,
                            unifiedExtracted => {
                                Right((TupleType(unifiedExtracted.map(_._1)), subst))
                            }
                        )
                    }
                    case _ => e()
                }
            }
            case SequenceType(subtypeA) => {
                b match {
                    case SequenceType(subtypeB) => {
                        val unificationResult = unify(subtypeA, subtypeB)
                        ifNoTypeErrors[(Type, Substitution)](
                            List(unificationResult),
                            unifiedExtracted => Right((SequenceType(unifiedExtracted(0)._1)), unifiedExtracted(0)._2)
                        )
                    }
                    case _ => e()
                }
            }
            case FunctionType(argumentA, outputA) => {
                b match {
                    case FunctionType(argumentB, outputB) => {
                        // If one of the arguments is `GenericType`, then the necessary substitution is simple.
                        def f(generic: Type, other: Type): Either[Error, (Type, Substitution)] = {
                            val map = List((generic.asInstanceOf[GenericType].name, other))
                            val substOutputA = outputA.substitute(map)
                            val substOutputB = outputB.substitute(map)
                            val unificationResult = unify(substOutputA, substOutputB)
                            ifNoTypeErrors[(Type, Substitution)](
                                List(unificationResult),
                                unifiedExtracted => {
                                    val (unifiedType, subst) = unifiedExtracted(0)
                                    Right((FunctionType(other.substitute(subst), unifiedType)), unionSubstitutions(map, subst))
                                }
                            )
                        }
                        if (argumentA.isInstanceOf[GenericType]) {
                            f(argumentA, argumentB)
                        } else if (argumentB.isInstanceOf[GenericType]) {
                            f(argumentB, argumentA)
                        } else {
                            val argumentUnificationResult = unify(argumentA, argumentB)
                            ifNoTypeErrors[(Type, Substitution)](
                                List(argumentUnificationResult),
                                unifiedArgumentExtracted => {
                                    val subst1 = unifiedArgumentExtracted(0)._2
                                    val outputUnificationResult = unify(outputA.substitute(subst1), outputB.substitute(subst1))
                                    ifNoTypeErrors[(Type, Substitution)](
                                        List(outputUnificationResult),
                                        unifiedOutputExtracted => {
                                            val subst2 = unifiedOutputExtracted(0)._2
                                            Right((
                                                FunctionType(unifiedArgumentExtracted(0)._1.substitute(subst2), unifiedOutputExtracted(0)._1)),
                                                unionSubstitutions(subst1, subst2)
                                            )
                                        }
                                    )
                                }
                            )
                        }
                    }
                    case _ => e()
                }
            }
            case GenericType(name) => {
                // We may naively substitute this generic type for the other type.
                // However, we have to check that our generic type does not appear
                // anywhere in the other type. If it does, then the types are not
                // unifiable.
                val vars = getTypeVars(b)
                if (vars.contains(name)) {
                    e()
                } else {
                    Right((b, List((name, b))))
                }
            }
        }
    }
}