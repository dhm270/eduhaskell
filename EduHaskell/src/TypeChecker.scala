package eduhaskell

import eduhaskell.syntax._
import eduhaskell.errors._
import eduhaskell.types._

case class TypeContext(bindings: Map[String, Type]) {
    def contains(name: String): Boolean = {
        this.bindings.isDefinedAt(name)
    }

    def get(name: String): Type = {
        this.bindings(name)
    }

    def without(name: String): TypeContext = {
        TypeContext(this.bindings - name)
    }

    def _with(name: String, _type: Type): TypeContext = {
        TypeContext(this.bindings.updated(name, _type))
    }

    def subtract(tc: TypeContext): TypeContext = {
        val namesToRemove = tc.getNames()
        def f(acc: TypeContext, curr: String): TypeContext = {
            acc.without(curr)
        }
        namesToRemove.foldLeft(this)(f)
    }

    def intersectAndReturnSubst(tc: TypeContext): Either[Error, (TypeContext, Substitution)] = {
        val commonNames = this.bindings.keys.toSeq.intersect(tc.bindings.keys.toSeq)
        var commonMapping: Map[String, Type] = Map()
        var currSubst: Substitution = List()
        for (commonName <- commonNames) {
            val t1 = this.get(commonName)
            val t2 = tc.get(commonName)
            val unification = TypeChecker.unify(t1, t2)
            unification match {
                case Right((t, subst)) => {
                    commonMapping += commonName -> t
                    currSubst ++= subst
                }
                case Left(_) => {
                    return Left(TypeError(s"cannot unify types ${t1.source} and ${t2.source}"))
                }
            }
        }
        Right((TypeContext(commonMapping), currSubst))
    }

    def intersect(tc: TypeContext): Either[Error, TypeContext] = {
        val result = this.intersectAndReturnSubst(tc)
        ifNoTypeErrorMulti[(TypeContext, Substitution), TypeContext](result, result => {
            val (tc, _) = result
            Right(tc)
        })
    }

    def unionAndReturnSubst(tc: TypeContext): Either[Error, (TypeContext, Substitution)] = {
        val intersectionResult = this.intersectAndReturnSubst(tc)
        ifNoTypeError[(TypeContext, Substitution)](intersectionResult, intersectionResult => {
            val (intersectionTC, intersectionSubst) = intersectionResult
            val finalMapping = (this.bindings ++ tc.bindings) ++ intersectionTC.bindings
            Right((TypeContext(finalMapping), intersectionSubst))
        })
    }

    def union(tc: TypeContext): Either[Error, TypeContext] = {
        val result = this.unionAndReturnSubst(tc)
        ifNoTypeErrorMulti[(TypeContext, Substitution), TypeContext](result, result => {
            val (tc, _) = result
            Right(tc)
        })
    }

    def substitute(map: Substitution): TypeContext = {
        val names = this.getNames()
        var newBindings: Map[String, Type] = Map()
        for (name <- names) {
            newBindings = newBindings.updated(name, this.get(name).substitute(map))
        }
        TypeContext(newBindings)
    }

    def getNames(): Iterable[String] = {
        this.bindings.keys
    }

    def isEmpty(): Boolean = {
        this.bindings.isEmpty
    }
}

object TypeContext {
    def empty: TypeContext = TypeContext(Map())

    def unionAll(contexts: Seq[TypeContext]): Either[Error, TypeContext] = {
        def f(acc: Either[Error, TypeContext], curr: TypeContext): Either[Error, TypeContext] = {
            acc match {
                case Left(_) => acc
                case Right(acc) => acc.union(curr)
            }
        }
        val x: Either[Error, TypeContext] = Right(TypeContext.empty)
        contexts.foldLeft(x)(f)
    }
}

object TypeChecker {
    var genericTypeCounter = 0
    def newGenericType(): Type = {
        val result = GenericType(s"t${genericTypeCounter}")
        genericTypeCounter += 1
        result
    }

    def ok(t: Type): Either[Error, (Type, TypeContext)] = {
        Right((t, TypeContext.empty))
    }

    def union[A](sets: Seq[Set[A]]): Set[A] = {
        val x: Set[A] = Set()
        def f(a: Set[A], b: Set[A]): Set[A] = a ++ b
        sets.foldLeft(x)(f)
    }

    def infer(env: TypeEnvironment, e: Expr): Either[Error, (Type, TypeContext)] = {
        // If successful, returns the type, along with the TypeContext.
        
        e match {
            case Op(name) => Left(NotImplemented())
            case Id(name) => {
                // We can't infer the type of this name at this point.
                // What if we're within a binding?
                // Then the value won't be the same as in the env.
                val t = newGenericType()
                val tc = TypeContext(Map(name -> t))
                Right((t, tc))
            }

            case Num(_) => ok(IntegerType())
            case Real(_) => ok(RealType())
            case Quote(quote) => {
                if (quote.opening == "'") {
                    ok(CharacterType())
                } else {
                    ok(SequenceType(CharacterType()))
                }
            }
            case Tuple(exprs) => {
                if (exprs.length == 1) {
                    infer(env, exprs(0))
                } else {
                    // TODO: fix e.g. `f x = (x, x + 1)`
                    val subtypeInferences = exprs.map(expr => infer(env, expr))
                    ifNoTypeErrors[(Type, TypeContext)](
                        subtypeInferences,
                        subtypeInferences => {
                            val subtypes = subtypeInferences.map(_._1)
                            val tc = TypeContext.unionAll(subtypeInferences.map(_._2))
                            ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](tc, tc => {
                                Right((TupleType(subtypes), tc))
                            })
                        }
                    )
                }
            }
            case s @ Sequence(exprs) => {
                if (s.containsDoubleDot()) {
                    s.toArithmeticSequence() match {
                        case Some(s) => infer(env, s)
                        case None => Left(TypeError("malformed arithmetic sequence"))
                    }
                } else {
                    inferSequence(env, exprs)
                }
            }
            case ArithmeticSequence(start, next, end) => inferArithmeticSequence(env, start, next, end)
            case SequenceComprehension(expr, qualifiers) => inferSequenceComprehension(env, expr, qualifiers)

            case Binop(op, l, r) => inferBinop(env, op, l, r)
            case Prefix(op, e) => inferPrefix(env, op, e)
            case Apply(l, r) => inferApply(env, l, r)
            case If(condition, trueExpr, falseExpr) => inferIf(env, condition, trueExpr, falseExpr)

            case Case(query, alts) => inferCase(env, query, alts)

            case Where(expr, defs) => inferWhere(env, expr, defs)

            case PartialFun(pattern, guard, expr) => inferPartialFun(env, pattern, guard, expr)
            case Function(alts) => inferFunction(env, alts)
            // case FunctionWithClosures(closureAlts) => ...
            // Not needed, because `FunctionWithClosures` is only ever generated by the evaluator.
            // It will never exist at this stage.
            case Bra(e) => infer(env, e)
            
            case x => Left(TypeError(s"cannot get the type of an expression of type ${x.getClass()}"))
        }
    }

    def getType(env: TypeEnvironment, e: Expr): Either[Error, Type] = {
        val inference = infer(env, e)
        ifNoTypeErrorMulti[(Type, TypeContext), Type](inference, inference => {
            val (t, tc) = inference
            val tcSubst = unifyUndeclaredVariables(env, tc)
            tcSubst match {
                case Right(tcSubst) => Right(t.substitute(tcSubst))
                case Left(_) => tcSubst.asInstanceOf[Either[Error, Type]]
            }
        })
    }

    def inferSequence(env: TypeEnvironment, exprs: Seq[Expr]): Either[Error, (Type, TypeContext)] = {
        if (exprs.length == 0) {
            ok(SequenceType(newGenericType()))
        } else {
            val subtypeInferences = exprs.map(infer(env, _))
            ifNoTypeErrors[(Type, TypeContext)](subtypeInferences, subtypeInferences => {
                if (subtypeInferences.length == 1) {
                    val (t, tc) = subtypeInferences(0)
                    Right((SequenceType(t), tc))
                } else {
                    val tc = TypeContext.unionAll(subtypeInferences.map(_._2))
                    ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](tc, tc => {
                        val types = subtypeInferences.map(_._1)
                        val unifiedType = unifyAll(types)
                        unifiedType match {
                            case Right((unifiedType, subst)) => {
                                Right((SequenceType(unifiedType), tc.substitute(subst)))
                            }
                            case _ => {
                                Left(TypeError("elements of a sequence must all be of the same type"))
                            }
                        }
                    })
                }
            })
        }
    }

    def inferArithmeticSequence(env: TypeEnvironment, start: Expr, next: Option[Expr], end: Option[Expr]): Either[Error, (Type, TypeContext)] = {
        val startInference = infer(env, start)
        ifNoTypeError[(Type, TypeContext)](startInference, startInference => {
            val (startType, startTC) = startInference
            var returnType = startType
            var tc = startTC
            if (next.isDefined) {
                val nextInference = infer(env, next.get)
                nextInference match {
                    case Left(_) => return nextInference
                    case Right((nextType, nextTC)) => {
                        val nextUnificationWithReturn = unify(nextType, returnType)
                        nextUnificationWithReturn match {
                            case Right((unifiedNextType, nextSubst)) => {
                                returnType = unifiedNextType
                                val unionResult = tc.union(nextTC)
                                unionResult match {
                                    case Right(newTC) => tc = newTC.substitute(nextSubst)
                                    case Left(_) => return unionResult.asInstanceOf[Either[Error, (Type, TypeContext)]]
                                }
                            }
                            case _ => Left(TypeError("elements of a sequence must all be of the same type"))
                        }
                    }
                }
            }
            if (end.isDefined) {
                val endInference = infer(env, end.get)
                endInference match {
                    case Left(_) => return endInference
                    case Right((endType, endTC)) => {
                        val endUnificationWithReturn = unify(endType, returnType)
                        endUnificationWithReturn match {
                            case Right((unifiedEndType, endSubst)) => {
                                returnType = unifiedEndType
                                val unionResult = tc.union(endTC)
                                unionResult match {
                                    case Right(newTC) => tc = newTC.substitute(endSubst)
                                    case Left(_) => return unionResult.asInstanceOf[Either[Error, (Type, TypeContext)]]
                                }
                            }
                            case _ => Left(TypeError("elements of a sequence must all be of the same type"))
                        }
                    }
                }
            }
            Right((SequenceType(returnType), tc))
        })
    }

    def inferSequenceComprehension(env: TypeEnvironment, expr: Expr, qualifiers: Seq[SequenceComprehensionQualifier]): Either[Error, (Type, TypeContext)] = {
        if (qualifiers.length == 0) {
            val exprInference = infer(env, expr)
            return ifNoTypeError[(Type, TypeContext)](exprInference, exprInference => {
                val (t, tc) = exprInference
                Right((SequenceType(t), tc))
            })
        }

        val qualifierInQuestion = qualifiers(0)
        val remainingQualifiers = qualifiers.drop(1)

        qualifierInQuestion match {
            case SequenceComprehensionGenerator(pattern, generatorExpr) => {
                val generatorInference = infer(env, generatorExpr)
                ifNoTypeError[(Type, TypeContext)](generatorInference, generatorInference => {
                    val (generatorType, tc) = generatorInference
                    generatorType match {
                        case SequenceType(subtype) => {
                            // TODO: matchAndExtendEnvironment ...
                            Left(NotImplemented())
                        }
                        case _ => Left(TypeError(s"cannot use an expression of type ${generatorType} as a generator in a sequence comprehension"))
                    }
                })
            }
            case SequenceComprehensionDefinition(defn) => {
                val extendedEnvironment = env.withDefinition(defn)
                extendedEnvironment match {
                    case Right(newEnv) => {
                        infer(newEnv, SequenceComprehension(expr, remainingQualifiers))
                    }
                    case x => x.asInstanceOf[Either[Error, (Type, TypeContext)]]
                }
            }
            case SequenceComprehensionGuard(guard) => {
                val conditionInference = infer(env, guard)
                ifNoTypeError[(Type, TypeContext)](conditionInference, conditionInference => {
                    val (conditionType, tc) = conditionInference
                    // Attempt to unify `conditionType` with Boolean
                    val conditionUnificationWithBool = unify(conditionType, BooleanType())
                    conditionUnificationWithBool match {
                        case Right((_, subst)) => {
                            val inference = infer(env, SequenceComprehension(expr, remainingQualifiers))
                            ifNoTypeError[(Type, TypeContext)](inference, inference => {
                                val (t, tc1) = inference
                                val unionResult = tc.union(tc1)
                                ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](unionResult, unionResult => {
                                    Right((t, unionResult.substitute(subst)))
                                })
                            })
                        }
                        case _ => Left(TypeError(s"sequence comprehension guard is of type ${conditionType}, but it should be ${BooleanType().source}"))
                    }
                })
            }
        }
    }

    def inferBinop(env: TypeEnvironment, op: String, l: Expr, r: Expr): Either[Error, (Type, TypeContext)] = {
        val lInference = infer(env, l)
        val rInference = infer(env, r)
        ifNoTypeErrors[(Type, TypeContext)](
            List(lInference, rInference),
            inferences => {
                val (lType, lTC) = inferences(0)
                val (rType, rTC) = inferences(1)
                def e(): Either[Error, (Type, TypeContext)] = {
                    Left(TypeError(
                        s"operands of ${op} have mismatched types: lhs is of type ${lType.source}, rhs is of type ${rType.source}"
                    ))
                }
                def e1(): Either[Error, (Type, TypeContext)] = {
                    Left(TypeError(
                        s"${op} cannot be applied to values of type ${lType.source} and ${rType.source}"
                    ))
                }
                def ok(t: Type): Either[Error, (Type, TypeContext)] = {
                    val unionResult = lTC.union(rTC)
                    ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](
                        unionResult,
                        unionResult => Right((t, unionResult))
                    )
                }
                def unifyAndYield(subst1: Substitution, subst2: Substitution, yieldValue: Type): Either[Error, (Type, TypeContext)] = {
                    val unifiedSubst = unionSubstitutions(subst1, subst2)
                    ifNoTypeErrorMulti[Substitution, (Type, TypeContext)](unifiedSubst, unifiedSubst => {
                        unifyTCAndYield(unifiedSubst, yieldValue)
                    })
                }
                def unifyTCAndYield(subst: Substitution, yieldValue: Type): Either[Error, (Type, TypeContext)] = {
                    val attemptedTCUnification = lTC.union(rTC)
                    ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](attemptedTCUnification, attemptedTCUnification => {
                        Right((yieldValue, attemptedTCUnification.substitute(subst)))
                    })
                }
                op match {
                    case "+" | "-" | "*" | "/" | "^" => {
                        // Attempt to unify the left operand with Integer
                        val leftUnificationWithInt = unify(lType, IntegerType())
                        leftUnificationWithInt match {
                            case Right((_, subst1)) => {
                                val rightUnificationWithInt = unify(rType.substitute(subst1), IntegerType())
                                rightUnificationWithInt match {
                                    case Right((_, subst2)) => {
                                        return unifyAndYield(subst1, subst2, IntegerType())
                                    }
                                    case Left(_) => {}
                                }
                            }
                            case Left(_) => {}
                        }
                        // If it's not Integer, then let's try Real instead
                        val leftUnificationWithReal = unify(lType, RealType())
                        leftUnificationWithReal match {
                            case Right((_, subst1)) => {
                                val rightUnificationWithReal = unify(rType.substitute(subst1), RealType())
                                rightUnificationWithReal match {
                                    case Right((_, subst2)) => {
                                        return unifyAndYield(subst1, subst2, RealType())
                                    }
                                    case Left(_) => return e()
                                }
                            }
                            case Left(_) => {}
                        }
                        e1()
                    }
                    case ":" => {
                        // Attempt to unify the right operand with [a]
                        val a = newGenericType()
                        val rightUnificationWithSeqA = unify(rType, SequenceType(a))
                        rightUnificationWithSeqA match {
                            case Right((SequenceType(subtype), subst1)) => {
                                // Attempt to unify the left operand with the subtype we just got
                                val leftUnificationWithSubtype = unify(lType, subtype)
                                leftUnificationWithSubtype match {
                                    case Right((unifiedSubtype, subst2)) => {
                                        return unifyAndYield(subst1, subst2, SequenceType(unifiedSubtype))
                                    }
                                    case Left(_) => {}
                                }
                            }
                            case _ => {}
                        }
                        e1()
                    }
                    case "==" => {
                        // Attempt to unify the types
                        val unification = unify(lType, rType)
                        unification match {
                            case Right((_, subst)) => {
                                return unifyTCAndYield(subst, BooleanType())
                            }
                            case Left(_) => {}
                        }
                        e()
                    }
                    case "<" | "<=" | ">=" | ">" => {
                        // Attempt to unify the left operand with Integer
                        val leftUnificationWithInt = unify(lType, IntegerType())
                        leftUnificationWithInt match {
                            case Right((_, subst1)) => {
                                val rightUnificationWithInt = unify(rType.substitute(subst1), IntegerType())
                                rightUnificationWithInt match {
                                    case Right((_, subst2)) => {
                                        return unifyAndYield(subst1, subst2, BooleanType())
                                    }
                                    case Left(_) => {}
                                }
                            }
                            case Left(_) => {}
                        }
                        // If it's not Integer, then let's try Real instead
                        val leftUnificationWithReal = unify(lType, RealType())
                        leftUnificationWithReal match {
                            case Right((_, subst1)) => {
                                val rightUnificationWithReal = unify(rType.substitute(subst1), RealType())
                                rightUnificationWithReal match {
                                    case Right((_, subst2)) => {
                                        return unifyAndYield(subst1, subst2, BooleanType())
                                    }
                                    case Left(_) => {}
                                }
                            }
                            case Left(_) => {}
                        }
                        e1()
                    }
                    case "||" | "&&" => {
                        // Attempt to unify the left operand with Boolean
                        val leftUnificationWithBool = unify(lType, BooleanType())
                        leftUnificationWithBool match {
                            case Right((_, subst1)) => {
                                val rightUnificationWithBool = unify(rType.substitute(subst1), BooleanType())
                                rightUnificationWithBool match {
                                    case Right((_, subst2)) => {
                                        return unifyAndYield(subst1, subst2, BooleanType())
                                    }
                                    case Left(_) => {}
                                }
                            }
                            case Left(_) => {}
                        }
                        e1()
                    }
                }
            }
        )
    }

    def inferPrefix(env: TypeEnvironment, op: String, e: Expr): Either[Error, (Type, TypeContext)] = {
        val eInference = infer(env, e)
        ifNoTypeError[(Type, TypeContext)](eInference, eInference => {
            val (eType, eTC) = eInference
            op match {
                case "-" => {
                    // Attempt to unify the operand with Integer
                    val unificationWithInt = unify(eType, IntegerType())
                    unificationWithInt match {
                        case Right((_, subst)) => {
                            return Right((IntegerType(), eTC.substitute(subst)))
                        }
                        case Left(_) => {}
                    }
                    // If it's not Integer, then let's try Real instead
                    val unificationWithReal = unify(eType, RealType())
                    unificationWithReal match {
                        case Right((_, subst)) => {
                            return Right((RealType(), eTC.substitute(subst)))
                        }
                        case Left(_) => {}
                    }
                    Left(TypeError(s"cannot negate a value of type ${eType.source}"))
                }
            }
        })
    }

    def inferApply(env: TypeEnvironment, l: Expr, r: Expr): Either[Error, (Type, TypeContext)] = {
        // Hindley's algorithm
        val lInference = infer(env, l)
        val rInference = infer(env, r)
        ifNoTypeErrors[(Type, TypeContext)](List(lInference, rInference), inferences => {
            val (lType, lTC) = inferences(0)
            val (rType, rTC) = inferences(1)
            // implicitly unifies C_1, ..., C_n with D_1, ..., D_n
            val unionResult = lTC.unionAndReturnSubst(rTC)
            ifNoTypeErrorMulti[(TypeContext, Substitution), (Type, TypeContext)](unionResult, unionResult => {
                var (unionTC, unionSubst) = unionResult
                lType match {
                    case FunctionType(argument, output) => {
                        // unify A with Q
                        val unificationResult = unify(argument, rType)
                        ifNoTypeErrorMulti[(Type, Substitution), (Type, TypeContext)](unificationResult, unificationResult => {
                            val (unifiedArgument, subst) = unificationResult
                            unionTC = unionTC.substitute(subst)
                            val unifiedSubst = unionSubstitutions(unionSubst, subst)
                            ifNoTypeErrorMulti[Substitution, (Type, TypeContext)](unifiedSubst, unifiedSubst => {
                                Right((output.substitute(unifiedSubst), unionTC))
                            })
                        })                        
                    }
                    case GenericType(name) => {
                        // unify a with Q -> b
                        val freshVar = newGenericType()
                        val unificationResult = unify(lType, FunctionType(rType, freshVar))
                        ifNoTypeErrorMulti[(Type, Substitution), (Type, TypeContext)](unificationResult, unificationResult => {
                            val (unifiedArgument, subst) = unificationResult
                            unionTC = unionTC.substitute(subst)
                            val unifiedSubst = unionSubstitutions(unionSubst, subst)
                            ifNoTypeErrorMulti[Substitution, (Type, TypeContext)](unifiedSubst, unifiedSubst => {
                                Right((freshVar.substitute(unifiedSubst), unionTC))
                            })
                        })
                    }
                    case _ => {
                        Left(TypeError(s"cannot call an expression of type ${lType.source}"))
                    }
                }
            })
        })
    }

    def inferIf(env: TypeEnvironment, condition: Expr, trueExpr: Expr, falseExpr: Expr): Either[Error, (Type, TypeContext)] = {
        val conditionInference = infer(env, condition)
        ifNoTypeError[(Type, TypeContext)](conditionInference, conditionInference => {
            val (conditionType, conditionTC) = conditionInference
            // Attempt to unify `conditionType` with Boolean
            val conditionUnificationWithBool = unify(conditionType, BooleanType())
            conditionUnificationWithBool match {
                case Right((_, subst1)) => {
                    val trueInference = infer(env, trueExpr)
                    val falseInference = infer(env, falseExpr)
                    ifNoTypeErrors[(Type, TypeContext)](List(trueInference, falseInference), inferences => {
                        val (trueType, trueTC) = inferences(0)
                        val (falseType, falseTC) = inferences(1)
                        // Attempt to unify the types of the two arms
                        val attemptedUnification = unify(trueType, falseType)
                        attemptedUnification match {
                            case Right((unifiedType, subst2)) => {
                                val unifiedSubst = unionSubstitutions(subst1, subst2)
                                ifNoTypeErrorMulti[Substitution, (Type, TypeContext)](unifiedSubst, unifiedSubst => {
                                    val tc = TypeContext.unionAll(
                                        List(conditionTC, trueTC, falseTC)
                                    )
                                    ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](tc, tc => {
                                        Right((unifiedType, tc.substitute(unifiedSubst)))
                                    })
                                })
                            }
                            case _ => Left(TypeError("both arms of an if-statement must be of the same type"))
                        }
                    })
                }
                case _ => Left(TypeError(s"condition of an if-statement must be of type ${BooleanType().source}"))
            }
        })
    }

    def inferCase(env: TypeEnvironment, query: Expr, alts: Seq[CaseArm]): Either[Error, (Type, TypeContext)] = {
        def altToPartialFun(alt: CaseArm): PartialFun = {
            PartialFun(alt.pattern, None, alt.result)
        }
        val partialFuns = alts.map(altToPartialFun)
        infer(env, Apply(Function(partialFuns), query))
    }

    def inferWhere(env: TypeEnvironment, expr: Expr, defs: Seq[Definition]): Either[Error, (Type, TypeContext)] = {
        val extendedEnvironment = env.withDefinitions(defs)
        ifNoTypeErrorMulti[TypeEnvironment, (Type, TypeContext)](
            extendedEnvironment,
            extendedEnvironment => {
                val newNames = extendedEnvironment.difference(env)
                val inference = infer(extendedEnvironment, expr)
                ifNoTypeError[(Type, TypeContext)](inference, inference => {
                    val (t, tc) = inference
                    val undeclaredVarsSubst = unifyUndeclaredVariables(extendedEnvironment, tc)
                    ifNoTypeErrorMulti[Substitution, (Type, TypeContext)](undeclaredVarsSubst, undeclaredVarsSubst => {
                        val modifiedTC = tc.subtract(TypeContext(newNames))
                        Right((t.substitute(undeclaredVarsSubst), modifiedTC.substitute(undeclaredVarsSubst)))
                    })
                })
            }
        )
    }

    def inferPartialFun(env: TypeEnvironment, pattern: Expr, guard: Option[Expr], expr: Expr): Either[Error, (Type, TypeContext)] = {
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

        val argumentType = infer(env, pat)
        val outputType = infer(env, e)
        ifNoTypeErrors[(Type, TypeContext)](
            List(argumentType, outputType),
            inferences => {
                val (argumentType, argumentTC) = inferences(0)
                var (outputType, outputTC) = inferences(1)
                val attemptedUnification = argumentTC.unionAndReturnSubst(outputTC)
                ifNoTypeErrorMulti[(TypeContext, Substitution), (Type, TypeContext)](attemptedUnification, attemptedUnification => {
                    val (unifiedTC, subst) = attemptedUnification
                    val partialFunType = FunctionType(
                        argumentType.substitute(subst),
                        outputType.substitute(subst)
                    )
                    outputTC = outputTC.substitute(subst)
                    val partialFunTC = outputTC.subtract(argumentTC)
                    Right((partialFunType, partialFunTC))
                })
            }
        )
    }

    def inferFunction(env: TypeEnvironment, alts: Seq[PartialFun]): Either[Error, (Type, TypeContext)] = {
        val altInferences = alts.map(alt => infer(env, alt))
        if (altInferences.length == 1) {
            altInferences(0)
        } else {
            ifNoTypeErrors[(Type, TypeContext)](altInferences, altInferences => {
                val altTC = TypeContext.unionAll(altInferences.map(_._2))
                ifNoTypeErrorMulti[TypeContext, (Type, TypeContext)](altTC, altTC => {
                    val altTypes = altInferences.map(_._1)
                    val unifiedType = unifyAll(altTypes)
                    unifiedType match {
                        case Right((unifiedType, subst)) => {
                            Right((unifiedType, altTC.substitute(subst)))
                        }
                        case _ => {
                            Left(TypeError("all possible paths in a given function must be of the same type"))
                        }
                    }
                })
            })
        }
    }

    def typesEqual(a: Type, b: Type): Boolean = {
        a match {
            case IntegerType() | RealType() | CharacterType() | BooleanType() | GenericType(_) => a == b
            case TupleType(subtypes) => {
                b match {
                    case TupleType(subtypes1) => {
                        subtypes.length == subtypes1.length &&
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

    def unionSubstitutions(substA: Substitution, substB: Substitution): Either[Error, Substitution] = {
        // This assumes that all type variables are distinct when declared.
        // Therefore, if we see two subsitutions for the same type variable,
        // then we need to unify them.
        val namesA = substA.map(_._1)
        val namesB = substB.map(_._1)
        val overlappingNames = namesA.intersect(namesB)
        var finalSubst: Substitution = List()
        for (subst <- (substA ++ substB)) {
            val (name, to) = subst
            if (!overlappingNames.contains(name)) finalSubst ++= List(subst)
        }
        for (overlappingName <- overlappingNames) {
            val thisNameSubstA = substA.filter(_._1 == overlappingName).map(_._2)
            val thisNameSubstB = substB.filter(_._1 == overlappingName).map(_._2)
            val allTypes = thisNameSubstA ++ thisNameSubstB
            val attemptedUnification = unifyAll(allTypes)
            attemptedUnification match {
                case Right((t, unificationSubst)) => {
                    val attemptedSubstUnification = unionSubstitutions(finalSubst, unificationSubst)
                    attemptedSubstUnification match {
                        case Right(s) => finalSubst = s
                        case Left(_) => return attemptedSubstUnification
                    }
                }
                case Left(_) => return attemptedUnification.asInstanceOf[Either[Error, Substitution]]
            }
        }
        Right(finalSubst)
    }

    def unifyUndeclaredVariables(env: TypeEnvironment, tc: TypeContext): Either[Error, Substitution] = {
        var currSubst: Substitution = List()
        for (variable <- tc.getNames()) {
            if (!env.containsName(variable)) {
                return Left(UndeclaredVariable(variable))
            } else {
                val typeInEnv = env.find(variable)
                val typeInTC = tc.get(variable)
                val attemptedUnification = unify(typeInEnv, typeInTC)
                attemptedUnification match {
                    case Right((t, s)) => {
                        val attemptedSubstUnification = unionSubstitutions(currSubst, s)
                        attemptedSubstUnification match {
                            case Right(s) => currSubst = s
                            case Left(_) => return attemptedSubstUnification
                        }
                    }
                    case Left(_) => return attemptedUnification.asInstanceOf[Either[Error, Substitution]]
                }
            }
        }
        Right(currSubst)
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
                    Right((a, List()))
                } else {
                    e()
                }
            }
            case TupleType(subtypesA) => {
                b match {
                    case TupleType(subtypesB) => {
                        if (subtypesA.length != subtypesB.length) return Left(TypeError(s"${a.source} and ${b.source} are tuple types of different arities, so are not unifiable"))

                        var subst: Either[Error, Substitution] = Right(List())
                        def addToSubst(subst1: Substitution): Unit = {
                            subst match {
                                case Right(s) => subst = unionSubstitutions(s, subst1)
                                case Left(_) => {}
                            }
                        }
                        val unifiedSubtypes = subtypesA.zip(subtypesB).map({ case (a, b) => {
                            subst match {
                                case Right(subst) => {
                                    val result = unify(a.substitute(subst), b.substitute(subst))
                                    result match {
                                        case Left(_) => {}
                                        case Right((r, subst1)) => addToSubst(subst1)
                                    }
                                    result
                                }
                                case Left(_) => subst.asInstanceOf[Either[Error, (Type, Substitution)]]
                            }
                        }})
                        ifNoTypeErrors[(Type, Substitution)](unifiedSubtypes, unifiedSubtypes => {
                            ifNoTypeErrorMulti[Substitution, (Type, Substitution)](subst, subst => {
                                Right((TupleType(unifiedSubtypes.map(_._1)), subst))
                            })
                        })
                    }
                    case _ => e()
                }
            }
            case SequenceType(subtypeA) => {
                b match {
                    case SequenceType(subtypeB) => {
                        val unificationResult = unify(subtypeA, subtypeB)
                        ifNoTypeError[(Type, Substitution)](
                            unificationResult,
                            unifiedExtracted => Right((SequenceType(unifiedExtracted._1), unifiedExtracted._2))
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
                            val attemptedUnification = unify(substOutputA, substOutputB)
                            ifNoTypeError[(Type, Substitution)](attemptedUnification, attemptedUnification => {
                                val (unifiedType, subst) = attemptedUnification
                                val attemptedSubstUnification = unionSubstitutions(map, subst)
                                ifNoTypeErrorMulti[Substitution, (Type, Substitution)](attemptedSubstUnification, attemptedSubstUnification => {
                                    Right((FunctionType(other.substitute(subst), unifiedType), attemptedSubstUnification))
                                })
                            })
                        }
                        if (argumentA.isInstanceOf[GenericType]) {
                            f(argumentA, argumentB)
                        } else if (argumentB.isInstanceOf[GenericType]) {
                            f(argumentB, argumentA)
                        } else {
                            val argumentUnificationResult = unify(argumentA, argumentB)
                            ifNoTypeError[(Type, Substitution)](
                                argumentUnificationResult,
                                unifiedArgumentExtracted => {
                                    val subst1 = unifiedArgumentExtracted._2
                                    val outputUnificationResult = unify(outputA.substitute(subst1), outputB.substitute(subst1))
                                    ifNoTypeError[(Type, Substitution)](
                                        outputUnificationResult,
                                        unifiedOutputExtracted => {
                                            val subst2 = unifiedOutputExtracted._2
                                            val unifiedSubst = unionSubstitutions(subst1, subst2)
                                            ifNoTypeErrorMulti[Substitution, (Type, Substitution)](unifiedSubst, unifiedSubst => {
                                                Right((
                                                    FunctionType(unifiedArgumentExtracted._1.substitute(subst2), unifiedOutputExtracted._1),
                                                    unifiedSubst
                                                ))
                                            })
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

    def unifyAll(inputs: Seq[Type]): Either[Error, (Type, Substitution)] = {
        def f(acc: Either[Error, (Type, Substitution)], curr: Type): Either[Error, (Type, Substitution)] = {
            ifNoTypeError[(Type, Substitution)](acc, acc => {
                val (accType, accSubst) = acc
                val attemptedUnification = unify(accType, curr)
                ifNoTypeError[(Type, Substitution)](attemptedUnification, attemptedUnification => {
                    val (unifiedType, subst) = attemptedUnification
                    val unifiedSubst = unionSubstitutions(accSubst, subst)
                    ifNoTypeErrorMulti[Substitution, (Type, Substitution)](unifiedSubst, unifiedSubst => {
                        Right((unifiedType, unifiedSubst))
                    })
                })
            })
        }
        val x: Either[Error, (Type, Substitution)] = Right((newGenericType(), List()))
        inputs.foldLeft(x)(f)
    }
}