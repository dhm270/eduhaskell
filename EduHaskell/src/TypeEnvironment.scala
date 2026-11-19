package eduhaskell

import collection.mutable.Map

import eduhaskell.errors._
import eduhaskell.syntax._
import eduhaskell.types._

// TODO: move "conflicting names" checks to the type checker
// rather than the evaluator?

class TypeEnvironment(private val map: Map[String, Type]) {
    def extended(ident: String, _type: Type): TypeEnvironment = {
        new TypeEnvironment(this.map.updated(ident, _type))
    }

    def find(ident: String): Type = {
        this.map(ident)
    }

    def define(ident: String, _type: Type): TypeEnvironment = {
        this.extended(ident, _type)
    }

    def flattenApply(e: Expr): List[Expr] = {
        e match {
            case Apply(l, r) => {
                flattenApply(l) ++ List(r)
            }
            case _ => List(e)
        }
    }

    def withDefinition(defn: Definition): Either[Error, TypeEnvironment] = {
        defn match {
            case Equation(lhs, rhs) => this.withEquationDefinition(lhs, rhs)
            case WhereEquation(lhs, rhs, defs) => {
                val extendedEnvironment = this.withDefinitions(defs)
                extendedEnvironment match {
                    case Right(newEnv) => {
                        newEnv.withEquationDefinition(lhs, rhs)
                    }
                    case x => x
                }
            }
            case GuardedEquation(lhs, cases, defs) => {
                // First, check that all guards are `Boolean`
                val guards = cases.map(_.guard)
                val guardTypes = guards.map(TypeChecker.getType(this, _))
                val allBoolean = guardTypes.forall(_ == BooleanType())
                if (allBoolean) {
                    lhs match {
                        case Id(name) => {
                            val arms = cases.map(_.expr)
                            val types = arms.map(TypeChecker.getType(this, _))
                            ifNoTypeErrorsMulti[Type, TypeEnvironment](types, typesExtracted => {
                                val comparisonType = typesExtracted(0)
                                val otherTypes = typesExtracted.drop(1)
                                val allEqual = otherTypes.forall(_ == comparisonType)
                                if (allEqual) {
                                    Right(this.define(name, comparisonType))
                                } else {
                                    Left(TypeError(s"not all possibilities in the guarded expression yield the same type"))
                                }
                            })
                        }
                        case Apply(l, r) => {
                            val exprs = flattenApply(lhs)
                            exprs(0) match {
                                case Id(funcName) => {
                                    def f(_case: GuardedResult): PartialFun = {
                                        _case match {
                                            case GuardedResult(guard, expr) => {
                                                createPartialFun(lhs, Some(expr), expr)
                                            }
                                        }
                                    }
                                    val partialFuns = cases.map(f)
                                    val func = Function(partialFuns)
                                    val functionType = TypeChecker.getType(this, func)
                                    ifNoTypeErrorMulti[Type, TypeEnvironment](
                                        functionType,
                                        extractedType => Right(this.define(funcName, extractedType))
                                    )
                                }
                            }
                        }
                    }
                } else {
                    Left(TypeError(s"not all guards in the guarded expression are of type ${BooleanType().source}"))
                }
            }
        }
    }

    def withEquationDefinition(lhs: Expr, rhs: Expr): Either[Error, TypeEnvironment] = {
        lhs match {
            case Id(name) => {
                val rhsType = TypeChecker.getType(this, rhs)
                ifNoTypeErrorMulti[Type, TypeEnvironment](
                    rhsType,
                    extractedType => Right(this.define(name, extractedType))
                )
            }
            case Apply(l, r) => {
                val exprs = flattenApply(lhs)
                exprs(0) match {
                    case Id(funcName) => {
                        val asPartialFun = createPartialFun(lhs, None, rhs)
                        // TODO: check that the type of the argument can be unified
                        // with how it is used within the function definition
                        val partialFunType = TypeChecker.getType(this, asPartialFun)
                        ifNoTypeErrorMulti[Type, TypeEnvironment](
                            partialFunType,
                            extractedType => Right(this.define(funcName, extractedType))
                        )
                    }
                }
            }
        }
    }

    def withDefinitions(defs: Seq[Definition]): Either[Error, TypeEnvironment] = {
        def f(acc: Either[Error, TypeEnvironment], curr: Definition): Either[Error, TypeEnvironment] = {
            acc match {
                case Right(env) => env.withDefinition(curr)
                case _ => acc
            }
        }
        val x: Either[Error, TypeEnvironment] = Right(this)
        defs.foldLeft(x)(f)
    }

    def withMultilineDefinition(name: String, defs: List[Definition]): Either[Error, TypeEnvironment] = {
        Left(NotImplemented())
    }

    def parseAllDefinitions(defs: List[Definition]): Either[Error, TypeEnvironment] = {
        def getNameFromLhsExpr(lhs: Expr): String = {
            lhs match {
                case Apply(l, r) => getNameFromLhsExpr(l)
                case Id(name) => name
            }
        }

        val defsByName: collection.mutable.Map[String, List[Definition]] = collection.mutable.Map()

        var currentName = ""
        var currentDefs: List[Definition] = List()

        var duplicatedName: Option[String] = None

        def f(defn: Definition, lhs: Expr): Unit = {
            val name = getNameFromLhsExpr(lhs)
            if (name == currentName) {
                currentDefs = defn :: currentDefs
            } else {
                if (currentDefs.length > 0) defsByName(currentName) = currentDefs
                currentDefs = List(defn)
                if (defsByName.isDefinedAt(name) && !duplicatedName.isDefined) {
                    duplicatedName = Some(name)
                }
                currentName = name
            }
        }

        defs.foreach(defn => {
            defn match {
                case Equation(lhs, _) => f(defn, lhs)
                case GuardedEquation(pattern, _, _) => f(defn, pattern)
                case WhereEquation(lhs, _, _) => f(defn, lhs)
                case _ => {}
            }
        })

        if (currentDefs.length > 0) defsByName(currentName) = currentDefs

        if (duplicatedName.isDefined) {
            Left(ModuleHasDuplicatedDefinitionsError(duplicatedName.get))
        } else {
            def f(acc: Either[Error, TypeEnvironment], curr: (String, List[Definition])): Either[Error, TypeEnvironment] = {
                acc match {
                    case Right(env) => {
                        curr match {
                            case (name, defs) => {
                                env.withMultilineDefinition(name, defs)
                            }
                        }
                    }
                    case _ => acc
                }
            }
            val x: Either[Error, TypeEnvironment] = Right(this)
            defsByName.keys.map(key => (key, defsByName(key))).foldLeft(x)(f)
        }
    }
}

object TypeEnvironment {
    def empty: TypeEnvironment = new TypeEnvironment(Map())
}