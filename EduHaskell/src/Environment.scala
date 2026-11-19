package eduhaskell

import collection.mutable.Map

import eduhaskell.types._
import eduhaskell.values._
import eduhaskell.errors._
import eduhaskell.syntax._
import eduhaskell.Evaluator

class Environment(private val map: Map[String, LazyValue], private var functionsInScope: List[String]) {
    def extended(ident: String, lazyValue: LazyValue): Environment = {
        new Environment(this.map.updated(ident, lazyValue), this.functionsInScope)
    }

    def find(ident: String): LazyValue = {
        if (map.isDefinedAt(ident)) {
            map(ident)
        } else {
            LazyValue(() => Result(Left(UndeclaredVariable(ident))))
        }
    }

    def define(ident: String, lazyValue: LazyValue): Environment = {
        this.extended(ident, lazyValue)
    }

    def isFunctionInScope(ident: String): Boolean = this.functionsInScope.contains(ident)

    def registerFunction(ident: String): Unit = {
        if (!this.isFunctionInScope(ident)) {
            this.functionsInScope = ident :: this.functionsInScope
        }
    }

    def flattenApply(e: Expr): List[Expr] = {
        e match {
            case Apply(l, r) => {
                flattenApply(l) ++ List(r)
            }
            case _ => List(e)
        }
    }

    def withDefinition(defn: Definition): Either[Error, (Environment, Seq[String])] = {
        defn match {
            case Equation(lhs, rhs) => Right(this.withEquationDefinition(lhs, rhs))
            case WhereEquation(lhs, rhs, defs) => {
                val extendedEnvironment = this.withDefinitions(defs)
                extendedEnvironment match {
                    case Right((newEnv, names)) => {
                        val result = newEnv.withEquationDefinition(lhs, rhs)
                        result match {
                            case (newEnv1, names1) => Right((newEnv1, names ++ names1))
                        }
                    }
                    case x => x
                }
            }
            case GuardedEquation(lhs, cases, defs) => {
                lhs match {
                    case Id(name) => {
                        def f(acc: Either[Error, (Environment, Seq[String])], curr: GuardedResult): Either[Error, (Environment, Seq[String])] = {
                            acc match {
                                case Right(_) => acc
                                case Left(PatternMatchError()) => {
                                    curr match {
                                        case GuardedResult(guard, expr) => {
                                            val result = Evaluator.evaluate(this, guard)
                                            result match {
                                                case Right(BooleanValue(true)) => {
                                                    val extendedEnvironment = this.withDefinitions(defs)
                                                    extendedEnvironment match {
                                                        case Right((env, names)) => {
                                                            val result = env.withEquationDefinition(lhs, expr)
                                                            result match {
                                                                case (env1, names1) => Right((env1, names ++ names1))
                                                            }
                                                        }
                                                        case x => x
                                                    }
                                                }
                                                case Right(BooleanValue(false)) => acc
                                                case Right(_) => Left(PatternMatchError())
                                                case Left(_) => result.asInstanceOf[Either[Error, (Environment, Seq[String])]]
                                            }
                                        }
                                    }
                                }
                                case Left(_) => acc
                            }
                        }
                        val x: Either[Error, (Environment, Seq[String])] = Left(PatternMatchError())
                        cases.foldLeft(x)(f)
                    }
                    case Apply(l, r) => {
                        val exprs = flattenApply(lhs)
                        exprs(0) match {
                            case Id(funcName) => {
                                if (this.isFunctionInScope(funcName)) {
                                    // If the function is already defined, then we want to warn the user that they have
                                    // redefined an existing function rather than extending it.
                                    println(s"% Warning: existing function definition for ${funcName} overwritten rather than extended") // TODO: handle this better
                                }
                                def f(_case: GuardedResult): PartialFun = {
                                    _case match {
                                        case GuardedResult(guard, expr) => {
                                            createPartialFun(lhs, Some(guard), expr)
                                        }
                                    }
                                }
                                val partialFuns = cases.map(f)
                                val func = Function(partialFuns)
                                lazy val newEnv: Environment = {
                                    lazy val cont = Closure(newEnv, func)
                                    this.registerFunction(funcName)
                                    this.define(funcName, LazyValue(() => cont))
                                }
                                Right((newEnv, List(funcName)))
                            }
                        }
                    }
                }
            }
        }
    }

    def withEquationDefinition(lhs: Expr, rhs: Expr): (Environment, Seq[String]) = {
        lhs match {
            case Id(name) => {
                lazy val newEnv: Environment = {
                    lazy val cont = Closure(newEnv, rhs)
                    this.define(name, LazyValue(() => cont))
                }
                (newEnv, List(name))
            }
            case Apply(l, r) => {
                val exprs = flattenApply(lhs)
                exprs(0) match {
                    case Id(funcName) => {
                        if (this.isFunctionInScope(funcName)) {
                            // If the function is already defined, then we want to warn the user that they have
                            // redefined an existing function rather than extending it.
                            println(s"% Warning: existing function definition for ${funcName} overwritten rather than extended") // TODO: handle this better
                        }
                        val asPartialFun = createPartialFun(lhs, None, rhs)
                        lazy val newEnv: Environment = {
                            lazy val cont = Closure(newEnv, asPartialFun)
                            this.registerFunction(funcName)
                            this.define(funcName, LazyValue(() => cont))
                        }
                        (newEnv, List(funcName))
                    }
                }
            }
        }
    }

    def withDefinitions(defs: Seq[Definition]): Either[Error, (Environment, Seq[String])] = {
        def f(acc: Either[Error, (Environment, Seq[String])], curr: Definition): Either[Error, (Environment, Seq[String])] = {
            acc match {
                case Right((env, names)) => {
                    val result = env.withDefinition(curr)
                    result match {
                        case Right((env1, names1)) => Right((env1, names ++ names1))
                        case x => x
                    }
                }
                case _ => acc
            }
        }
        val x: Either[Error, (Environment, Seq[String])] = Right((this, List()))
        defs.foldLeft(x)(f)
    }

    def withMultilineDefinition(name: String, defs: List[Definition]): Either[Error, (Environment, Seq[String])] = {
        // Check if we're just defining a regular value, or a function
        defs(0) match {
            case Equation(Id(name), _) => {
                // i.e. something like `x = 1` (rather than e.g. `f x = 1`)
                if (defs.length == 1) {
                    return this.withDefinition(defs(0))
                } else {
                    return Left(ModuleLoadError(s"multiple definitions for ${name}"))
                }
            }
            case _ => {}
        }

        // If we've got here, then we have a function definition
        def toAlts(env: Environment, defn: Definition): Seq[(Environment, Seq[String], PartialFun)] = {
            defn match {
                case Equation(lhs, rhs) => List((env, List(), createPartialFun(lhs, None, rhs)))
                case WhereEquation(lhs, rhs, defs) => {
                    val extendedEnvironment = env.withDefinitions(defs)
                    extendedEnvironment match {
                        case Right((newEnv, names)) => List((newEnv, names, createPartialFun(lhs, None, rhs)))
                        case x => List() // TODO: fix
                    }
                }
                case GuardedEquation(lhs, cases, defs) => {
                    val exprs = flattenApply(lhs)
                    def f(_case: GuardedResult): PartialFun = {
                        _case match {
                            case GuardedResult(guard, expr) => {
                                createPartialFun(lhs, Some(guard), expr)
                            }
                        }
                    }
                    val extendedEnvironment = env.withDefinitions(defs)
                    extendedEnvironment match {
                        case Right((env, names)) => {
                            val partialFuns = cases.map(f)
                            partialFuns.map(p => (env, names, p))
                        }
                        case x => List() // TODO: fix
                    }
                }
            }
        }
        lazy val newEnv: Environment = {
            lazy val alts: Seq[(Environment, Seq[String], PartialFun)] = defs.flatMap(defn => toAlts(newEnv, defn))
            lazy val function = FunctionWithClosures(alts)
            lazy val cont = Closure(newEnv, function)
            this.registerFunction(name)
            this.define(name, LazyValue(() => cont))
        }
        Right((newEnv, List(name)))
    }

    def parseAllDefinitions(defs: List[Definition]): Either[Error, (Environment, Seq[String])] = {
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
            def f(acc: Either[Error, (Environment, Seq[String])], curr: (String, List[Definition])): Either[Error, (Environment, Seq[String])] = {
                acc match {
                    case Right((env, names)) => {
                        curr match {
                            case (name, defs) => {
                                val result = env.withMultilineDefinition(name, defs)
                                result match {
                                    case Right((env1, names1)) => Right(env1, names ++ names1)
                                    case x => x
                                }
                            }
                        }
                    }
                    case _ => acc
                }
            }
            val x: Either[Error, (Environment, Seq[String])] = Right((this, List()))
            defsByName.keys.map(key => (key, defsByName(key))).foldLeft(x)(f)
        }
    }
}

object Environment {
    def empty: Environment = new Environment(Map(), List())
}