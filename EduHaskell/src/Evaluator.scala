package eduhaskell

import eduhaskell.syntax._
import eduhaskell.errors._
import eduhaskell.Environment

object Evaluator {
    def withDefinition(env: Environment, defn: Definition): Either[Error, (Environment, Seq[String])] = {
        defn match {
            case Equation(lhs, rhs) => Right(withEquationDefinition(env, lhs, rhs))
            case WhereEquation(lhs, rhs, defs) => {
                val extendedEnvironment = withDefinitions(env, defs)
                extendedEnvironment match {
                    case Right((newEnv, names)) => {
                        val result = withEquationDefinition(newEnv, lhs, rhs)
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
                                            val result = evaluate(env, guard)
                                            result match {
                                                case Right(BooleanValue(true)) => {
                                                    val extendedEnvironment = withDefinitions(env, defs)
                                                    extendedEnvironment match {
                                                        case Right((env, names)) => {
                                                            val result = withEquationDefinition(env, lhs, expr)
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
                                if (env.isFunctionInScope(funcName)) {
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
                                    lazy val value = evaluate(newEnv, func)
                                    env.registerFunction(funcName)
                                    env.lazyDefine(funcName, LazyValue(() => value))
                                }
                                Right((newEnv, List(funcName)))
                            }
                        }
                    }
                }
            }
        }
    }

    def withEquationDefinition(env: Environment, lhs: Expr, rhs: Expr): (Environment, Seq[String]) = {
        lhs match {
            case Id(name) => {
                lazy val newEnv: Environment = {
                    lazy val value = evaluate(newEnv, rhs)
                    env.lazyDefine(name, LazyValue(() => value))
                }
                (newEnv, List(name))
            }
            case Apply(l, r) => {
                val exprs = flattenApply(lhs)
                exprs(0) match {
                    case Id(funcName) => {
                        if (env.isFunctionInScope(funcName)) {
                            // If the function is already defined, then we want to warn the user that they have
                            // redefined an existing function rather than extending it.
                            println(s"% Warning: existing function definition for ${funcName} overwritten rather than extended") // TODO: handle this better
                        }
                        val asPartialFun = createPartialFun(lhs, None, rhs)
                        lazy val newEnv: Environment = {
                            lazy val value = evaluate(newEnv, asPartialFun)
                            env.registerFunction(funcName)
                            env.lazyDefine(funcName, LazyValue(() => value))
                        }
                        (newEnv, List(funcName))
                    }
                }
            }
        }
    }

    def withDefinitions(env: Environment, defs: Seq[Definition]): Either[Error, (Environment, Seq[String])] = {
        def f(acc: Either[Error, (Environment, Seq[String])], curr: Definition): Either[Error, (Environment, Seq[String])] = {
            acc match {
                case Right((env, names)) => {
                    val result = withDefinition(env, curr)
                    result match {
                        case Right((env1, names1)) => Right((env1, names ++ names1))
                        case x => x
                    }
                }
                case _ => acc
            }
        }
        val x: Either[Error, (Environment, Seq[String])] = Right((env, List()))
        defs.foldLeft(x)(f)
    }

    def withMultilineDefinition(env: Environment, name: String, defs: List[Definition]): Either[Error, (Environment, Seq[String])] = {
        // Check if we're just defining a regular value, or a function
        defs(0) match {
            case Equation(Id(name), _) => {
                // i.e. something like `x = 1` (rather than e.g. `f x = 1`)
                if (defs.length == 1) {
                    return withDefinition(env, defs(0))
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
                    val extendedEnvironment = withDefinitions(env, defs)
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
                    val extendedEnvironment = withDefinitions(env, defs)
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
            lazy val value = evaluate(newEnv, function)
            env.registerFunction(name)
            env.lazyDefine(name, LazyValue(() => value))
        }
        Right((newEnv, List(name)))
    }

    def parseAllDefinitions(env: Environment, defs: List[Definition]): Either[Error, (Environment, Seq[String])] = {
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
                                val result = withMultilineDefinition(env, name, defs)
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
            val x: Either[Error, (Environment, Seq[String])] = Right((env, List()))
            defsByName.keys.map(key => (key, defsByName(key))).foldLeft(x)(f)
        }
    }

    def getLeftmostArgument(e: Expr): Expr = {
        e match {
            case Apply(l @ Apply(_, _), r) => getLeftmostArgument(l)
            case Apply(l, r) => l
        }
    }

    def removeLeftmostArgument(e: Expr): Expr = {
        e match {
            case Apply(l @ Apply(_, _), r) => Apply(removeLeftmostArgument(l), r)
            case Apply(_, r) => r
        }
    }

    def createPartialFun(lhs: Expr, guard: Option[Expr], rhs: Expr): PartialFun = {
        val editedLhs = removeLeftmostArgument(lhs)
        PartialFun(editedLhs, guard, rhs)
    }

    def flattenApply(e: Expr): List[Expr] = {
        e match {
            case Apply(l, r) => {
                flattenApply(l) ++ List(r)
            }
            case _ => List(e)
        }
    }

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
                    Right((env.lazyDefine(name, comparison), List(name)))
                }
            }

            case Num(value) => {
                val got = comparison.get()
                got match {
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
                got match {
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
                got match {
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
                                    charGot match {
                                        case Right(CharValue(c)) => str += c
                                        case _ => valid = false
                                    }
                                    r.get() match {
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
                got match {
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
                    got match {
                        case Right(NilValue()) => Right((env, List()))
                        case _ => Left(PatternMatchError())
                    }
                } else {
                    matchAndExtendEnvironment(env, conflictingNames, Binop(":", patternExprs(0), Sequence(patternExprs.drop(1))), comparison)
                }
            }

            case Binop(":", l, r) => {
                val got = comparison.get()
                got match {
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

    def patternMatch(env: Environment, conflictingNames: Seq[String], pattern: Expr, guard: Option[Expr], comparison: LazyValue, expr: Expr): Either[Error, Value] = {
        val (_, _, result) = patternMatchAndReturnEnvironment(env, conflictingNames, pattern, guard, comparison, expr)
        result
    }

    def patternMatchAndReturnEnvironment(env: Environment, conflictingNames: Seq[String], pattern: Expr, guard: Option[Expr], comparison: LazyValue, expr: Expr): (Environment, Seq[String], Either[Error, Value]) = {
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
                    (newEnv, newNames, evaluate(newEnv, e))
                } else {
                    (env, List(), Left(PatternMatchError()))
                }
            }
            case x => {
                (env, List(), x.asInstanceOf[Either[Error, Value]])
            }
        }
    }

    def evaluate(env: Environment, e: Expr): Either[Error, Value] = {
        e match {
            case Op(name) => Left(NotImplemented())
            case Id(name) => env.find(name).get()

            case Num(value) => Right(IntValue(value))
            case Real(value) => Right(RealValue(value))
            case Quote(quote) => quoteToValue(quote)

            case Tuple(exprs) => {
                if (exprs.length == 1) {
                    evaluate(env, exprs(0))
                } else {
                    def evalAsLazyVal(e: Expr): LazyValue = {
                        LazyValue(() => evaluate(env, e))
                    }
                    Right(TupleValue(exprs.map(evalAsLazyVal(_))))
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

            case Where(e, defs) => {
                val extendedEnvironment = withDefinitions(env, defs)
                extendedEnvironment match {
                    case Right((newEnv, names)) => evaluate(newEnv, e)
                    case x => x.asInstanceOf[Either[Error, Value]]
                }
            }

            case PartialFun(pattern, guard, expr) => {
                def func(lazyValue: LazyValue): Either[Error, Value] = {
                    patternMatch(env, List(), pattern, guard, lazyValue, expr)
                }
                Right(FunctionValue(func))
            }
            case Function(alts) => evaluateFunction(env, alts)
            case FunctionWithClosures(closureAlts) => evaluateFunctionWithClosures(closureAlts)

            case Bra(e) => evaluate(env, e)
            case Assoc(op, exprs) => Left(NotImplemented())
        }
    }

    def stringToStringValue(str: String): Value = {
        if (str == "") {
            NilValue()
        } else {
            ConsValue(
                LazyValue(() => Right(CharValue(str(0)))),
                LazyValue(() => Right(stringToStringValue(str.drop(1))))
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
        } else if (exprs.length == 0) {
            Right(NilValue())
        } else {
            val head = exprs(0)
            val tail = exprs.drop(1)
            evaluate(env, Binop(":", head, Sequence(tail)))
        }
    }

    def generateIntArithmeticSequence(start: Long, step: Long, end: Option[Long]): Either[Error, Value] = {
        if (end.isDefined && start > end.get) {
            Right(NilValue())
        } else {
            lazy val restOfSequence = generateIntArithmeticSequence(start + step, step, end)
            Right(ConsValue(LazyValue(() => Right(IntValue(start))), LazyValue(() => restOfSequence)))
        }
    }

    def generateRealArithmeticSequence(start: Double, step: Double, end: Option[Double]): Either[Error, Value] = {
        if (end.isDefined && start > end.get) {
            Right(NilValue())
        } else {
            lazy val restOfSequence = generateRealArithmeticSequence(start + step, step, end)
            Right(ConsValue(LazyValue(() => Right(RealValue(start))), LazyValue(() => restOfSequence)))
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
        def e(op: String, l: Value, r: Value): Either[Error, Value] = {
            Left(TypeError(
                s"operands of ${op} have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"
            ))
        }
        lazy val lValue = evaluate(env, l)
        lazy val rValue = evaluate(env, r)
        op match {
            case "+" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(IntValue(int1 + int2))
                            case Right(r) => e("+", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(real1 + real2))
                            case Right(r) => e("+", l, r)
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
                            case Right(r) => e("-", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(real1 - real2))
                            case Right(r) => e("-", l, r)
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
                            case Right(r) => e("*", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(real1 * real2))
                            case Right(r) => e("*", l, r)
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
                            case Right(r) => e("/", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => if (real2 == 0) { Left(DivisionByZero()) } else { Right(RealValue(real1 / real2)) }
                            case Right(r) => e("/", l, r)
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
                            case Right(r) => e("^", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(RealValue(Math.pow(real1, real2)))
                            case Right(r) => e("^", l, r)
                            case x => x
                        }
                    }
                    case Right(x) => Left(TypeError(s"lhs operand of ^ is of type ${x.getType()}, which is illegal"))
                    case x => x
                }
            }
            case ":" => {
                // TODO: some kind of type checking
                Right(ConsValue(LazyValue(() => lValue), LazyValue(() => rValue)))
            }
            case "==" => valuesEqual(lValue, rValue)
            case "<" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 < int2))
                            case Right(r) => e("<", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 < real2))
                            case Right(r) => e("<", l, r)
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case "<=" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 <= int2))
                            case Right(r) => e("<=", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 <= real2))
                            case Right(r) => e("<=", l, r)
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case ">=" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 >= int2))
                            case Right(r) => e(">=", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 >= real2))
                            case Right(r) => e(">=", l, r)
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
                            case Right(r) => e(">", l, r)
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 > real2))
                            case Right(r) => e(">", l, r)
                            case x => x
                        }
                    }
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case "||" => {
                lValue match {
                    case Right(l @ BooleanValue(false)) => {
                        rValue match {
                            case r @ Right(BooleanValue(_)) => r
                            case Right(r) => e("||", l, r)
                            case x => x
                        }
                    }
                    case Right(BooleanValue(true)) => Right(BooleanValue(true))
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
            case "&&" => {
                lValue match {
                    case Right(l @ BooleanValue(true)) => {
                        rValue match {
                            case r @ Right(BooleanValue(_)) => r
                            case Right(r) => e("&&", l, r)
                            case x => x
                        }
                    }
                    case Right(BooleanValue(false)) => Right(BooleanValue(false))
                    case Right(x) => Left(NotImplemented())
                    case x => x
                }
            }
        }
    }

    def valuesEqual(lValue: Either[Error, Value], rValue: Either[Error, Value]): Either[Error, Value] = {
        def m(l: Value, r: Value): String = {
            s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"
        }
        lValue match {
            case Right(l @ IntValue(int1)) => {
                rValue match {
                    case Right(IntValue(int2)) => Right(BooleanValue(int1 == int2))
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case Right(l @ RealValue(real1)) => {
                rValue match {
                    case Right(RealValue(real2)) => Right(BooleanValue(real1 == real2))
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case Right(l @ CharValue(char1)) => {
                rValue match {
                    case Right(CharValue(char2)) => Right(BooleanValue(char1 == char2))
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case Right(l @ BooleanValue(bool1)) => {
                rValue match {
                    case Right(BooleanValue(bool2)) => Right(BooleanValue(bool1 == bool2))
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case Right(l @ FunctionValue(_)) => {
                Left(TypeError("functions cannot be compared"))
            }
            case Right(l @ ConsValue(head1, tail1)) => {
                rValue match {
                    case Right(ConsValue(head2, tail2)) => {
                        val headEqualityResult = valuesEqual(head1.get(), head2.get())
                        headEqualityResult match {
                            case Right(BooleanValue(true)) => valuesEqual(tail1.get(), tail2.get())
                            case x => x
                        }
                    }
                    case Right(NilValue()) => Right(BooleanValue(false))
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case Right(l @ NilValue()) => {
                rValue match {
                    case Right(NilValue()) => Right(BooleanValue(true))
                    case Right(ConsValue(_, _)) => Right(BooleanValue(false))
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case Right(l @ TupleValue(vals1)) => {
                rValue match {
                    case Right(r @ TupleValue(vals2)) => {
                        if (vals1.length == vals2.length) {
                            def f(acc: Either[Error, Value], curr: (LazyValue, LazyValue)): Either[Error, Value] = {
                                acc match {
                                    case Right(BooleanValue(true)) => {
                                        curr match {
                                            case (l, r) => {
                                                val lValue = l.get()
                                                val rValue = r.get()
                                                valuesEqual(lValue, rValue)
                                            }
                                            case _ => Right(BooleanValue(false))
                                        }
                                    }
                                    case _ => acc
                                }
                            }
                            val x: Either[Error, Value] = Right(BooleanValue(true))
                            vals1.zip(vals2).foldLeft(x)(f)
                        } else {
                            Left(TypeError(m(l, r)))
                        }
                    }
                    case Right(r) => Left(TypeError(m(l, r)))
                    case x => x
                }
            }
            case x @ Left(_) => x
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
                def f(): Either[Error, Value] = {
                    evaluate(env, r)
                }
                func(LazyValue(f))
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

    def evaluateFunction(env: Environment, alts: Seq[PartialFun]): Either[Error, Value] = {
        val modifiedAlts = alts.map(alt => (env, List(), alt))
        evaluateFunctionWithClosures(modifiedAlts)
    }

    def evaluateFunctionWithClosures(alts: Seq[(Environment, Seq[String], PartialFun)]): Either[Error, Value] = {
        if (alts.length == 0) return Left(PatternMatchError())
        def func(lazyValue: LazyValue): Either[Error, Value] = {
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
                                    case Left(PatternMatchError()) => acc
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
                    evaluate(env, FunctionWithClosures(matches))
                }
                case (_, _, _) => {
                    // this is the last argument, so we will return a non-function
                    def f(acc: Either[Error, Value], curr: (Environment, Seq[String], PartialFun)): Either[Error, Value] = {
                        acc match {
                            case Right(_) => acc // we have already successfully pattern matched
                            case Left(PatternMatchError()) => {
                                // i.e. we haven't successfully matched yet, so we keep going
                                curr match {
                                    case (env, names, PartialFun(pattern, guard, expr)) => patternMatch(env, names, pattern, guard, lazyValue, expr)
                                }
                            }
                            case Left(_) => acc
                        }
                    }
                    val x: Either[Error, Value] = Left(PatternMatchError())
                    alts.foldLeft(x)(f)
                }
            }
        }
        Right(FunctionValue(func))
    }
}