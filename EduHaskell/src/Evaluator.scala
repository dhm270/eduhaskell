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
                            env.lazyDefine(name, LazyValue(() => value))
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
                                    env.lazyDefine(funcName, LazyValue(() => value))
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

    def createPartialFun(lhs: Expr, rhs: Expr): Expr = {        
        val editedLhs = removeLeftmostArgument(lhs)
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

    def matchAndExtendEnvironment(env: Environment, pattern: Expr, comparison: LazyValue): Option[Environment] = {
        // If we're able to successfully pattern match, then we return Some(newEnv),
        // where newEnv is the potentially extended environment, containing any
        // necessary new identifier definitions.
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
                Some(env.lazyDefine(name, comparison))
            }

            case Num(value) => {
                val got = comparison.get()
                got match {
                    case Right(IntValue(int)) => {
                        if (int == value) {
                            Some(env)
                        } else {
                            None
                        }
                    }
                    case _ => None
                }
            }

            case Real(value) => {
                val got = comparison.get()
                got match {
                    case Right(RealValue(real)) => {
                        if (real == value) {
                            Some(env)
                        } else {
                            None
                        }
                    }
                    case _ => None
                }
            }

            case Quote(value) => {
                val got = comparison.get()
                got match {
                    case Right(StringValue(quote)) => {
                        if (quote == value) {
                            Some(env)
                        } else {
                            None
                        }
                    }
                    case _ => None
                }
            }

            case Tuple(patternExprs) => {
                val got = comparison.get()
                got match {
                    case Right(TupleValue(lazyVals)) => {
                        if (patternExprs.length == lazyVals.length) {
                            def f(acc: Option[Environment], curr: (Expr, LazyValue)): Option[Environment] = {
                                curr match {
                                    case (e, l) => {
                                        if (acc.isDefined) {
                                            matchAndExtendEnvironment(acc.get, e, l)
                                        } else {
                                            None
                                        }
                                    }
                                    case _ => None
                                }
                            }
                            val x: Option[Environment] = Some(env)
                            patternExprs.zip(lazyVals).foldLeft(x)(f)
                        } else {
                            None
                        }
                    }
                    case _ => None
                }
            }

            case Sequence(patternExprs) => {
                val got = comparison.get()
                if (patternExprs.length == 0) {
                    got match {
                        case Right(NilValue()) => Some(env)
                        case _ => None
                    }
                } else {
                    matchAndExtendEnvironment(env, Binop(":", patternExprs(0), Sequence(patternExprs.drop(1))), comparison)
                }
            }

            case Binop(":", l, r) => {
                val got = comparison.get()
                got match {
                    case Right(ConsValue(head, tail)) => {
                        val envAfterHead = matchAndExtendEnvironment(env, l, head)
                        if (envAfterHead.isDefined) {
                            matchAndExtendEnvironment(envAfterHead.get, r, tail)
                        } else {
                            None
                        }
                    }
                    case _ => None
                }
            }

            case Bra(e) => matchAndExtendEnvironment(env, e, comparison)

            case _ => None
        }
    }

    def patternMatch(env: Environment, pattern: Expr, comparison: LazyValue, expr: Expr): Either[Error, Value] = {
        // If the pattern is multiple things, then we convert it.
        // Instead, we want to think of this as a function that takes only
        // a single parameter and, if it is defined to have "multiple parameters",
        // then we return a partial function.
        var pat = pattern
        var e = expr
        pat match {
            case Apply(l, r) => {
                val argument = getLeftmostArgument(pat)
                val partialFun = createPartialFun(pat, e)
                pat = argument
                e = partialFun
            }
            case _ => {}
        }
        
        val maybeNewEnv: Option[Environment] = matchAndExtendEnvironment(env, pat, comparison)
        if (maybeNewEnv.isDefined) {
            val newEnv = maybeNewEnv.get
            evaluate(newEnv, e)
        } else {
            Left(PatternMatchError())
        }
    }

    def evaluate(env: Environment, e: Expr): Either[Error, Value] = {
        e match {
            case Op(name) => Left(NotImplemented())
            case Id(name) => env.find(name).get()

            case Num(value) => Right(IntValue(value))
            case Real(value) => Right(RealValue(value))
            case Quote(quote) => Right(StringValue(quote))

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

            case Where(e, defs) => evaluate(withDefinitions(env, defs), e)

            case PartialFun(pattern, guard, expr) => {
                def func(lazyValue: LazyValue): Either[Error, Value] = {
                    patternMatch(env, pattern, lazyValue, expr)
                }
                Right(FunctionValue(func))
            }
            case Function(alts) => {
                def func(lazyValue: LazyValue): Either[Error, Value] = {
                    def f(acc: Either[Error, Value], curr: PartialFun): Either[Error, Value] = {
                        acc match {
                            case Right(_) => acc // we have already successfully pattern matched
                            case Left(PatternMatchError()) => {
                                // i.e. we haven't successfully matched yet, so we keep going
                                curr match {
                                    case PartialFun(pattern, guard, expr) => patternMatch(env, pattern, lazyValue, expr)
                                }
                            }
                            case Left(_) => acc
                        }
                    }
                    val x: Either[Error, Value] = Left(PatternMatchError())
                    alts.foldLeft(x)(f)
                }
                Right(FunctionValue(func))
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
                Right(ConsValue(LazyValue(() => lValue), LazyValue(() => rValue)))
            }
            case "==" => valuesEqual(lValue, rValue)
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
            case "<=" => {
                lValue match {
                    case Right(l @ IntValue(int1)) => {
                        rValue match {
                            case Right(IntValue(int2)) => Right(BooleanValue(int1 <= int2))
                            case Right(r) => Left(TypeError(s"operands of <= have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 <= real2))
                            case Right(r) => Left(TypeError(s"operands of <= have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
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
                            case Right(r) => Left(TypeError(s"operands of >= have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                            case x => x
                        }
                    }
                    case Right(l @ RealValue(real1)) => {
                        rValue match {
                            case Right(RealValue(real2)) => Right(BooleanValue(real1 >= real2))
                            case Right(r) => Left(TypeError(s"operands of >= have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
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

    def valuesEqual(lValue: Either[Error, Value], rValue: Either[Error, Value]): Either[Error, Value] = {
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
                    case Right(r) => Left(TypeError(s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                    case x => x
                }
            }
            case Right(l @ NilValue()) => {
                rValue match {
                    case Right(NilValue()) => Right(BooleanValue(true))
                    case Right(ConsValue(_, _)) => Right(BooleanValue(false))
                    case Right(r) => Left(TypeError(s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
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
                            Left(TypeError(s"operands of == have mismatched types: lhs is of type ${l.getType()}, rhs is of type ${r.getType()}"))
                        }
                    }
                }
            }
            case Right(x) => Left(NotImplemented())
            case x => x
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
}