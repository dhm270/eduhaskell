//> using jar Assets/scalalrruntime.jar

import java.nio.file.Path

import org.sufrin.scalalr._
import org.sufrin.utility._
import org.sufrin.utility.PrettyPrint._

import eduhaskell.Components
import eduhaskell.Scanner._
import eduhaskell.values._
import eduhaskell.syntax._
import eduhaskell.errors._
import eduhaskell.types._

import eduhaskell.Evaluator
import eduhaskell.Environment
import eduhaskell.TypeEnvironment
import eduhaskell.TypeChecker

import LRParser._

object Main {
    var debugLevel = 0
    var shouldTypeCheck = true
    var shouldSimplifyTypes = true

    def main(args: Array[String]): Unit = {
        var fileName = ""
        var i = 0
        var shouldStartRepl = true
        while (i < args.length) {
            val arg = args(i)
            arg match {
                case "-d" | "--debug-level" =>
                    if (i + 1 < args.length) {
                        this.debugLevel = args(i + 1).toInt
                        i += 1
                    } else {
                        return printUsage()
                    }
                case "-h" | "--help" =>
                    return printUsage()
                case "-r" | "--no-repl" =>
                    shouldStartRepl = false
                case "-s" | "--no-simplify-types" =>
                    shouldSimplifyTypes = false
                case "-u" | "--no-type-check" =>
                    shouldTypeCheck = false
                case _ => fileName = arg
            }
            i += 1
        }
        try {
            var replEnv: Environment = Environment.empty
            var replTypeEnv: TypeEnvironment = TypeEnvironment.empty
            if (fileName != "") {
                val loadFileResult = loadFile(fileName)
                loadFileResult match {
                    case (Right((env, names)), Right(typeEnv)) => {
                        replEnv = env
                        replTypeEnv = typeEnv
                    }
                    case (Right((env, names)), Left(NotImplemented())) => {
                        replEnv = env
                    }
                    case (Left(e), _) => {
                        shouldStartRepl = false
                        println("Error while loading file:")
                        println(e.message)
                    }
                    case (_, Left(e)) => {
                        shouldStartRepl = false
                        println("Error while loading file:")
                        println(e.message)
                    }
                }
            }
            if (shouldStartRepl) repl(replEnv, replTypeEnv)
        } catch {
            case e: java.nio.file.NoSuchFileException => {
                println(s"Input file '${fileName}' does not exist. Aborting.")
            }
            case e: java.lang.StackOverflowError => {
                println("!!! Fatal exception: maximum recursion depth exceeded.")
            }
        }
    }

    def printUsage(): Unit = {
        println("Usage: eduhaskell [flags-and-files]")
        println("  -d, --debug-level        Configure the level of debug information outputted.")
        println("  -h, --help               Display this help and exit.")
        println("  -r, --no-repl            Just run the input file; don't also start the REPL.")
        println("  -s, --no-simplify-types  Don't simplify e.g. `t34 -> t35` to `a -> b`.")
        println("  -u, --no-type-check      Don't perform any type checking on REPL inputs.")
    }

    def translate(clause: TREE): TREE = {
        import eduhaskell.visitor.Visitors._
        import eduhaskell.visitor.Dispatch._
        clause match {
            case it: Expr => it.visit(DetupleExpression)
            case it: Definition => it.visit(DetupleDefinition)
        }
    }

    def layout(clause: TREE): TREE = {
        import eduhaskell.visitor.Dispatch._
        import eduhaskell.visitor.Formatting._
        val scheme = clause match {
            case it: Expr => ExprFormat(it)
            case it: Definition => DefinitionFormat(it)
        }
        print("Parsed as: ")
        println(scheme.render(40))
        println("with tree:")
        clause
    }

    def loadFile(path: String): (Either[Error, (Environment, Seq[String])], Either[Error, TypeEnvironment]) = {
        var state: ParseState = RUNNING
        val input = SourceTextCursor(Path.of(path))
        val scanner = makeScanner(input)

        var result: Either[Error, Environment] = Left(ModuleLoadError("loading the file did not complete properly"))
        var definitions: List[Definition] = List()

        while (state == RUNNING) {
            val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
            parser.reductionOnError = true

            state = parser.run(scanner.next)

            state match {
                case ERRONEOUS(message) =>
                    result = Left(ModuleLoadError(message))
                case INSTRUCTED(instruction) =>
                    if (instruction.isInstanceOf[Definition]) {
                        definitions = definitions :+ instruction.asInstanceOf[Definition]
                        state = RUNNING
                    } else if (instruction.isInstanceOf[Expr]) {
                        result = Left(ModuleContainsTopLevelExpressionsError())
                    } else {
                        result = Left(ModuleLoadError("Invalid state"))
                    }
                case _ => {}
            }
        }

        (
            Environment.empty.parseAllDefinitions(definitions),
            TypeEnvironment.empty.parseAllDefinitions(definitions)
        )
    }

    def repl(initialReplEnv: Environment, initialReplTypeEnv: TypeEnvironment): Unit = {
        var replEnv = initialReplEnv
        var replTypeEnv = initialReplTypeEnv

        var state: ParseState = RUNNING
        val input = SourceTextCursor(Path.of("/dev/console"))
        val scanner = makeScanner(input)

        while (state == RUNNING) {
            print("$ ")
            val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)

            parser.reductionOnError = true

            state = parser.run(scanner.next)
            
            state match {
                case ERRONEOUS(message) =>
                    println(message)
                    state = RUNNING
                case INSTRUCTED(instruction) =>
                    if (this.debugLevel >= 2) {
                        layout(instruction.asInstanceOf[TREE])
                    }
                    if (this.debugLevel >= 1) {
                        translate(instruction.asInstanceOf[TREE]).prettyPrint()
                    }
                    if (instruction.isInstanceOf[Expr]) {
                        val expr = instruction.asInstanceOf[Expr]
                        def eval(): Unit = {
                            val result = Evaluator.evaluate(replEnv, expr)
                            result match {
                                case Right(r) => {
                                    print("= ")
                                    LazyValue.lazyPrint(r)
                                    println()
                                }
                                case Left(e) => {
                                    print("! ")
                                    println(e.message)
                                }
                            }
                        }
                        if (shouldTypeCheck) {
                            val _type = TypeChecker.getType(replTypeEnv, expr)
                            _type match {
                                case Right(_) | Left(NotImplemented()) => {
                                    // If the type error is just that we haven't implemented type
                                    // checking for this kind of expression yet, then we can still
                                    // go ahead and run it. We have to assume that it's fine.
                                    eval()
                                }
                                case Left(e) => {
                                    print("! Error while type-checking: ")
                                    println(e.message)
                                }
                            }
                        } else {
                            eval()
                        }
                    } else if (instruction.isInstanceOf[Definition]) {
                        val d = instruction.asInstanceOf[Definition]
                        def eval(): Unit = {
                            val extendedEnvironment = replEnv.withDefinition(d)
                            extendedEnvironment match {
                                case Right((newEnv, names)) => replEnv = newEnv
                                case Left(e) => {
                                    print("! ")
                                    println(e.message)
                                }
                            }
                        }
                        if (shouldTypeCheck) {
                            val extendedTypeEnvironment = replTypeEnv.withDefinition(d)
                            def f(env: TypeEnvironment): Unit = {
                                val newDefs = env.difference(replTypeEnv)
                                replTypeEnv = env
                                eval()
                                for ((newDefName, newDefType) <- newDefs) {
                                    print(newDefName)
                                    print(" :: ")
                                    if (shouldSimplifyTypes) {
                                        println(Type.getSimplifiedSource(newDefType))
                                    } else {
                                        println(newDefType.source)
                                    }
                                }
                            }
                            extendedTypeEnvironment match {
                                case Right(newTypeEnv) => f(newTypeEnv)
                                case Left(NotImplemented()) => f(replTypeEnv)
                                case Left(e) => {
                                    print("! Error while type-checking: ")
                                    println(e.message)
                                }
                            }
                        } else {
                            eval()
                        }
                    } else {
                        throw new Exception("Invalid state")
                    }
                    println()
                    state = RUNNING
                case ACCEPTED(()) =>
                    println()
                case _ =>
                    state = RUNNING
            }
        }
    }
}