//> using jar Assets/scalalrruntime.jar

import java.nio.file.Path

import org.sufrin.scalalr._
import org.sufrin.utility._
import org.sufrin.utility.PrettyPrint._

import eduhaskell.Components
import eduhaskell.Scanner._
import eduhaskell.Evaluator
import eduhaskell.Environment
import eduhaskell.LazyValue
import eduhaskell.syntax._
import eduhaskell.errors._

import LRParser._

object RunEduHaskell {
    var debugLevel = 0

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
                case _ => fileName = arg
            }
            i += 1
        }
        try {
            var replEnv: Environment = Environment.empty
            if (fileName != "") {
                val loadFileResult = loadFile(fileName)
                loadFileResult match {
                    case Right(env) => replEnv = env
                    case Left(e) => {
                        shouldStartRepl = false
                        println("Error while loading file:")
                        println(e.message)
                    }
                }
            }
            if (shouldStartRepl) repl(replEnv)
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
        println("  -d, --debug-level  Configure the level of debug information outputted")
        println("  -h, --help         Display this help and exit")
        println("  -r, --no-repl      Just run the input file; don't also start the REPL.")
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

    def loadFile(path: String): Either[Error, Environment] = {
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

        Evaluator.parseAllDefinitions(Environment.empty, definitions)
    }

    def repl(initialReplEnv: Environment): Unit = {
        var replEnv = initialReplEnv

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
                        val result = Evaluator.evaluate(replEnv, instruction.asInstanceOf[Expr])
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
                    } else if (instruction.isInstanceOf[Definition]) {
                        val extendedEnvironment = Evaluator.withDefinition(replEnv, instruction.asInstanceOf[Definition])
                        extendedEnvironment match {
                            case Right(newEnv) => replEnv = newEnv
                            case Left(e) => {
                                print("! ")
                                println(e.message)
                            }
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