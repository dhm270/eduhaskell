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

import LRParser._

object RunEduHaskell {
    var debugLevel = 0

    def main(args: Array[String]): Unit = {
        var fileName = "/dev/console"
        if (!args.isEmpty) {
            var i = 0
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
                    case _ => fileName = arg
                }
                i += 1
            }
        }
        try {
            inputFrom(fileName)
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

    def inputFrom(path: String): Unit = {
        val interactive = path == "/dev/console"
        var state: ParseState = RUNNING
        val input = SourceTextCursor(Path.of(path))
        val scanner = makeScanner(input)

        var replEnv = Environment.empty

        while (state == RUNNING) {
            if (interactive) print("$ ")
            val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)

            parser.reductionOnError = true

            state = parser.run(scanner.next)
            
            state match {
                case ERRONEOUS(message) =>
                    println(message)
                    if (interactive) state = RUNNING
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
                        replEnv = Evaluator.withDefinition(replEnv, instruction.asInstanceOf[Definition])
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