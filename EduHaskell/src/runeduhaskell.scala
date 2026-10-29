//> using jar Assets/scalalrruntime.jar

import java.nio.file.Path

import org.sufrin.scalalr._
import org.sufrin.utility._

import eduhaskell.Components
import eduhaskell.Scanner._
import eduhaskell.Evaluator
import eduhaskell.Environment
import eduhaskell.syntax._

import LRParser._

object RunEduHaskell {
    def main(args: Array[String]): Unit = {
        if (args.isEmpty) {
            inputFrom("/dev/console")
        } else {
            inputFrom(args(0))
        }
    }

    def inputFrom(path: String): Unit = {
        val interactive = path == "/dev/console"
        var state: ParseState = RUNNING
        val input = SourceTextCursor(Path.of(path))
        val scanner = makeScanner(input)

        var replEnv = Environment.empty

        while (state == RUNNING) {
            print("> ")
            val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)

            parser.reductionOnError = true

            state = parser.run(scanner.next)
            
            state match {
                case ERRONEOUS(message) =>
                    println(message)
                    if (interactive) state = RUNNING
                case INSTRUCTED(instruction) =>
                    if (instruction.isInstanceOf[Expr]) {
                        val result = Evaluator.evaluate(replEnv, instruction.asInstanceOf[Expr])
                        result match {
                            case Right(r) => {
                                print("< ")
                                println(r)
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