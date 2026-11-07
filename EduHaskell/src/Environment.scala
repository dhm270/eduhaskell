package eduhaskell

import collection.mutable.Map

import eduhaskell.syntax._
import eduhaskell.errors._

trait Value {
    val eagerSource: String
    override lazy val toString: String = eagerSource
    def isNumber(): Boolean = false

    def getType(): String = "Unknown"
}

case class IntValue(value: Long) extends Value {
    val eagerSource = value.toString
    override def isNumber(): Boolean = true
    override def getType(): String = "Integer"
}

case class RealValue(value: Double) extends Value {
    lazy val eagerSource = value.toString
    override def isNumber(): Boolean = true
    override def getType(): String = "Real"
}

case class CharValue(value: Char) extends Value {
    lazy val eagerSource = s"'${value}'"
    override def getType(): String = "Character"
}

case class BooleanValue(value: Boolean) extends Value {
    lazy val eagerSource = value.toString
    override def getType(): String = "Boolean"
}

case class FunctionValue(func: LazyValue => Either[Error, Value]) extends Value {
    lazy val eagerSource = "<function value>"
    override def getType(): String = "Function" // TODO: better type
}

case class ConsValue(head: LazyValue, tail: LazyValue) extends Value {
    lazy val eagerSource = s"${head.eagerSource} : ${tail.eagerSource}"
    override def getType(): String = "Cons" // TODO: better type
}

case class NilValue() extends Value {
    lazy val eagerSource = "nil"
    override def getType(): String = "Nil" // TODO: better type
}

case class TupleValue(vals: Seq[LazyValue]) extends Value {
    lazy val eagerSource = vals.map(_.eagerSource).mkString("(", ", ", ")")
    override def getType(): String = "Tuple" // TODO: better type
}

case class LazyValue(private var getter: () => Either[Error, Value]) {
    lazy val eagerSource: String = {
        val got = this.getter()
        got match {
            case Right(v) => v.eagerSource
            case Left(e) => s"*** Error: ${e.message}"
        }
    }

    def get(): Either[Error, Value] = this.getter()
}

object LazyValue {
    def lazyPrint(v: Value): Unit = {
        v match {
            case ConsValue(h, t) => {
                this.lazyPrint(h)
                print(" : ")
                this.lazyPrint(t)
            }
            case v: Value => {
                print(v)
            }
        }
    }

    def lazyPrint(lv: LazyValue): Unit = {
        var curr = lv
        var printing = true
        while (printing) {
            val got = curr.get()
            got match {
                case Left(e) => {
                    print(s"*** Error: ${e.message}")
                    printing = false
                }
                case Right(v) => {
                    v match {
                        case ConsValue(h, t) => {
                            this.lazyPrint(h)
                            print(" : ")
                            curr = t
                        }
                        case v: Value => {
                            print(v)
                            printing = false
                        }
                    }
                }
            }
        }
    }
}

class Environment(private var map: Map[String, LazyValue], private var functionsInScope: List[String]) {
    def extended(ident: String, lazyValue: LazyValue): Environment = {
        new Environment(this.map.updated(ident, lazyValue), this.functionsInScope)
    }

    def find(ident: String): LazyValue = {
        if (map.isDefinedAt(ident)) {
            map(ident)
        } else {
            LazyValue(() => Left(UndeclaredVariable(ident)))
        }
    }

    def define(ident: String, value: Value): Environment = {
        this.extended(ident, LazyValue(() => Right(value)))
    }

    def lazyDefine(ident: String, lazyValue: LazyValue): Environment = {
        this.extended(ident, lazyValue)
    }

    def defineMultiple(idents: List[String], values: List[Value]): Environment = {
        if (idents.length == values.length) {
            idents.zip(values).foldLeft(this)((acc, curr) => curr match { case (ident, value) => acc.define(ident, value) })
        } else {
            throw new java.lang.IllegalArgumentException("idents must be the same length as values")
        }
    }

    def isFunctionInScope(ident: String): Boolean = this.functionsInScope.contains(ident)

    def registerFunction(ident: String): Unit = {
        if (!this.isFunctionInScope(ident)) {
            this.functionsInScope = ident :: this.functionsInScope
        }
    }
}

object Environment {
    def empty: Environment = new Environment(Map(), List())
}