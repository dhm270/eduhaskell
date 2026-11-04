package eduhaskell

import collection.mutable.Map

import eduhaskell.syntax._
import eduhaskell.errors._

trait Value {
    val eagerSource: String
    override lazy val toString: String = eagerSource
    def isNumber(): Boolean = false
    def lazyPrint(): Unit = print(this.eagerSource)

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

case class StringValue(value: Quotation) extends Value {
    lazy val eagerSource = value.toString
    override def getType(): String = "String"
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

    override def lazyPrint(): Unit = {
        head.lazyPrint()
        print(" : ")
        tail.lazyPrint()
    }
}

case class NilValue() extends Value {
    lazy val eagerSource = "nil"
    override def getType(): String = "Nil" // TODO: better type
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

    def lazyPrint(): Unit = {
        val got = this.getter()
        got match {
            case Right(v) => v.lazyPrint()
            case Left(e) => s"*** Error: ${e.message}"
        }
    }
}

class Environment(private var map: Map[String, LazyValue]) {
    def find(ident: String): LazyValue = {
        if (map.isDefinedAt(ident)) {
            map(ident)
        } else {
            LazyValue(() => Left(UndeclaredVariable(ident)))
        }
    }

    def define(ident: String, value: Value): Environment = {
        new Environment(this.map.updated(ident, LazyValue(() => Right(value))))
    }

    def lazyDefine(ident: String, lazyValue: LazyValue): Environment = {
        new Environment(this.map.updated(ident, lazyValue))
    }

    def defineMultiple(idents: List[String], values: List[Value]): Environment = {
        if (idents.length == values.length) {
            idents.zip(values).foldLeft(this)((acc, curr) => curr match { case (ident, value) => acc.define(ident, value) })
        } else {
            // TODO
            this
        }
    }
}

object Environment {
    def empty: Environment = new Environment(Map())
}