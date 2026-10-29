package eduhaskell

import collection.mutable.Map

import eduhaskell.syntax._
import eduhaskell.errors._

trait Value {
    val source: String
    override lazy val toString: String = source
    def isNumber(): Boolean = false

    def getType(): String = "Unknown"
}

case class IntValue(value: Long) extends Value {
    lazy val source = value.toString
    override def isNumber(): Boolean = true
    override def getType(): String = "Integer"
}

case class RealValue(value: Double) extends Value {
    lazy val source = value.toString
    override def isNumber(): Boolean = true
    override def getType(): String = "Real"
}

case class StringValue(value: Quotation) extends Value {
    lazy val source = value.toString
    override def getType(): String = "String"
}

case class BooleanValue(value: Boolean) extends Value {
    lazy val source = value.toString
    override def getType(): String = "Boolean"
}

case class FunctionValue(func: Value => Either[Error, Value]) extends Value {
    lazy val source = "<function value>"
    override def getType(): String = "Function" // TODO: better type
}

case class ConsValue(head: Value, tail: Value) extends Value {
    lazy val source = s"${head.source} : ${tail.source}"
    override def getType(): String = "Cons" // TODO: better type
}

case class NilValue() extends Value {
    lazy val source = "nil"
    override def getType(): String = "Nil" // TODO: better type
}

class Environment(private var map: Map[String, () => Either[Error, Value]]) {
    def find(ident: String): Either[Error, Value] = {
        if (map.isDefinedAt(ident)) {
            map(ident)()
        } else {
            Left(UndeclaredVariable(ident))
        }
    }

    def define(ident: String, value: Value): Environment = {
        new Environment(this.map.updated(ident, () => Right(value)))
    }

    def lazyDefine(ident: String, getter: () => Either[Error, Value]): Environment = {
        new Environment(this.map.updated(ident, getter))
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