package eduhaskell

import collection.mutable.Map

import eduhaskell.syntax._

trait Value {
    val source: String
    override lazy val toString: String = source
}

case class IntValue(value: Long) extends Value {
    lazy val source = value.toString
}

case class RealValue(value: Double) extends Value {
    lazy val source = value.toString
}

case class StringValue(value: Quotation) extends Value {
    lazy val source = value.toString
}

case class BooleanValue(value: Boolean) extends Value {
    lazy val source = value.toString
}

case class FunctionValue(func: Value => Value) extends Value {
    lazy val source = "<function value>"
}

class Environment(private var map: Map[String, Value]) {
    def find(ident: String): Value = {
        map(ident)
    }

    def define(ident: String, value: Value): Environment = {
        new Environment(this.map.updated(ident, value))
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