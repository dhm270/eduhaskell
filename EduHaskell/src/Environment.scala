package eduhaskell

import collection.mutable.Map

import eduhaskell.types._
import eduhaskell.values._
import eduhaskell.errors._
import eduhaskell.Evaluator

class Environment(private var map: Map[String, LazyValue], private var functionsInScope: List[String]) {
    def extended(ident: String, lazyValue: LazyValue): Environment = {
        new Environment(this.map.updated(ident, lazyValue), this.functionsInScope)
    }

    def find(ident: String): LazyValue = {
        if (map.isDefinedAt(ident)) {
            map(ident)
        } else {
            LazyValue(() => Evaluator.Result(Left(UndeclaredVariable(ident))))
        }
    }

    def define(ident: String, lazyValue: LazyValue): Environment = {
        this.extended(ident, lazyValue)
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