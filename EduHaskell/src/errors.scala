package eduhaskell

object errors {
    trait Error {
        val message: String
    }

    trait RuntimeError extends Error {}

    case class NotImplemented() extends RuntimeError {
        val message = s"evaluating this type of expression is not yet implemented"
    }

    case class UndeclaredVariable(ident: String) extends RuntimeError {
        val message = s"variable ${ident} is undefined"
    }

    case class DivisionByZero() extends RuntimeError {
        val message = "denominator of division is zero"
    }

    case class SyntaxError(message: String) extends Error {}

    case class TypeError(message: String) extends Error {}
}