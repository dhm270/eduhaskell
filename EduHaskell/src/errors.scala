package eduhaskell

object errors {
    trait Error {
        val message: String
    }

    trait RuntimeError extends Error {}

    case class UndeclaredVariable(ident: String) extends RuntimeError {
        val message = s"variable ${ident} is undefined"
    }

    case class SyntaxError(message: String) extends Error {}
}