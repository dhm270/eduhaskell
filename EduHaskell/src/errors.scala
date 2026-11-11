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

    case class PatternMatchError() extends RuntimeError {
        val message = "pattern matching failed"
    }

    case class ConflictingDefinitionsError(identifiers: Seq[String]) extends RuntimeError {
        val message = s"conflicting definitions for ${identifiers.mkString(", ")}"
    }

    case class SyntaxError(message: String) extends Error {}

    case class TypeError(message: String) extends Error {}

    case class ModuleLoadError(message: String) extends Error {}

    case class ModuleContainsTopLevelExpressionsError() extends Error {
        val message = "file being loaded as a module contains top level expressions, which is illegal"
    }

    case class ModuleHasDuplicatedDefinitionsError(duplicatedName: String) extends Error {
        val message = s"file being loaded has multiple definitions for ${duplicatedName}"
    }
}