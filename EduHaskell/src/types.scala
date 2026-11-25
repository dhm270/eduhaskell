package eduhaskell

import eduhaskell.errors._
import eduhaskell.syntax._

object types {
    def getFirstError[A](types: Seq[Either[Error, A]]): Either[Error, A] = {
        def f(acc: Either[Error, A], curr: Either[Error, A]): Either[Error, A] = {
            acc match {
                case Left(_) => acc
                case Right(_) => {
                    curr match {
                        case Left(_) => curr
                        case Right(_) => acc
                    }
                }
            }
        }
        types.reduceLeft(f)
    }

    def ifNoTypeErrorsMulti[A, B](unextractedTypes: Seq[Either[Error, A]], callback: Seq[A] => Either[Error, B]): Either[Error, B] = {
        val firstTypeError = getFirstError[A](unextractedTypes)
        firstTypeError match {
            case Right(_) => {
                // i.e. no type errors were found
                def extract(x: Either[Error, A]): A = {
                    x match {
                        case Right(_type) => _type
                    }
                }
                val extractedTypes = unextractedTypes.map(extract)
                callback(extractedTypes)
            }
            case Left(_) => firstTypeError.asInstanceOf[Either[Error, B]]
        }
    }

    def ifNoTypeErrorMulti[A, B](unextractedType: Either[Error, A], callback: A => Either[Error, B]): Either[Error, B] = {
        ifNoTypeErrorsMulti[A, B](List(unextractedType), seq => callback(seq(0)))
    }

    def ifNoTypeErrors[A](unextractedTypes: Seq[Either[Error, A]], callback: Seq[A] => Either[Error, A]): Either[Error, A] = {
        ifNoTypeErrorsMulti[A, A](unextractedTypes, callback)
    }

    def ifNoTypeError[A](unextractedType: Either[Error, A], callback: A => Either[Error, A]): Either[Error, A] = {
        ifNoTypeErrors[A](List(unextractedType), seq => callback(seq(0)))
    }

    type Substitution = Seq[(String, Type)]

    trait Type {
        val source: String
        def substitute(map: Substitution): Type
        def getGenericTypes(): Seq[String]
    }

    case class IntegerType() extends Type {
        val source = "Integer"

        def substitute(map: Substitution): Type = this
        def getGenericTypes(): Seq[String] = List()
    }

    case class RealType() extends Type {
        val source = "Real"

        def substitute(map: Substitution): Type = this
        def getGenericTypes(): Seq[String] = List()
    }

    case class CharacterType() extends Type {
        val source = "Character"

        def substitute(map: Substitution): Type = this
        def getGenericTypes(): Seq[String] = List()
    }

    case class BooleanType() extends Type {
        val source = "Boolean"

        def substitute(map: Substitution): Type = this
        def getGenericTypes(): Seq[String] = List()
    }

    case class TupleType(subtypes: Seq[Type]) extends Type {
        val source = subtypes.map(_.source).mkString("(", ", ", ")")

        def substitute(map: Substitution): Type = {
            TupleType(subtypes.map(_.substitute(map)))
        }
        def getGenericTypes(): Seq[String] = {
            subtypes.flatMap(_.getGenericTypes())
        }
    }

    case class SequenceType(subtype: Type) extends Type {
        val source = s"[${subtype.source}]"

        def substitute(map: Substitution): Type = {
            SequenceType(subtype.substitute(map))
        }
        def getGenericTypes(): Seq[String] = {
            subtype.getGenericTypes()
        }
    }

    case class FunctionType(argument: Type, output: Type) extends Type {
        val source = {
            val argumentSource = argument.source
            if (argumentSource.contains("->")) {
                s"(${argument.source}) -> ${output.source}"
            } else {
                s"${argument.source} -> ${output.source}"
            }
        }

        def substitute(map: Substitution): Type = {
            FunctionType(argument.substitute(map), output.substitute(map))
        }
        def getGenericTypes(): Seq[String] = {
            argument.getGenericTypes() ++ output.getGenericTypes()
        }
    }

    case class GenericType(name: String) extends Type {
        val source = name

        def substitute(map: Substitution): Type = {
            var t: Type = this
            var i = 0
            while (i < map.length) {
                t match {
                    case GenericType(tName) => {
                        val (varName, subst) = map(i)
                        if (varName == tName) {
                            t = subst
                        }
                    }
                    case _ => {
                        t = t.substitute(map.drop(i + 1))
                        i = map.length // break out of loop
                    }
                }
                i += 1
            }
            t
        }
        def getGenericTypes(): Seq[String] = List(name)
    }

    object Type {
        def syntaxToType(e: Expr): Either[Error, Type] = {
            e match {
                case Id(name) => {
                    name match {
                        case "Integer" => Right(IntegerType())
                        case "Real" => Right(RealType())
                        case "Character" => Right(CharacterType())
                        case "Boolean" => Right(BooleanType())
                        case _ => Right(GenericType(name))
                    }
                }
                case Tuple(exprs) => {
                    if (exprs.length == 1) {
                        syntaxToType(exprs(0))
                    } else {
                        val subtypes = exprs.map(syntaxToType)
                        ifNoTypeErrors[Type](subtypes, subtypesExtracted => Right(TupleType(subtypesExtracted)))
                    }
                }
                case Sequence(exprs) => {
                    if (exprs.length == 1) {
                        val subtypes = exprs.map(syntaxToType)
                        ifNoTypeErrors[Type](subtypes, subtypesExtracted => Right(SequenceType(subtypesExtracted(0))))
                    } else {
                        Left(SyntaxError(s"${exprs.mkString("[", ", ", "]")} does not define a valid type"))
                    }
                }
                case Binop("->", l, r) => {
                    val leftSubtype = syntaxToType(l)
                    val rightSubtype = syntaxToType(r)
                    ifNoTypeErrors[Type](
                        List(leftSubtype, rightSubtype),
                        subtypesExtracted => {
                            Right(FunctionType(subtypesExtracted(0), subtypesExtracted(1)))
                        }
                    )
                }
                case Bra(expr) => syntaxToType(expr)
                case _ => Left(SyntaxError(s"${e} does not define a valid type"))
            }
        }
    }
}