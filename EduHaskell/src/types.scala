package eduhaskell

object types {
    type Substitution = Seq[(String, Type)]

    trait Type {
        val source: String
        def substitute(map: Substitution): Type
    }

    case class IntegerType() extends Type {
        val source = "Integer"

        def substitute(map: Substitution): Type = this
    }

    case class RealType() extends Type {
        val source = "Real"

        def substitute(map: Substitution): Type = this
    }

    case class CharacterType() extends Type {
        val source = "Character"

        def substitute(map: Substitution): Type = this
    }

    case class BooleanType() extends Type {
        val source = "Boolean"

        def substitute(map: Substitution): Type = this
    }

    case class TupleType(subtypes: Seq[Type]) extends Type {
        val source = subtypes.map(_.source).mkString("(", ", ", ")")

        def substitute(map: Substitution): Type = {
            TupleType(subtypes.map(_.substitute(map)))
        }
    }

    case class SequenceType(subtype: Type) extends Type {
        val source = s"[${subtype.source}]"

        def substitute(map: Substitution): Type = {
            SequenceType(subtype.substitute(map))
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
    }
}