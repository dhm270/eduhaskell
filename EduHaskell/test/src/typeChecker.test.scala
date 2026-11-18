package eduhaskell

import eduhaskell.TypeChecker
import eduhaskell.types._

class TypeCheckerTests extends munit.FunSuite {
    def assertUnification(t1: Type, t2: Type, exp: Type): Unit = {
        val unification = TypeChecker.unify(t1, t2)
        unification match {
            case Right((t, s)) => assert(t == exp)
            case _ => assert(false)
        }
    }

    def assertUnificationFails(t1: Type, t2: Type): Unit = {
        val unification = TypeChecker.unify(t1, t2)
        unification match {
            case Left(_) => assert(true)
            case _ => assert(false)
        }
    }

    def f(t1: Type, t2: Type): Type = FunctionType(t1, t2)
    def g(name: String): Type = GenericType(name)

    def int(): Type = IntegerType()
    def real(): Type = RealType()
    def char(): Type = CharacterType()
    def bool(): Type = BooleanType()
    
    def tuple(subtypes: Seq[Type]): Type = TupleType(subtypes)
    def seq(subtype: Type): Type = SequenceType(subtype)

    test("Unify a and a") {
        // a
        val t1 = g("a")
        // a
        val t2 = g("a")
        // a
        val exp = g("a")
        assertUnification(t1, t2, exp)
    }

    test("Unify a and b") {
        // a
        val t1 = g("a")
        // b
        val t2 = g("b")
        // a
        val exp = g("a")
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify a and a -> b") {
        // a
        val t1 = g("a")
        // a -> b
        val t2 = f(g("a"), g("b"))
        assertUnificationFails(t1, t2)
    }

    test("Unify a -> b and a -> b") {
        // a -> b
        val t1 = f(g("a"), g("b"))
        // a -> b
        val t2 = f(g("a"), g("b"))
        // a -> b
        val exp = f(g("a"), g("b"))
        assertUnification(t1, t2, exp)
    }

    test("Unify a -> a and b -> c") {
        // a -> a
        val t1 = f(g("a"), g("a"))
        // b -> c
        val t2 = f(g("b"), g("c"))
        // b -> b
        val exp = f(g("b"), g("b"))
        assertUnification(t1, t2, exp)
    }

    test("Unify a -> b -> b and (c -> c) -> d") {
        // a -> b -> b
        val t1 = f(
            g("a"),
            f(g("b"), g("b"))
        )
        // (c -> c) -> d
        val t2 = f(
            f(g("c"), g("c")),
            g("d")
        )
        // (c -> c) -> (b -> b)
        val exp = f(
            f(g("c"), g("c")),
            f(g("b"), g("b"))
        )
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify a -> a -> a and b -> b") {
        // a -> a -> a
        val t1 = f(
            g("a"),
            f(g("a"), g("a"))
        )
        // b -> b
        val t2 = f(g("b"), g("b"))
        assertUnificationFails(t1, t2)
    }

    test("Unify a -> c -> c and (b -> d) -> a") {
        // a -> c -> c
        val t1 = f(
            g("a"),
            f(g("c"), g("c"))
        )
        // (b -> d) -> a
        val t2 = f(
            f(g("b"), g("d")),
            g("a")
        )
        // (b -> b) -> b -> b
        val exp = f(
            f(g("b"), g("b")),
            f(g("b"), g("b"))
        )
        assertUnification(t1, t2, exp)
    }

    test("Unify a -> b -> a and c -> c -> d") {
        // a -> b -> a
        val t1 = f(
            g("a"),
            f(g("b"), g("a"))
        )
        // c -> c -> d
        val t2 = f(
            g("c"),
            f(g("c"), g("d"))
        )
        // c -> c -> c
        val exp = f(
            g("c"),
            f(g("c"), g("c"))
        )
        assertUnification(t1, t2, exp)
    }

    test("Unify (a -> b) -> a and c -> d -> d") {
        // (a -> b) -> a
        val t1 = f(
            f(g("a"), g("b")),
            g("a")
        )
        // c -> d -> d
        val t2 = f(
            g("c"),
            f(g("d"), g("d"))
        )
        // ((d -> d) -> b) -> d -> d
        val exp = f(
            f(
                f(g("d"), g("d")),
                g("b")
            ),
            f(g("d"), g("d"))
        )
        assertUnification(t1, t2, exp)
    }

    test("Unify ((a -> b) -> c) -> a -> c and (d -> e) -> (f -> f) -> g") {
        // ((a -> b) -> c) -> a -> c
        val t1 = f(
            f(
                f(g("a"), g("b")),
                g("c")
            ),
            f(g("a"), g("c"))
        )
        // (d -> e) -> (f -> f) -> g
        val t2 = f(
            f(g("d"), g("e")),
            f(
                f(g("f"), g("f")),
                g("g")
            )
        )
        // (((f -> f) -> b) -> c) -> (f -> f) -> c
        val exp = f(
            f(
                f(
                    f(g("f"), g("f")),
                    g("b")
                ),
                g("c")
            ),
            f(
                f(g("f"), g("f")),
                g("c")
            )
        )
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify a and a -> b") {
        // a
        val t1 = g("a")
        // a -> b
        val t2 = f(g("a"), g("b"))
        assertUnificationFails(t1, t2)
    }

    test("Fail to unify a -> b and b -> a -> c") {
        // a -> b
        val t1 = f(g("a"), g("b"))
        // b -> a -> c
        val t2 = f(
            g("b"),
            f(g("a"), g("c"))
        )
        assertUnificationFails(t1, t2)
    }

    test("Fail to unify (a -> b) -> a and c -> c") {
        // (a -> b) -> a
        val t1 = f(
            f(g("a"), g("b")),
            g("a")
        )
        // c -> c
        val t2 = f(g("c"), g("c"))
        assertUnificationFails(t1, t2)
    }

    test("Fail to unify a -> b -> a and (c -> c) -> b") {
        // a -> b -> a
        val t1 = f(
            g("a"),
            f(g("b"), g("a"))
        )
        // (c -> c) -> b
        val t2 = f(
            f(g("c"), g("c")),
            g("b")
        )
        assertUnificationFails(t1, t2)
    }

    test("Unify (a -> b) -> (b -> c) -> c and (d -> d) -> (e -> e) -> f") {
        // (a -> b) -> (b -> c) -> c
        val t1 = f(
            f(g("a"), g("b")),
            f(
                f(g("b"), g("c")),
                g("c")
            )
        )
        // (d -> d) -> (e -> e) -> f
        val t2 = f(
            f(g("d"), g("d")),
            f(
                f(g("e"), g("e")),
                g("f")
            )
        )
        // (c -> c) -> (c -> c) -> c
        val exp = f(
            f(g("c"), g("c")),
            f(
                f(g("c"), g("c")),
                g("c")
            )
        )
        assertUnification(t1, t2, exp)
    }

    test("Unify a -> Integer and Boolean -> b") {
        // a -> Integer
        val t1 = f(g("a"), int())
        // Boolean -> b
        val t2 = f(bool(), g("b"))
        // Boolean -> Integer
        val exp = f(bool(), int())
        assertUnification(t1, t2, exp)
    }

    test("Unify a -> b -> c and Integer -> Real -> Character") {
        // a -> b -> c
        val t1 = f(
            g("a"),
            f(g("b"), g("c"))
        )
        // Integer -> Real -> Character
        val t2 = f(
            int(),
            f(real(), char())
        )
        // Integer -> Real -> Character
        val exp = f(
            int(),
            f(real(), char())
        )
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify Integer -> a and Real -> Boolean") {
        // Integer -> a
        val t1 = f(int(), g("a"))
        // Real -> Boolean
        val t2 = f(real(), bool())
        assertUnificationFails(t1, t2)
    }

    test("Fail to unify a -> a and Integer -> Boolean") {
        // a -> a
        val t1 = f(g("a"), g("a"))
        // Integer -> Boolean
        val t2 = f(int(), bool())
        assertUnificationFails(t1, t2)
    }

    test("Unify (a, Integer, b) and (Boolean, c, Real)") {
        // (a, Integer, b)
        val t1 = tuple(List(
            g("a"), int(), g("b")
        ))
        // (Boolean, c, Real)
        val t2 = tuple(List(
            bool(), g("c"), real()
        ))
        // (Boolean, Integer, Real)
        val exp = tuple(List(
            bool(), int(), real()
        ))
        assertUnification(t1, t2, exp)
    }

    test("Unify (a, a, b) and (Integer, c, Boolean)") {
        // (a, a, b)
        val t1 = tuple(List(
            g("a"), g("a"), g("b")
        ))
        // (Integer, c, Boolean)
        val t2 = tuple(List(
            int(), g("c"), bool()
        ))
        // (Integer, Integer, Boolean)
        val exp = tuple(List(
            int(), int(), bool()
        ))
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify (a, b) and (Integer, Boolean, Character)") {
        // (a, b)
        val t1 = tuple(List(
            g("a"), g("b")
        ))
        // (Integer, Boolean, Character)
        val t2 = tuple(List(
            int(), bool(), char()
        ))
        assertUnificationFails(t1, t2)
    }

    test("Unify (a, (b, c), d) and ((Integer, Boolean), (Real, c), Character)") {
        // (a, (b, c), d)
        val t1 = tuple(List(
            g("a"),
            tuple(List(g("b"), g("c"))),
            g("d")
        ))
        // ((Integer, Boolean), (Real, e), Character)
        val t2 = tuple(List(
            tuple(List(int(), bool())),
            tuple(List(real(), g("c"))),
            char()
        ))
        // ((Integer, Boolean), (Real, c), Character)
        val exp = tuple(List(
            tuple(List(int(), bool())),
            tuple(List(real(), g("c"))),
            char()
        ))
        assertUnification(t1, t2, exp)
    }

    test("Unify [a] and [Integer]") {
        // [a]
        val t1 = seq(g("a"))
        // [Integer]
        val t2 = seq(int())
        // [Integer]
        val exp = seq(int())
        assertUnification(t1, t2, exp)
    }

    test("Unify [[a]] and [[Boolean]]") {
        // [[a]]
        val t1 = seq(seq(g("a")))
        // [[Boolean]]
        val t2 = seq(seq(bool()))
        // [[Boolean]]
        val exp = seq(seq(bool()))
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify [Integer] and [Real]") {
        // [Integer]
        val t1 = seq(int())
        // [Real]
        val t2 = seq(real())
        assertUnificationFails(t1, t2)
    }

    test("Unify a -> [a] and [Integer] -> b") {
        // a -> [a]
        val t1 = f(
            g("a"),
            seq(g("a"))
        )
        // [Integer] -> b
        val t2 = f(
            seq(int()),
            g("b")
        )
        // [Integer] -> [[Integer]]
        val exp = f(
            seq(int()),
            seq(seq(int()))
        )
        assertUnification(t1, t2, exp)
    }

    test("Unify [(a, b)] and [(Integer, Boolean)]") {
        // [(a, b)]
        val t1 = seq(
            tuple(List(g("a"), g("b")))
        )
        // [(Integer, Boolean)]
        val t2 = seq(
            tuple(List(int(), bool()))
        )
        // [(Integer, Boolean)]
        val exp = seq(
            tuple(List(int(), bool()))
        )
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify [a] and (Integer, Boolean)") {
        // [a]
        val t1 = seq(g("a"))
        // (Integer, Boolean)
        val t2 = tuple(List(
            int(), bool()
        ))
        assertUnificationFails(t1, t2)
    }

    test("Unify (a, b) -> [a] and (Integer, Boolean) -> c") {
        // (a, b) -> [a]
        val t1 = f(
            tuple(List(g("a"), g("b"))),
            seq(g("a"))
        )
        // (Integer, Boolean) -> c
        val t2 = f(
            tuple(List(int(), bool())),
            g("c")
        )
        // (Integer, Boolean) -> [Integer]
        val exp = f(
            tuple(List(int(), bool())),
            seq(int())
        )
        assertUnification(t1, t2, exp)
    }

    test("Fail to unify a and [a]") {
        // a
        val t1 = g("a")
        // [a]
        val t2 = seq(g("a"))
        assertUnificationFails(t1, t2)
    }

    test("Fail to unify a and (Integer, a)") {
        // a
        val t1 = g("a")
        // (Integer, a)
        val t2 = tuple(List(
            int(), g("a")
        ))
        assertUnificationFails(t1, t2)
    }

    test("Unify ([a], b -> c) and (d, Integer -> [a])") {
        // ([a], b -> c)
        val t1 = tuple(List(
            seq(g("a")),
            f(g("b"), g("c"))
        ))
        // (d, Integer -> [a])
        val t2 = tuple(List(
            g("d"),
            f(int(), seq(g("a")))
        ))
        // ([a], Integer -> [a])
        val exp = tuple(List(
            seq(g("a")),
            f(int(), seq(g("a")))
        ))
        assertUnification(t1, t2, exp)
    }
}