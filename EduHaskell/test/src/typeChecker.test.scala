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

    test("Unify a and a -> b") {
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

    test("Unify a and a -> b") {
        // a
        val t1 = g("a")
        // a -> b
        val t2 = f(g("a"), g("b"))
        assertUnificationFails(t1, t2)
    }

    test("Unify a -> b and b -> a -> c") {
        // a -> b
        val t1 = f(g("a"), g("b"))
        // b -> a -> c
        val t2 = f(
            g("b"),
            f(g("a"), g("c"))
        )
        assertUnificationFails(t1, t2)
    }

    test("Unify (a -> b) -> a and c -> c") {
        // (a -> b) -> a
        val t1 = f(
            f(g("a"), g("b")),
            g("a")
        )
        // c -> c
        val t2 = f(g("c"), g("c"))
        assertUnificationFails(t1, t2)
    }

    test("Unify a -> b -> a and (c -> c) -> b") {
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
}