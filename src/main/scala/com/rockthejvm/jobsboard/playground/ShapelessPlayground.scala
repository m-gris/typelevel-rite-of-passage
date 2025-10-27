package com.rockthejvm.jobsboard.playground

import scala.util.NotGiven
import shapeless3.deriving.*
import shapeless3.deriving.K0.*
import shapeless3.deriving.Labelling.apply

/*
 * Here are the key URLs that directly influenced the code:

  Most Helpful/Influential:

  1. Shapeless 3 test file (showed actual K0.ProductGeneric usage):
    - https://github.com/typelevel/shapeless-3/blob/main/modules/deriving/src/test/scala/shapeless3/deriving/deriving.scala
    - Showed toRepr method and how to work with K0.ProductGeneric
  2. Shapeless 3 kinds.scala (API reference):
    - https://github.com/typelevel/shapeless-3/blob/main/modules/deriving/src/main/scala/shapeless3/deriving/kinds.scala
    - Showed complete K0.ProductInstances API including foldRight, construct, map, etc.
  3. Scala 3 Tuple.scala source (for Tuple.Concat):
    - https://github.com/scala/scala3/blob/main/library/src/scala/Tuple.scala
    - Showed built-in Tuple.Concat type and ++ operator

  Moderately Helpful:

  4. Xebia blog on Shapeless 3 derivation:
    - https://xebia.com/blog/how-to-derive-type-class-instances-with-shapeless-3/
    - Showed examples of deriving Monoid/Show with K0.ProductInstances
  5. StackOverflow on K0.ProductInstances:
    - https://stackoverflow.com/questions/72235395/using-k0-productinstances-in-shapeless3
    - Confirmed API usage patterns

  Why these mattered:

  - #1 & #2 were critical for understanding the actual Shapeless 3 API (not Shapeless 2!)
  - #3 saved us from reimplementing Tuple.Concat
  - #4 showed the general pattern of type class derivation with K0

 * */

object ShapelessPlayground {

  // Step 4: Building a Flattener Type Class
  // Strategy: recursively convert nested case classes → fully flat tuples
  // Example: Person(name, age, Address(street, city)) → (name, age, street, city)

  // GOOD NEWS: Scala 3 provides Tuple.Concat out of the box!
  // Type level: Tuple.Concat[X, Y] or X ++ Y (infix)
  // Runtime: tuple1 ++ tuple2
  // Example: (String, Int) ++ (Boolean, Char) = (String, Int, Boolean, Char)

  // ============================================================================
  // TYPE-LEVEL HELPER: FlattenTuple
  // ============================================================================
  // Purpose: Recursively flatten a tuple of tuples into a single flat tuple
  //
  // Why needed:
  //   When deriving Flattener[Person], we get individual Flattener instances:
  //     - Flattener[String]  with Out = Tuple1[String]
  //     - Flattener[Int]     with Out = Tuple1[Int]
  //     - Flattener[Address] with Out = (String, String)
  //
  //   These Out types form a tuple: (Tuple1[String], Tuple1[Int], (String, String))
  //   We need to flatten this to: (String, Int, String, String)
  //
  // Strategy: Recursively concatenate each tuple element with the flattened tail
  //   Base case: EmptyTuple → EmptyTuple
  //   Recursive case: (head *: tail) → head ++ FlattenTuple[tail]
  //
  // Example expansion:
  //   FlattenTuple[(Tuple1[String], Tuple1[Int], (String, String))]
  //   = Tuple1[String] ++ FlattenTuple[(Tuple1[Int], (String, String))]
  //   = Tuple1[String] ++ (Tuple1[Int] ++ FlattenTuple[(String, String)])
  //   = Tuple1[String] ++ (Tuple1[Int] ++ ((String, String) ++ EmptyTuple))
  //   = Tuple1[String] ++ (Tuple1[Int] ++ (String, String))
  //   = Tuple1[String] ++ (Int, String, String)
  //   = (String, Int, String, String) ✓
  //
  // Note: Tuple.Concat is built into Scala 3, no manual implementation needed
  type FlattenTuple[T <: Tuple] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case h *: t     => Tuple.Concat[h, FlattenTuple[t]]
  }

  // ============================================================================
  // TYPE CLASS: Flattener
  // ============================================================================
  // Purpose: Convert any value (including nested case classes) to a flat tuple
  //
  // Examples:
  //   - Flattener[Int]        with Out = Tuple1[Int]
  //   - Flattener[String]     with Out = Tuple1[String]
  //   - Flattener[Address]    with Out = (String, String)
  //   - Flattener[Person]     with Out = (String, Int, String, String)
  //
  // Design: Uses dependent types (type Out depends on input type A)
  trait Flattener[A] {
    type Out <: Tuple // The result type: always a tuple (flat or Tuple1)
    def flatten(a: A): Out
  }

  object Flattener {
    // ============================================================================
    // AUX PATTERN
    // ============================================================================
    // Purpose: Extract the dependent type member 'Out' as an explicit type parameter
    //          This makes it available for implicit resolution in subsequent steps
    //
    // Usage:
    //   Flattener.Aux[Address, (String, String)]
    //     instead of:
    //   Flattener[Address] { type Out = (String, String) }
    //
    // Why: The compiler can't always infer type members during implicit resolution
    //      The Aux pattern makes the Out type explicit and solvable
    //
    // Recommended by: "The Type Astronaut's Guide to Shapeless" (dependent types chapter)
    type Aux[A, O <: Tuple] = Flattener[A] { type Out = O }

    // ============================================================================
    // RECURSIVE DERIVATION: Case Classes (Products)
    // ============================================================================
    // Purpose: Automatically derive Flattener for any case class by:
    //   1. Getting a Flattener for each field
    //   2. Recursively flattening nested case classes
    //   3. Concatenating all flattened results
    //
    // Example derivation chain for Person(name: String, age: Int, address: Address):
    //   Flattener[Person] needs:
    //     - Flattener[String]  → primitiveFlattener → Tuple1[String]
    //     - Flattener[Int]     → primitiveFlattener → Tuple1[Int]
    //     - Flattener[Address] → productFlattener (RECURSIVE!) → (String, String)
    //   Result: FlattenTuple[(Tuple1[String], Tuple1[Int], (String, String))]
    //         = (String, Int, String, String) ✓
    //
    // Key components:
    //   - K0.ProductInstances[Flattener, A]: Provides Flattener[T] for each field T
    //   - K0.ProductGeneric[A]: Mirror for case class A (provides field types, toRepr, etc.)
    //
    // IMPORTANT: Must come BEFORE primitiveFlattener in source order
    //            Otherwise, compiler might pick primitiveFlattener for case classes!
    //
    // Note: Using by-name parameter (=>) for inst to prevent implicit divergence
    //       Recommended by "The Type Astronaut's Guide" for recursive derivation
    //       Note: gen must be strict (not by-name) because:
    //         1. We need gen.MirroredElemTypes as a stable type path
    //         2. Scala 3 doesn't have Shapeless 2's Lazy wrapper
    //         3. By-name breaks implicit search for K0.ProductGeneric
    given productFlattener[A](using
        inst: => K0.ProductInstances[Flattener, A], // By-name = lazy evaluation
        gen: K0.ProductGeneric[A]                   // Strict - needed for type access
    ): Flattener[A] =
      new Flattener[A] {
        // --------------------------------------------------------------------------
        // TYPE-LEVEL: Compute Out type at compile-time
        // --------------------------------------------------------------------------
        // Strategy:
        //   1. gen.MirroredElemTypes gives tuple of field types: (String, Int, Address)
        //   2. Tuple.Map[_, Flattener] extracts Out from each Flattener:
        //        (Flattener[String]#Out, Flattener[Int]#Out, Flattener[Address]#Out)
        //      = (Tuple1[String], Tuple1[Int], (String, String))
        //   3. FlattenTuple recursively concatenates: (String, Int, String, String)
        //
        // This all happens at compile-time - zero runtime overhead!
        type Out = FlattenTuple[Tuple.Map[gen.MirroredElemTypes, Flattener]]

        // --------------------------------------------------------------------------
        // RUNTIME: Execute flattening logic
        // --------------------------------------------------------------------------
        def flatten(a: A): Out = {
          // K0.ProductInstances.foldRight signature:
          //   foldRight[Acc](x: A)(init: Acc)(f: [t] => (F[t], t, Acc) => Acc): Acc
          //
          // Parameters:
          //   - x: A           = the case class instance to process
          //   - init: Acc      = starting accumulator (EmptyTuple for us)
          //   - f: [t] => ...  = polymorphic function processing each field
          //
          // Processing: Iterates RIGHT-TO-LEFT over fields
          //   Person("Alice", 30, Address("Main St", "NYC"))
          //   → Process Address → ("Main St", "NYC")
          //   → Process 30      → (30,) ++ ("Main St", "NYC") = (30, "Main St", "NYC")
          //   → Process "Alice" → ("Alice",) ++ (30, ...) = ("Alice", 30, "Main St", "NYC")
          //
          // Note: We must cast to Tuple because ft.Out is an abstract type member
          //       The type system can't prove ft.Out <: Tuple without the cast
          //       This is a known limitation when working with dependent types
          inst
            .foldRight[Tuple](a)(EmptyTuple)(
              [t] =>
                (ft: Flattener[t], elem: t, acc: Tuple) => {
                  // Flatten current field (might be nested case class)
                  val flattened = ft.flatten(elem).asInstanceOf[Tuple]
                  // Concatenate with accumulator
                  (flattened ++ acc).asInstanceOf[Tuple]
              }
            )
            .asInstanceOf[Out] // Final cast to the computed Out type
        }
      }

    // ============================================================================
    // BASE CASE: Primitives (Leaf Types)
    // ============================================================================
    // Purpose: Handle types that cannot be further decomposed (Int, String, Boolean, etc.)
    //
    // Strategy: Wrap primitives in Tuple1 for consistency
    //   Why Tuple1? To maintain homogeneity - all Flattener.Out types are tuples
    //   This allows us to use ++ operator uniformly in productFlattener
    //
    // Examples:
    //   - Int       → Tuple1[Int]
    //   - String    → Tuple1[String]
    //   - Boolean   → Tuple1[Boolean]
    //   - Option[T] → Tuple1[Option[T]]  (treated as atomic, not decomposed)
    //
    // Priority Control: Uses NotGiven[K0.ProductInstances[Flattener, A]]
    //   This ensures primitiveFlattener is ONLY selected when:
    //     - A is NOT a case class (no ProductInstances available)
    //     - productFlattener cannot match
    //
    // Why NotGiven is needed:
    //   Without it, both productFlattener and primitiveFlattener could match String
    //   The compiler would report ambiguous implicit resolution
    //   NotGiven[...] creates a negative constraint: "only if productFlattener CAN'T apply"
    //
    // Recommended by: "The Type Astronaut's Guide to Shapeless" (type class derivation pattern)
    given primitiveFlattener[A](using
        NotGiven[K0.ProductInstances[Flattener, A]]
    ): Flattener.Aux[A, Tuple1[A]] =
      new Flattener[A] {
        type Out = Tuple1[A]
        def flatten(a: A): Tuple1[A] = Tuple1(a)
      }
  }

  def main(args: Array[String]): Unit = {
    println("=== Shapeless Tutorial ===")

    // Step 1: The Problem
    // We have nested case classes (DDD style)
    case class Person(name: String, age: Int, address: Address)
    case class Address(street: String, city: String)

    val person = Person("Alice", 30, Address("Main St", "NYC"))

    // Goal: Convert to flat representation for DB/SQL
    // Person("Alice", 30, Address("Main St", "NYC"))
    //   ↓
    // ("Alice", 30, "Main St", "NYC")

    println(s"Nested: $person")
    println("We want to flatten this automatically...")

    println("\n=== Step 2: Understanding K0.ProductGeneric ===")

    // LESSON LEARNED FROM DOCS:
    // - K0.ProductGeneric is NOT meant to be instantiated manually
    // - It's a type constraint provided by the compiler automatically
    // - Main use: building type class derivation (like Show, Monoid, etc.)
    // - Key method: toRepr - converts case class → tuple

    // K0 = "Kind-0" = works with simple types (not type constructors like List[_])
    // ProductGeneric = type alias for Mirror.Product (Scala 3 native)

    // Let's use it correctly:
    val addressGen = summon[K0.ProductGeneric[Address]]
    // Compiler automatically provides this - no .derived needed!

    val address      = Address("Main St", "NYC")
    val addressTuple = addressGen.toRepr(address)
    // toRepr converts: Address("Main St", "NYC") → ("Main St", "NYC")

    println(s"Original: $address")
    println(s"As tuple: $addressTuple")
    // Output: ("Main St", "NYC")

    println("\n=== Step 3: Flattening Nested Case Classes ===")

    // Now for the real challenge: Person contains Address (nested!)
    // Person("Alice", 30, Address("Main St", "NYC"))
    //   → ("Alice", 30, Address(...))  ← still nested!
    //   → ("Alice", 30, "Main St", "NYC")  ← fully flat! (what we want)

    val personGen   = summon[K0.ProductGeneric[Person]]
    val personTuple = personGen.toRepr(person)

    println(s"Person as tuple: $personTuple")
    // Output: ("Alice", 30, Address("Main St", "NYC"))
    // ⚠️ Problem: Address is NOT flattened! It's still a case class inside the tuple

    println("We need to recursively flatten nested structures...")

    println("\n=== Step 4A: Pragmatic Approach (Manual Flattening) ===")

    // For our specific case (Person + Address), we can manually flatten:
    // Strategy: convert each nested piece separately, then concatenate tuples

    val personRepr = personGen.toRepr(person)
    // personRepr: (String, Int, Address) = ("Alice", 30, Address(...))

    // Extract the fields manually
    val name = personRepr._1 // "Alice"
    val age  = personRepr._2 // 30
    val addr = personRepr._3 // Address("Main St", "NYC")

    // Flatten the nested Address
    val addrRepr = addressGen.toRepr(addr) // ("Main St", "NYC")

    // Concatenate tuples: (name, age) ++ (street, city)
    val flatPerson = (name, age) ++ addrRepr
    // Result: ("Alice", 30, "Main St", "NYC") ✓ FLAT!

    println(s"Manually flattened: $flatPerson")
    println(s"Type: ${flatPerson.getClass.getSimpleName}")

    println("\nPros: Simple, explicit, works immediately")
    println("Cons: Brittle - breaks if Person structure changes")
    println("      Not generic - need new code for each case class")

    println("\n=== Step 4B: Generic Approach (Shapeless Flattener) ===")

    // Now let's use our Flattener type class for automatic flattening
    // The compiler will recursively derive instances for nested structures

    // For Address (no nesting):
    val addressFlattener = summon[Flattener[Address]]
    val flatAddr         = addressFlattener.flatten(address)
    println(s"Address flattened generically: $flatAddr")
    println(s"  Type: ${flatAddr.getClass.getSimpleName}")

    // For Person (has nesting) - the MAGIC happens here!
    // Compiler will:
    // 1. See we need Flattener[Person]
    // 2. Use productFlattener given
    // 3. Recursively derive Flattener[String], Flattener[Int], Flattener[Address]
    // 4. Flattener[Address] uses productFlattener again → Flattener[String], Flattener[String]
    // 5. All primitives use primitiveFlattener
    // 6. Type-level: computes (String, Int, String, String)
    val personFlattener   = summon[Flattener[Person]]
    val flatPersonGeneric = personFlattener.flatten(person)

    println(s"\nPerson flattened generically: $flatPersonGeneric")
    println(s"  Type: ${flatPersonGeneric.getClass.getSimpleName}")

    // Compare by converting both to strings (types are complex for direct ==)
    println(s"  Manual result:  $flatPerson")
    println(s"  Generic result: $flatPersonGeneric")
    println(s"  Match: ${flatPerson.toString == flatPersonGeneric.toString}")

    println("\n✅ SUCCESS! Fully generic flattening works!")
    println("Key achievement: Compiler automatically flattened nested Person → flat tuple!")
    println("No manual field enumeration needed!")

    println("\n" + "=" * 70)
    println("SUMMARY: What We Learned (Path B - Deep Shapeless 3 Dive)")
    println("=" * 70)

    println("""
    |1. TYPE-LEVEL PROGRAMMING:
    |   - Match types (T match { case ... }) for compile-time computation
    |   - FlattenTuple recursively concatenates tuple types
    |   - Tuple.Concat is built into Scala 3 (no need to implement!)
    |
    |2. K0.ProductInstances API:
    |   - Provides Flattener instance for EACH field of a case class
    |   - foldRight iterates over fields at runtime, building result
    |   - Compiler recursively derives instances (Address → Person → ...)
    |
    |3. GIVEN RESOLUTION PRIORITY:
    |   - NotGiven[...] creates negative constraints
    |   - primitiveFlattener only applies when productFlattener can't
    |   - Order matters: more specific givens should come first
    |
    |4. TYPE-LEVEL vs RUNTIME:
    |   - type Out = ... computes result type at compile-time
    |   - def flatten(...) executes at runtime using foldRight
    |   - Both must align perfectly!
    |
    |5. KEY INSIGHT:
    |   - Shapeless 3 is MUCH simpler than Shapeless 2
    |   - Built on Scala 3 native features (Mirror, Tuple, match types)
    |   - Zero runtime overhead - all derivation happens at compile-time
    |
    |6. TRADE-OFFS:
    |   Path A (Manual):  Simple, explicit, brittle
    |   Path B (Generic): Complex, automatic, robust to changes
    |
    |   For production: Start with Path A, graduate to Path B if needed
    """.stripMargin)

    println("=" * 70)
  }
}
