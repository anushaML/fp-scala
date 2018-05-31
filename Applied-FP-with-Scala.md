# Applied Functional Programming with Scala - Notes

*Copyright &copy; 2016-2018 Fantasyland Institute of Learning. All rights reserved.*

# 1. Mastering Functions

A function is a mapping from one set, called a *domain*, to another set, called the *codomain*. A function associates every element in the domain with exactly one element in the codomain. In Scala, both domain and codomain are *types*.

```scala
val square : Int => Int = x => x * x

square(2) // 4
```

## Higher-Order Functions

A *higher-order* function is a function that *accepts* or *returns* a function.

```scala
trait List[A] {
  def filter(f: A => Boolean): List[A]
}
```

*Example*: `List[A].filter` is a higher-order function that accepts the function `A => Boolean` and returns the value `List[A]`.

## Combinators

*Function combinators* are higher-order functions that accept and return functions. 

```scala
type Conf[A] = ConfigReader => A

def string(name: String): Conf[String] = _.readString(name)

def both(left: Conf[A], right: Conf[B]): Conf[(A, B)] = c => (left(c), right(c))
```

*Example*: `both` is a combinator that takes two functions and returns a function.

## Polymorphic Functions

A polymorphic function is one that is universally quantified over one or more type parameters. Scala has no support for polymorphic functions, but they can be emulated via polymorphic methods on traits. A trait modeling a polymorphic function usually has a single method called `apply`, so it can be applied with ordinary function application syntax.

```scala
case object identity {
  def apply[A](value: A): A = value
}
identity(3)   // 3
identity("3") // "3"
```

*Example*: This emulates a polymorphic function called `id`, which accepts one type parameter `A`, and a value of type `A`, and returns that same value.

# 2. Mastering Types

## Type Theory 101

A *type* is a compile-time description of a *set of values*. `Int` is the set of all integers between -2147483648 and 2147483647. Values *have* types, which is to say, they may be a *member* of the set of values they represent.

```scala
2 : Int
```

*Example*: `2` is a member of the set of all `Int`. Equivalently, `2` has type `Int`.

## Algebraic Data Types

An algebraic data type is a type formed by composing *product* and *sum* types.

### Product Type

Product types are defined by a *Cartesian cross product* on 2 or more types.

```scala
type Point2D = (Int, Int)
```

*Example*: A two-dimensional point is a product of a number and a number; each value has *both* an x-coordinate *and* a y-coordinate.

#### Case Classes

In Scala, case classes are the idiomatic representation of product types. The terms of a case class are identified by *name*.

```scala
case class Person(name: String, age: Int)
```

*Example*: A person has *both* a string (`name`) and an integer (`age`).

### Sum Types

Sum types are defined by a *disjoint union* on 2 or more types.

```scala
type RequestResult = Either[Error, HttpResponse]
```

*Example*: An request result is a sum of `Error` and `HttpResponse`; each value is *either* an error *or* an HTTP response (but not both).

#### Sealed Traits

In Scala, sealed traits are the idiomatic representation of sum types (pre-Dotty). The terms of a sum type are identified by *constructor / deconstructor* (and, incidentally, by *subtype*).

```scala
sealed trait AddressType
case object Home     extends AddressType
case object Business extends AddressType
```

*Example*: An `AddressType` is *either* a `Home` or a `Business`, but not both.

## Subtyping

Type `A` is a subtype of `B` if $A \subseteq B$. In Scala, $A$ must *extend* $B$. The compiler allows subtypes to be used wherever a supertype is required.

```scala
sealed trait Shape {
  def width: Int
  def height: Int
}
case class Rectangle(corner: Point2D, width: Int, height: Int) extends Shape
```

*Example*: A `Rectangle` is a subtype of `Shape`, because every `Rectangle` is a shape (but not every `Shape` is a `Rectangle`).

## Supertyping

Type `A` is a supertype of `B` if $B \subseteq A$. In Scala, $B$ must *extend* $A$. The compiler allows supertypes to be used wherever a subtype is provided.

In the previous example, `Shape` is a supertype of `Rectangle`, and a variable of type `Shape` may be assigned to a value of type `Rectangle`.

## Universals

A universally-quantified type defines a *category* (or *kind*) of types that are all parameterized by some arbitrary type. In Scala, type constructors (such as some traits) and methods may be universally quantified, although Scala methods do not have a type (they *appear* in types such as traits).

## Type Constructors

A type constructor is a universally quantified type, which can be used to construct types.

```
sealed trait List[A]
case class Nil[A]() extends List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
```

*Example*: `List` is type constructor, which defines a family of `List`-like types, including `List[Boolean]` (the type of lists of booleans). `List` is said to be *universally quantified* over its type variable `A`.

## Methods

Polymorphic methods are also referred to as universally-quantified functions, because their domain is universally quantified over all types.

```scala
case object identity {
  def apply[A](value: A): A = value
}
identity(3)   // 3
identity("3") // "3"
```

## Higher-Kinded Types

### Type-Level Functions

Type constructors can be thought of as *type-level functions*, which accept types and return types. This interpretation is useful when reasoning about *higher-kinded types*.

*Example*: `List` is a type-level function that accepts one type `A` (the type of its elements), and returns another type `List[A]`. If you pass `Boolean` to `List`, you get back `List[Boolean]`, the type of lists of boolean values.

### Kinds

Kinds can be thought of as *the type of types*.

 * `*` — The kind of types (the set of all types).
 * `* => *` — The kind of type-level functions that accept 1 type (the set of all type-level functions that accept 1 type). The type constructor `List` has kind `* => *`, represented as `_[_]` in Scala.
 * `[*, *] => *` — The kind of type-level functions that accept 2 types (the set of all type-level functions that accept 2 types). The type constructor `Either` has kind `[*, *] => *`, represented as `_[_, _]` in Scala.

**Note**: Compare with the types of functions: `A => B`, `(A, B) => C`, `(A, B, C) => D`.

### Higher-Order Kinds

Just like functions can be "higher-order", type constructors can be higher-order, too. Scala uses underscores to encode higher-order type constructors. The declaration `trait CollectionModule[Collection[_]]` specifies that `CollectionModule`'s type constructor requires a type constructor of kind `* -> *` (such as `List`).

 * `(* => *) => *` — The kind of type constructors that accept a type constructor of kind `* => *`. For example, `trait Functor[F[_]] { ... }` and `trait Monad[F[_]] { ... }`.

**Note**: All higher-order kinds that return a type (`*`) are valid kinds.

## Existentials

An existentially quantified type defines a type that depends on some definite but unknowable type. Existential types are useful for hiding type information that is not globally relevant.

```scala
trait ListMap[A] {
  type B
  val list : List[B]
  val mapf : B => A
  
  def run : List[A] = list.map(mapf)
}
```

*Example*: The type `ListMap[A]#B` is some definite type, but there is no way to know what that type is — it could be anything.

### Skolemization

Every existential type can be encoded as a universal type. This process is called *skolemization*.

```scala
case class ListMap[B, A](list: List[B], mapf: B => A)

trait ListMapInspector[A, Z] {
  def apply[B](value: ListMap[B, A]): Z
}

case class AnyListMap[A] {
  def apply[Z](value: ListMapInspector[A, Z]): Z
}
```

*Example*: Instead of using `ListMap` directly, we use `AnyListMap`, which allows us to inspect a `ListMap` but only if we can handle any type parameter for `B`.

## Type Lambdas

Functions may be *partially applied* with the underscore operator; e.g. `zip(a, _)`. A type lambda is a way to partially apply a higher-kinded type, which yields another type constructor with fewer type parameters.

Type lambdas are to type constructors as lambdas are to functions. Type constructors and functions are declarations, while lambdas are expressions (either value expressions, or type expressions).

```scala
({type λ[α] = Either[String, α]})#λ
```

*Example*: This is the `Either` type, partially applied with a `String` as the first type parameter.

**Note**: In many (but not all) cases, you can use type aliases instead of type lambdas (e.g. `type EitherString[A] = Either[String, A]`).

### Kind Projector

*Kind Projector* is a common compiler plugin for Scala that provides easier syntax to create type lambdas. For example, the type lambda `({type λ[α] = Either[String, α]})#λ` can be represented with the syntax `Either[String, ?]`. Other syntax can be used to create more complex type lambdas.

<https://github.com/non/kind-projector>

# 3. Mastering Type Classes

A *type class* is a bundle of types and operations defined on them. Most type classes have *laws* that implementations are required to satisfy.

```scala
trait ShowRead[A] {
  def show(v: A): String
  def read(v: String): Either[String, A]
}
object ShowRead {
  def apply[A](implicit v: ShowRead[A]): ShowRead[A] = v
}
```

*Example*: The `ShowRead[A]` type class defines a way of "showing" a type `A` by rendering it to a string, and reading it by parsing it from a string (or producing an error message). 

 * **Right Identity**
 
   ```scala
   read(show(v)) == Right(v)
   ```
 * **Left Partial Identity**

   ```scala
   read(v).map(show(_)).fold(_ => true, _ == v)
   ```

## Instances

A *type class instance*, or simply *instance*, is an implementation of a type class for a given set of types. Such instances are usually made *implicit* so the compiler can thread them through functions that require them.

```scala
implicit val ShowReadString: ShowRead[String] = new ShowRead[String] {
  def show(v: String): String = v
  def read(v: String): Either[String, String] = Right(v)
}

ShowRead[String].show("foo") // foo
```

## Syntax

Convenient syntax, sometimes called *extension methods*, can be added to types to make it easier to use type classes.

```scala
implicit class ShowOps[A: ShowRead](self: A) {
  def show: String = ShowRead[A].show(self)
}
implicit class ReadOps(self: String) {
  def read[A: ShowRead]: Either[String, A] = ShowRead[A].read(self)
}

true.show.read[Boolean] // Right(true)
```

# 4. Mastering Functional Patterns

## Option, Either, Validation

These types are commonly used to describe optionality and partiality.

```scala
sealed trait Maybe[A]
final case class Just [A](value: A) extends Maybe[A]
final case class Empty[A]()         extends Maybe[A]

sealed trait \/[A, B]
final case class -\/ [A, B](value: A) extends \/[A, B]
final case class  \/-[B, B](value: B) extends \/[A, B]

type Either[A, B] = A \/ B

sealed trait Validation[A, B]
final case class Failure[A, B](value: A) extends Validation[A, B]
final case class Success[A, B](value: B) extends Validation[A, B]
```

## Semigroup, Monoid

Semigroups allows combining two things of the same type into another thing of the same type. For example, addition forms a semigroup over integers. Monoids add the additional property of having an "zero" element, which you can append to a value without changing the value.

```scala
trait Semigroup[A] {
  def append(a1: A, a2: A): A
}
trait Monoid[A] extends Semigroup[A] {
  def zero: A
}
```

 * **Associativity**
   
   ```scala
   append(a1, append(a2, a3)) == append(append(a1, a2), a3)
   ```
 * **Identity** 
   
   ```scala
   append(a, zero) == a
   ```

## Functors

A functor `F[_]` is a type constructor of kind `* -> *`. In the most general case, an `F[A]` represents a *recipe* that may halt, run forever, or produce 0 or more `A`'s.

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(ab: A => B): F[B]
}
```

**Note**: Technically, this is a *covariant endofunctor*, and there are many other types of functors, including invariant and contravariant.

 * **Identity**
   
   ```scala
   map(fa)(identity) == fa
   ```
 * **Composition**
   
   ```scala
   map(map(fa)(ab))(bc) == map(fa)(ab.andThen(bc))
   ```

*Example*: `List` is a functor, and `List[Int]` is a trivial description of a computation producing some number of `Int`'s.

### Natural Transformations

A polymorphic function that maps from one functor `F[_]` to another functor `G[_]` is called a *natural transformation*, and is typically denoted using `F ~> G`. These functions are extremely important in higher-order functional programming.

```scala
trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}
type ~> [-F[_], +G[_]] = NaturalTransformation[F, G]
```

**Note**: If you have `F ~> G`, and `G ~> H`, you can compose them to get `F ~> H`. In addition, for any `F`, there is an identity `F ~> F`.

### Functor Composition

Two functors can be composed together in a variety of ways to yield another functor.

#### Natural

The natural composition of two functors occurs when one is nested inside another.

```scala
case class Composite[F[_], G[_], A](run: F[G[A]])
```

#### Product

The product of two functors is a functor.

```scala
case class Product[F[_], G[_], A](run: (F[A], G[A]))
```

#### Coproduct

The sum (or coproduct) of two functors is a functor.

```scala
case class Coproduct[F[_], G[_], A](run: Either[F[A], G[A]])
```

### Lifting

Often, for some value `X`, `F[X]` is referred to as the "lifted" version of `X`, because it is the same value, but placed "inside" of some functor `F`. For example, you can lift the function `x => x * x` inside `List` by writing `List(x => x * x)`.

### Apply

Some functors implement `Apply`, which provides a way of applying a lifted function `F[A => B]` to some lifted value `F[A]` to yield `F[B]`.

```scala
trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]
}
```

 * **Associative Composition**

    ```scala
    ap(ap(fa)(fab))(fbc) ==
       ap(fa)(ap(fab)(map(fbc)(_.compose(_).curry))
    ```
    
### Applicative

Some functors that implement `Apply` also implement `Applicative`, which provides the ability to lift any value into the functor.

```scala
trait Applicative[F[_]] extends Apply[F] {
  def point[A](a: A): F[A]
}
```

 * **Identity**

   ```scala
   ap(fa)(point(_)) == fa
   ```
 * **Homomorphism**
 
   ```scala
   ap(point(a))(point(ab)) == point(ab(a))
   ```
   
 * **Interchange**
   
   ```scala
   ap(point(a))(fab) == ap(fab)(point(_.apply(a)))
   ```
 
 * **Derived Map**
 
   ```scala
   map(fa)(ab) == ap(fa)(point(ab))
   ```
   
### Bind

Some functors that implement `Apply` also implement `Bind`, which adds the ability to extend a recipe `F[A]` with a second recipe that depends on the result of `A` (`A => F[B]`), and collapse the result into a single recipe `F[B]`.

```scala
trait Bind[F[_]] extends Apply[F] {
  def bind[A, B](fa: F[A])(afb: A => F[B]): F[B]
}
```

* **Associative Bind**

  ```scala
  bind(bind(fa)(afb))(bfc) == bind(fa)((a) => bind(afb(a))(bfc))
  ```
* **Derived Ap**

  ```scala
  ap(fa)(fab) == bind(fab)(map(fa)(_))
  ```

### Monad

Some functors that implement `Applicative` and `Bind` are `Monad`s.

```scala
trait Monad[F[_]] extends Applicative[F] with Bind[F]
```

* **Right Identity**

  ```scala
  bind(fa)(point(_)) == fa
  ```
* **Left Identity**
  
  ```scala
  bind(point(a))(afb) == afb(a)
  ```

### Other Types of Functors

 * **Invariant**

   ```scala
   trait InvariantFunctor[F[_]] {
     def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B]
   }
   ```

 * **Contravariant**

   ```scala
   trait Contravariant[F[_]] extends InvariantFunctor[F] {
     def contramap[A, B](r: F[A])(f: B => A): F[B]
   }
   ```

 * **Bifunctor**
   
   ```scala
   trait Bifunctor[F[_, _]] {
     def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
   }
   ```
   
 * **Profunctor**
 
   ```scala
   trait Profunctor[P[_, _]] {
     def dimap[A, B, C, D](fab: P[A, B])(f: C => A)(g: B => D): P[C, D]
   }
   ```

## Functional Collections

Structures `F[_]` that are `Foldable` are "containers", whose elements can be inspected.

### Foldable

```scala
trait Foldable[F[_]] {
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
}
```

 * **Consistent Left Fold**
 
   ```scala
   foldMap(fa)(Vector(_)) == foldLeft(fa, Vector.empty[A])(_ :+ _)
   ```
 * **Consistent Right Fold**
 
   ```scala
   foldMap(fa)(Vector(_)) == foldRight(fa, Vector.empty[A])(_ +: _)
   ```

### Traversable

Containers `F[_]` that define `Traverse` can be traversed in a way that rebuilds the original structure. The typical usage is *sequencing*, which is a way of type-flipping while preserving structure, transforming `F[G[A]]` into `G[F[A]]` for some `F[_]` which has a `Traverse`.

```scala
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  final def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] = ...
}
```

**Notes**: Laws include identity, sequential fusion, purity, naturality, and parallel fusion.

# 5. Mastering Data

## Data & Codata

*Data* is a finite store of information, and may be non-recursive or recursive. Recursive data is sometimes called *inductive data*.

Codata is a description of a process for producing information, and may be non-corecursive or corecursive. Corecursive data is sometimes called *coinductive data*.

Finite data structures can be modeled with data or codata. Infinite data structures are modeled with codata. 

## Church Encoding

The lambda calculus, the basis of functional programming, is sufficient to represent all possible data structures in an encoding known as *Church encoding*. Church encoding can sometimes be used to speed up operations, and other times to give you a better understanding of the fundamental properties of some data structure. Church encoding must be faked in Scala because Scala does not have true polymorphic functions (only polymorphic methods).

### Natural Numbers

```scala
trait Natural { self =>
  def fold[Z](zero: => Z, succ: Z => Z): Z
  
  def succ: Natural = new Natural { 
    def fold[Z](zero: => Z, succ: Z => Z): Z = succ(self.fold(zero, succ))
  }
  def + (that: Natural): Natural = new Natural {
    def fold[Z](zero: => Z, succ: Z => Z): Z = that.fold[Natural](self, _.succ).fold[Z](zero, succ)
  }
  def * (that: Natural): Natural = new Natural {
    def fold[Z](zero: => Z, succ: Z => Z): Z = 
      that.fold[Natural](Natural.zero, _ + self).fold[Z](zero, succ)
  }
  def isZero: Boolean = fold[Boolean](true, _ => false)
  def toInt: Int = fold[Int](0, _ + 1)
  override def toString = toInt.toString
}
object Natural {
  val zero = new Natural { def fold[Z](zero: => Z, succ: Z => Z): Z = zero }
  def of(v: Int): Natural = if (v == 0) zero else of(v - 1).succ
}
```

## Optics

Optics provide highly-composable, purely-functional machinery for focusing in on a part of a substructure `A` inside a superstructure `S` and performing local transformations to yield a new substructure `B` inside a new superstructure `T`. Different types of optics compose (for example, two lenses can be composed to yield another lens, but a lens and a prism yield an optional).

### Lens

Lenses provide a way to focus on a single term inside a product.

```scala
abstract class PLens[S, T, A, B] extends Serializable { self =>
  def get(s: S): A
  def set(b: B): S => T
  def modifyF[F[_]: Functor](f: A => F[B])(s: S): F[T]
  def modify(f: A => B): S => T
}
type Lens[S, A] = PLens[S, S, A, A]
```

### Prisms

Prisms provide a way to focus on a single term inside a sum.

```scala
abstract class PPrism[S, T, A, B] extends Serializable { self =>
  def getOrModify(s: S): T \/ A
  def reverseGet(b: B): T
  def getOption(s: S): Option[A]
}
type Prism[S, A] = PPrism[S, S, A, A]
```

### Traversal

Traversals provide a way to focus on zero or more elements.

```scala
abstract class PTraversal[S, T, A, B] extends Serializable { self =>
  def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T]
}
type Traversal[S, A] = PTraversal[S, S, A, A]
```

### Fold

Folds provide a way to fold over zero or more values embedded in a larger structure.

```scala
abstract class Fold[S, A] extends Serializable { self =>
  def foldMap[M: Monoid](f: A => M)(s: S): M
}
```

### Getter

Getters provide a way to get a part of a larger structure.

```scala
abstract class Getter[S, A] extends Serializable { self =>
  def get(s: S): A
}
```

### Setter

Setters provide a way to modify and set a part of a larger structure.

```scala
abstract class PSetter[S, T, A, B] extends Serializable { self =>
  def modify(f: A => B): S => T
  def set(b: B): S => T
}
type Setter[S, A] = PSetter[S, S, A, A]
```

### Iso

Isos provide an isomorphism between an `A` in `S` and a `B` in `T`.

```scala
abstract class PIso[S, T, A, B] extends Serializable { self =>
  def get(s: S): A
  def reverseGet(b: B): T
  def reverse: PIso[B, A, T, S]
}
type Iso[S, A] = PIso[S, S, A, A]
```

### Optional

Optionals provide either a way to get an `A` in `S`, or to get a `T`, and a way to set a `B` in `S` to get a `T`.

```scala
abstract class POptional[S, T, A, B] extends Serializable { self =>
  def getOrModify(s: S): T \/ A
  def set(b: B): S => T
  def getOption(s: S): Option[A]
  def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T]
  def modify(f: A => B): S => T
}
type Optional[S, A] = POptional[S, S, A, A]
```

### Optic Composition Table

Almost all optics compose with the other optics. The composition of one type and itself results in the same type of optic.

|               | Getter     | Iso        | Lens       | Optional     | Prism      | Setter     | Traversal     |
| ------------- |:----------:|:----------:|:----------:|:------------:|:----------:|:----------:|:-------------:|
| **Getter**    | **Getter** | Getter     | Getter     | Fold         | Fold       | -          | Fold          |
| **Iso**       | Getter     | **Iso**    | Lens       | Optional     | Prism      | Setter     | Traversal     | 
| **Lens**      | Getter     | Lens       | **Lens**   | Optional     | Optional   | Setter     | Traversal     |
| **Optional**  | Fold       | Optional   | Optional   | **Optional** | Optional   | Setter     | Traversal     |
| **Prism**     | Fold       | Prism      | Optional   | Optional     | **Prism**  | Setter     | Traversal     |
| **Setter**    | -          | Setter     | Setter     | Setter       | Setter     | **Setter** | Setter        |
| **Traversal** | Fold       | Traversal  | Traversal  | Traversal    | Traversal  | Setter     | **Traversal** |

## Recursion Schemes

*Recursion schemes* are the ways that recursive and corecursive structures may be traversed, for purposes of altering them or inspecting them. For example, you can "fold" over a list to yield a new value. Using fixed-point data types, you can leverage generic recursion schemes that can work on any type of recursive or corecursive data structure.

### Fixed-Point Types

Fixed-point types factor out the recursion from recursive data structures. They operate on functors `F[_]` and extend them with recursion.

 * **Fix** — Inductive or coinductive recursive structures. The simplest fixed-point type.
 
   ```scala
   final case class Fix[F[_]](unFix: F[Fix[F]])
   ```
 * **Mu** — Inductive (finite) recursive structures. For example, a JSON data structure.

   ```scala
   type Algebra[F[_], A] = F[A] => A
   
   final case class Mu[F[_]](unMu: Algebra[F, ?] ~> Id)
   ```
 * **Nu** — Coinductive (possibly infinite) recursive structures. For example, a stream of values.

  ```scala
  sealed abstract class Nu[F[_]] {
    type A
    val a: A
    val unNu: A => F[A]
  }
  ```
 * **Cofree[F[_], A]** — A tree-like recursive structure whose nodes are annotated by values of type `A`.
 
   ```scala
   final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
   ```
 * **Free[F[_], A]** — A tree-like recursive structure whose leaves may be values of type `A`.
 
   ```scala
   sealed trait Free[F[_], A]
   final case class Pure[F[_], A](value: A) extends Free[F, A]
   final case class Wrap[F[_], A)(unwrap: F[Free[F, A]]) extends Free[F, A]
   ```

### Type Classes

Recursive and corecursive type classes allow you to abstract over the fixed-point type used to encode recursion or corecursion.

 * **Recursive** — `Mu` and `Nu` are recursive.
   
   ```scala
   trait Recursive[T[_[_]]] {
    def project[F[_]: Functor](t: T[F]): F[T[F]]
  }
   ```
 * **Corecursive** — `Mu` and `Nu` are corecursive.
 
    ```scala
    trait Corecursive[T[_[_]]] {
      def embed[F[_]: Functor](t: F[T[F]]): T[F]
    }
    ```

### Algebras

Algebras are single-step folds from a structure `F[_]`, while coalgebras are single-step unfolds to a structure `F[_]`. Both folds and unfolds may utilize some monadic effect `M[_]`, which may be the `Identity` monad.

 * **Algebra**

  ```scala
  type Algebra[F[_], A] = F[A] => A
  type AlgebraM[M[_], F[_], A] = F[A] => M[A]
  ```
 * **Generalized Algebra**

  ```scala
  type GAlgebra[W[_], F[_], A] = F[W[A]] => A
  type GAlgebraM[W[_], M[_], F[_], A] = F[W[A]] => M[A]
  ```
 * **Elgot Algebra**
  
  ```scala
  type ElgotAlgebra[W[_], F[_], A] = W[F[A]] => A
  type ElgotAlgebraM[W[_], M[_], F[_], A] = W[F[A]] => M[A]
  ```
 * **Coalgebra**
  
  
  ```scala
  type Coalgebra[F[_], A] = A => F[A]
  type CoalgebraM[M[_], F[_], A] = A => M[F[A]]
  ```
 * **Generalized Coalgebra**
  
  
  ```scala
  type GCoalgebra[N[_], F[_], A] = A => F[N[A]]
  type GCoalgebraM[N[_], M[_], F[_], A] = A => M[F[N[A]]]
  ```
 * **Elgot Coalgebra**

  
  ```scala
  type ElgotCoalgebraM[E[_], M[_], F[_], A] = A => M[E[F[A]]]
  type ElgotCoalgebra[E[_], F[_], A] = A => E[F[A]]
   ```


### Folds

Folds tear down a `Recursive` structure. These are some common schemes.

 * **Catamorphism**
  
  ```scala
        def cata[F[_]: Functor, A](t: T[F])(f: Algebra[F, A]): A
        ```
 * **Prepromorphism**
 
  ```scala
  def prepro[F[_]: Functor, A](t: T[F])
    (e: F ~> F, f: Algebra[F, A])(implicit T: Corecursive[T]): A
  ```
 * **Paramorphism**
 
   ```scala
   def para[F[_]: Functor, A](t: T[F])(f: GAlgebra[(T[F], ?), F, A]): A
   ```
 * **Zygomorphism**
 
   ```scala
   def zygo[F[_]: Functor, A, B](t: T[F])(f: Algebra[F, B], g: GAlgebra[(B, ?), F, A]): A
   ```   
* **Histomorphism**
 
   ```scala
   def histo[F[_]: Functor, A](t: T[F])(f: GAlgebra[Cofree[F, ?], F, A]): A
   ```

### Unfolds

Unfolds build up a `Corecursive` structure. These are some common schemes.

* **Anamorphism**
 
   ```scala
   def ana[F[_]: Functor, A](a: A)(f: Coalgebra[F, A]): T[F]
   ```
* **Postpromorphism**
 
   ```scala
   def postpro[F[_]: Functor, A](a: A)
     (e: F ~> F, g: Coalgebra[F, A])(implicit T: Recursive[T]): T[F]
   ```
* **Apomorphism**
 
   ```scala
   def apo[F[_]: Functor, A](a: A)(f: GCoalgebra[T[F] \/ ?, F, A]): T[F]
   ```
* **Futumorphism**
 
   ```scala
   def futu[F[_]: Functor, A](a: A)(f: GCoalgebra[Free[F, ?], F, A]): T[F]
   ```
   
### Refolds

Refolds tear down and build up a structure.

* **Hylomorphism**
 
   ```scala
   def hylo[F[_]: Functor, A, B](a: A)(f: Algebra[F, B], g: Coalgebra[F, A])): B
   ```   
* **Dynamorphism**
 
   ```scala
   def dyna[F[_]: Functor, A, B](a: A)(φ: F[Cofree[F, B]] => B, ψ: Coalgebra[F, A]): B
   ```   
* **Codynamorphism**
 
   ```scala
   def codyna[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: GCoalgebra[Free[F, ?], F, A]): B
   ```   
* **Chronomorphism**
 
   ```scala
   def chrono[F[_]: Functor, A, B](a: A)
     (g: F[Cofree[F, B]] => B, f: A => F[Free[F, A]]): B
   ```   
* **Elgot Algebra**
 
   ```scala
   def elgot[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: CoalgebraM[B \/ ?, F, A]): B
   ```   
* **coElgot Algebra**
 
   ```scala
   def coelgot[F[_]: Functor, A, B](a: A)(φ: ElgotAlgebra[(A, ?), F, B], ψ: Coalgebra[F, A]): B
   ```

### Transforms

Transforms are available on any type which acts as a functor transformer.

 * **Trans Catamorphism**
 
   ```scala
   def transCataT[F[_]: Functor](t: T[F])(f: T[F] => T[F]): T[F]
   ```
 * **Trans Anamorphism**
 
   ```scala
   def transAnaT[F[_]: Functor](t: T[F])(f: T[F] => T[F]): T[F]
   ```
 * **Trans Apomorphism**
 
   ```scala
   def transApoT[F[_]: Functor](t: T[F])(f: T[F] => T[F] \/ T[F]): T[F]
   ```
 * **Top-Down Catamorphism**
  
    ```scala
    def topDownCata[F[_]: Functor, A](t: T[F], a: A)(f: (A, T[F]) => (A, T[F])): T[F]
    ```

# 6. Mastering Effects

## IO

An `IO` abstraction converts effectful computations into pure values. This lets you write purely functional programs that are useful. The values have to be eventually executed, but only in the `main` function of the application.

```scala
class IO[A](val unsafePerformIO: () => A) {
  def map[B](ab: A => B): IO[B] =
    new IO(() => ab(unsafePerformIO()))
  def flatMap[B](afb: A => IO[B]): IO[B] =
    new IO(() => afb(unsafePerformIO()).unsafePerformIO())
  def attempt: IO[Either[Throwable, A]] = new IO(() => {
    try Right(unsafePerformIO())
    catch {
      case t : Throwable => Left(t)
    }
  })
}
object IO {
  def apply[A](a: => A): IO[A] = new IO(() => a)

  def fail[A](t: Throwable): IO[A] = new IO(() => throw t)
}
```

## State

A `State` monad allows retrieving and updating some arbitrary state `S`. Conceptually, the monad is a function from the old state, to both the new state and a value produced by the function.

```scala
case class State[S, A](runState: S => (S, A)) {
  def evalState(s: S): A = runState(s)._2
  def execState(s: S): S = runState(s)._1
  def map[B](ab: A => B): State[S, B] = 
    State(s => runState(s) match {
      case (s, a) => (s, ab(a))
    })
  def flatMap[B](afb: A => State[S, B]): State[S, B] =
    State(s => runState(s) match {
      case (s, a) => afb(a).runState(s)
    })
}
object State {
  def point[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](ss: S => S): State[S, Unit] = 
    for { s <- get[S]; _ <- put(ss(s)) } yield ()
}
```

## Reader

A `Reader` monad allows reading from some environment `S`.

```scala
case class Reader[S, A](runReader: S => A) {
  def map[B](ab: A => B): Reader[S, B] = Reader(s => ab(runReader(s)))
  def flatMap[B](afb: A => Reader[S, B]): Reader[S, B] =
    Reader(s => afb(runReader(s)).runReader(s))
}
object Reader {
  def point[S, A](a: => A): Reader[S, A] = Reader(_ => a)
  def read[S]: Reader[S, S] = Reader(s => s)
}
```

## Combining Effects

In general, monadic effects do not combine. However, using type classes to abstract over effects enables composition of the requirements for an effect.

### Classes

```scala
trait MonadIO[F[_]] {
  def captureEffect[A](a: A): F[A]
  
  def tryIO[A](fa: F[A]): F[Either[Throwable, A]]
}
trait MonadReader[F[_], S] {
  def ask: F[S]
  def local[A](f: S => S)(fa: F[A]): F[A]
}
trait MonadState[F[_], S] extends Monad[F] {
  def init: F[S]
  def get: F[S]
  def put(s: S): F[Unit]
}

def myFunction[M[_]: MonadIO: MonadReader[?, S]] = ...
```

### Transformers

Monad transformers are abstractions that provide an effect (such as getting and setting state) that is layered on top of another, more powerful effect.

```scala
trait MonadTrans[F[_[_], _]] {
  def liftM[G[_] : Monad, A](a: G[A]): F[G, A]

  /** The [[scalaz.Monad]] implied by this transformer. */
  implicit def apply[G[_] : Monad]: Monad[F[G, ?]]
}
```

 * **Identity**
 
   ```scala
   liftM(G.point(a)) == Monad[F[G, ?]].point(a)
   ```
 * **Composition**

   ```scala
   liftM(Monad[G].bind(ga)(f)) == 
     Monad[F[G, ?]].bind(liftM(ga))(a => liftM(f(a)))
   ```
   
#### Common Transformers

 * `StateT[F[_], S, A]` — Adds stateful operations atop `F[_]`.
 * `StreamT[F[_], A]` — Adds non-determinism atop `F[_]`.
 * `ReaderT[F[_], R, A]` — Adds reading "config" atop `F[_]`.
 * `WriterT[F[_], W, A]` — Adds adds "logging" of monoidal `W` atop `F[_]`.
 * `EitherT[F[_], E, A]` — Adds "errors" atop `F[_]`.
 * `OptionT[F[_], A]` — Adds optionality atop `F[_]`.

# 7. Mastering Functional Architecture

Functional programs are comprised from smaller pieces. Small effects compose together to form larger effects. Small pieces of state compose together to form larger state. Small functions compose together to form "larger" functions.

Code that does not interact with external systems is easy to decompose. Code that interacts with external systems — so-called *effects* — can be decomposed in a way that depends on the application's overall architecture for effects.

## Onion Architecture

The *onion architecture* for FP involves layering semantic domains of the application. On the inside, the application speaks at the level of the domain model; at the outside, the application speaks the language of external effects. 

*Interpreters* translate from an inner layer to an outer layer.

The onion architecture can be implemented with monad classes and transformers, or with free monads and similar structures of reified computation.

### Reified Computation

The purest and most powerful technique in representing effects involves *reifying* the structure and semantics of effectful computation. Standard abstractions such as the free monad and free applicative are useful.

#### Free Monad

A free monad provides a way to record and playback a sequential tree of operations described by some `F[_]`. Free monads provide a powerful way to describe all manner of effects using ordinary data structures that can be introspected, reflected, played back, and altered at runtime.

```scala
sealed trait Free[F[_], A] { self =>
  final def map[B](ab: A => B): Free[F, B] = Free.flatMap(self, ab andThen (Free.point[F, B](_)))
  final def flatMap[B](afb: A => Free[F, B]): Free[F, B] = Free.flatMap(self, afb)

  final def interpret[G[_]: Monad](fg: F ~> G): G[A] = self match {
    case Free.Point(a0) => a0().point[G]
    case Free.Effect(fa) => fg(fa)
    case fm : Free.FlatMap[F, A] =>
      val ga0 = fm.fa.interpret[G](fg)
      ga0.flatMap(a0 => fm.afb(a0).interpret[G](fg))
  }
}
object Free {
  def point[F[_], A](a: => A): Free[F, A] = Point[F, A](() => a)
  def liftF[F[_], A](fa: F[A]): Free[F, A] = Effect[F, A](fa)
  private final case class Point[F[_], A](a0: () => A) extends Free[F, A]
  private final case class Effect[F[_], A](fa: F[A]) extends Free[F, A]
  private sealed trait FlatMap[F[_], B] extends Free[F, B] {
    type A; def fa: Free[F, A]; def afb: A => Free[F, B]
  }
  private def flatMap[F[_], A0, B](fa0: Free[F, A0], afb0: A0 => Free[F, B]): Free[F, B] = new FlatMap[F, B] {
    type A = A0; def fa = fa0; def afb = afb0
  }
}
```

##### Free Monad Example

```scala
sealed trait ConsoleF[A]
final case class WriteLine(line: String) extends ConsoleF[Unit]
final case object ReadLine extends ConsoleF[String]

def writeLine(line: String): Free[ConsoleF, Unit] = WriteLine[ConsoleF, String](line)
def readLine: Free[ConsoleF, String] = Free.liftF[ConsoleF, String](ReadLine)

def example: Free[ConsoleF, Unit] = for {
  _    <- writeLine("What is your name?")
  name <- readLine
  _    <- writeLine("Hello, " + name + "!")
} yield ()
```

### Deferring Effects

Pervasive use of type classes allows polymorphism in the implementation for an effect and supports painless large-scale refactoring. This pattern is associated with monad transformers (*MTL*) but it can be applied to free computations as well.

```scala
trait Logging[F[_]] {
  def log(v: String): F[Unit]
}
object Logging {
  def apply[F[_]](implicit f: Logging[F]): Logging[F] = f
  implicit def LoggingFree[F[_]: Logging]: Logging[Free[F, ?]] =
    new Logging[Free[F, ?]] {
      def log(v: String): Free[F, Unit] = Free.liftF(Logging[F].log(v))
    }
  }
}

def myFunctionThatLogs[F[_]: Logging: Monad] = ...
```

#### Free Applicative

A free applicative provides a way to record and playback a parallel tree of operations described by some `F[_]`. It is not as expressive as a free monad, but it can be introspected all the way to the leaves, without runtime interpretation.

```scala
sealed trait FreeAp[F[_], A] { self =>
  final def map[B](ab: A => B): FreeAp[F, B] = FreeAp.map(self, ab)
  final def ap[B](fab: FreeAp[F, A => B]): FreeAp[F, B] = FreeAp.ap(self, fab)

  final def interpret[G[_]: Applicative](fg: F ~> G): G[A] = self match {
    case FreeAp.Point(a0) => a0().point[G]
    case FreeAp.Effect(fa) => fg(fa)
    case fm : FreeAp.Map[F, A] => fm.fa.interpret[G](fg).map(fm.ab)
    case fap : FreeAp.Ap[F, A] =>
      val ga0 = fap.fa.interpret[G](fg)
      ga0 <*> fap.fab.interpret[G](fg)
  }
}
object FreeAp {
  def point[F[_], A](a: => A): FreeAp[F, A] = Point[F, A](() => a)
  def liftF[F[_], A](fa: F[A]): FreeAp[F, A] = Effect[F, A](fa)
  private final case class Point[F[_], A](a0: () => A) extends FreeAp[F, A]
  private final case class Effect[F[_], A](fa: F[A]) extends FreeAp[F, A]
  private sealed trait Map[F[_], B] extends FreeAp[F, B] {
    type A; def fa: FreeAp[F, A]; def ab: A => B
  }
  private sealed trait Ap[F[_], B] extends FreeAp[F, B] {
    type A; def fa: FreeAp[F, A]; def fab: FreeAp[F, A => B]
  }
  private def ap[F[_], A0, B](fa0: FreeAp[F, A0], fa0b: FreeAp[F, A0 => B]): FreeAp[F, B] = new Ap[F, B] {
    type A = A0; def fa = fa0; def fab = fa0b
  }
  private def map[F[_], A0, B](fa0: FreeAp[F, A0], a0b: A0 => B): FreeAp[F, B] = new Map[F, B] {
    type A = A0; def fa = fa0; def ab = a0b
  }
}
```

#### Interpreters

Interpreters for reified computations often have the structure `F ~> T[G, ?]`, where `T` describes the computational context (sequential or parallel, for example), `F` describes the source operations, and `G` describes the target operations. 

```scala
type Interpreter[T[_[_], _], F[_], G[_]] = F ~> T[G, ?]
```

 * **Horizontal Composition**

   Interpreters compose *horizontally*. You can compose `Interpreter[T, F, G]` and `Interpreter[T, G, H]` into `Interpreter[T, F, H]`. Stated differently, if you can interpret `F` to `G` in some context `T`, and `G` to `H` (in the same context), then you can interpret `F` to `H`.
 * **Vertical Composition**
 
   Interpreters compose *vertically*. You can compose `Interpreter[T, F, G]` and `Interpreter[T, F, H]` into `Interpreter[T, F, Product[G, H, ?]]`.
 * **Monoidal Composition**

   Interpreters compose monoidally as per the operations supported by `T[_[_], _]`. For example, if `T` supports a notion of failure, then two `T[F, A]` operations can be appended together, such that the first computation is used if it succeeds, otherwise the second. Racing is another example of monoidal composition possible with parallel computations.
   
#### Computation Contexts

Computation contexts `T[_[_], _]` are higher-order abstractions that reify computation. `Free` is a sequential context for computation, and a higher-order monad. `FreeAp` is a parallel context for computation, and a higher-order applicative.

#### Applications

Reified computation allows for dynamic introspection and transformation of a program's structure, including weaving effects (by composing interpreters), optimizing program structure (for parallel computational contexts), and even purely-functional mocking.

# Notices

Various code snippets and laws derive from Scalaz, Monocle, and Matryoshka. All rights are reserved via their respective copyright holders.