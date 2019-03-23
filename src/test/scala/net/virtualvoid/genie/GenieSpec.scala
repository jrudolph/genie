package net.virtualvoid.genie

import org.specs2.mutable.Specification
import shapeless._

trait Mutate[T] {
  def apply(t: T): T
}
object Mutate {
  def apply[T](f: T => T): Mutate[T] = new Mutate[T] { override def apply(t: T): T = f(t) }
  def mutate[T](t: T)(implicit m: Mutate[T]): T = m(t)

  implicit val perElementForHNil: Mutate[HNil] = Mutate(_ => HNil)
  implicit def perElementForHCons[H, T <: HList](implicit hMutate: Mutate[H], tMutate: Mutate[T]): Mutate[H :: T] =
    Mutate { case head :: tail => hMutate(head) :: tMutate(tail) }

  def generic[T, R](implicit tGen: Generic.Aux[T, R], rCross: Mutate[R]): Mutate[T] =
    Mutate(t => tGen.from(rCross(tGen.to(t))))
}

trait Crossover[T] {
  def apply(t1: T, t2: T): T
}

object Crossover {
  def apply[T](f: (T, T) => T): Crossover[T] = new Crossover[T] { override def apply(t1: T, t2: T): T = f(t1, t2) }
  def cross[T](t1: T, t2: T)(implicit tCross: Crossover[T]): T = tCross(t1, t2)

  implicit val perElementForHNil: Crossover[HNil] = Crossover((_, _) => HNil)
  implicit def perElementForHCons[H, T <: HList](implicit hCross: Crossover[H], tCross: Crossover[T]): Crossover[H :: T] =
    Crossover { (cons1, cons2) =>
      hCross(cons1.head, cons2.head) :: tCross(cons1.tail, cons2.tail)
    }

  def generic[T, R](implicit tGen: Generic.Aux[T, R], rCross: Crossover[R]): Crossover[T] =
    Crossover((t1, t2) => tGen.from(rCross(tGen.to(t1), tGen.to(t2))))
}

class GenieSpec extends Specification {
  case class Params(
      a: Int,
      b: String
  )

  ""
}
