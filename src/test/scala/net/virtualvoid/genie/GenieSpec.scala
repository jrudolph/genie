package net.virtualvoid.genie

import org.specs2.mutable.Specification
import shapeless._

import scala.util.Random

trait Mutate[T] {
  def apply(t: T, random: Random): T
}
object Mutate {
  def apply[T](m: Mutate[T]): Mutate[T] = m
  def mutate[T](t: T, random: Random)(implicit m: Mutate[T]): T = m(t, random)

  implicit val perElementForHNil: Mutate[HNil] = Mutate((_, _) => HNil)
  implicit def perElementForHCons[H, T <: HList](implicit hMutate: Mutate[H], tMutate: Mutate[T]): Mutate[H :: T] =
    Mutate { (t, random) =>
      t match {
        case head :: tail => hMutate(head, random) :: tMutate(tail, random)
      }
    }

  def generic[T, R](implicit tGen: Generic.Aux[T, R], rCross: Mutate[R]): Mutate[T] =
    Mutate((t, random) => tGen.from(rCross(tGen.to(t), random)))
}

trait Crossover[T] {
  def apply(t1: T, t2: T, random: Random): (T, T)
}

object Crossover {
  def apply[T](cross: Crossover[T]): Crossover[T] = cross
  def cross[T](t1: T, t2: T, random: Random)(implicit tCross: Crossover[T]): (T, T) = tCross(t1, t2, random)

  implicit val perElementForHNil: Crossover[HNil] = Crossover((_, _, _) => (HNil, HNil))
  implicit def perElementForHCons[H, T <: HList](implicit hCross: Crossover[H], tCross: Crossover[T]): Crossover[H :: T] =
    Crossover { (cons1, cons2, random) =>
      val (h1, h2) = hCross(cons1.head, cons2.head, random)
      val (t1, t2) = tCross(cons1.tail, cons2.tail, random)
      (h1 :: t1, h2 :: t2)
    }

  def generic[T, R](implicit tGen: Generic.Aux[T, R], rCross: Crossover[R]): Crossover[T] =
    Crossover { (t1, t2, random) =>
      val (n1, n2) = rCross(tGen.to(t1), tGen.to(t2), random)
      (tGen.from(n1), tGen.from(n2))
    }

  def swap[T](swapProb: Double = 0.5): Crossover[T] =
    Crossover { (t1, t2, random) =>
      if (random.nextDouble() < swapProb) (t2, t1)
      else (t1, t2)
    }
}

case class IntRange[T](from: Int, until: Int, create: Int => T) {
  def randomly(random: Random): T =
    create(from + random.nextInt(until - from))
}
object IntRange {
  implicit def mutateRandomly[T](implicit range: IntRange[T]): Mutate[T] =
    Mutate { (_, random) => range.randomly(random) }
}

case class DistinctValues[T](set: Set[T]) {
  private val ordered = set.toVector
  def randomly(random: Random): T =
    ordered(random.nextInt(ordered.size))
}
object DistinctValues {
  implicit def mutateRandomly[T](implicit distinct: DistinctValues[T]): Mutate[T] =
    Mutate { (_, random) => distinct.randomly(random) }
}

trait Genetics {
  type GenT
  type ResultT

  // FIXME: name
  case class Evaluation(
      generation:       Int,
      genotype:         GenT,
      parent1:          GenT,
      parent2:          GenT,
      evaluationResult: ResultT,
      fitness:          Double
  ) {
    def parents: Iterable[GenT] = parent1 :: parent2 :: Nil
  }

  case class Generation(
      generation:  Int,
      evaluations: Seq[Evaluation]
  ) {
    val evaluationsByFitness = evaluations.sortBy(-_.fitness)

    def top(n: Int): Seq[Evaluation] = evaluationsByFitness.take(n)
  }
}

class GenieSpec extends Specification {
  case class Age(age: Int)
  object Age {
    implicit val intRange: IntRange[Age] = IntRange[Age](0, 80, Age(_))
  }

  sealed trait Kind
  object Kind {
    case object Elephant extends Kind
    case object Mole extends Kind
    case object Giraffe extends Kind

    implicit val kindValues: DistinctValues[Kind] =
      DistinctValues(Set(Elephant, Mole, Giraffe))
  }

  case class Params(
      a: Age,
      b: Kind
  )
  object Params {

  }

  ""
}
