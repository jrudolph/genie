package net.virtualvoid.genie

import org.specs2.mutable.Specification
import shapeless._

import scala.annotation.tailrec
import scala.util.Random

trait Sample[T] {
  def apply(random: Random): T
}
object Sample {
  def apply[T](c: Sample[T]): Sample[T] = c

  def create[T](random: Random)(implicit c: Sample[T]): T = c(random)

  implicit val perElementForHNil: Sample[HNil] = Sample(_ => HNil)
  implicit def perElementForHCons[H, T <: HList](implicit hCreate: Sample[H], tCreate: Sample[T]): Sample[H :: T] =
    Sample { random =>
      hCreate(random) :: tCreate(random)
    }

  trait WithGeneric[T, R] {
    def get(implicit rCreate: Sample[R]): Sample[T]
  }
  def generic[T](implicit tGen: Generic[T]): WithGeneric[T, tGen.Repr] = new WithGeneric[T, tGen.Repr] {
    def get(implicit rCreate: Sample[tGen.Repr]): Sample[T] =
      Sample(random => tGen.from(rCreate(random)))
  }

  implicit def sampleIntRangeRandomly[T](implicit range: IntRange[T]): Sample[T] =
    Sample { random => range.sample(random) }
  implicit def sampleDistinctValueRandomly[T](implicit distinct: DistinctValues[T]): Sample[T] =
    Sample { random => distinct.sample(random) }
}

trait Mutate[T] {
  def apply(t: T, random: Random): T
}
object Mutate {
  def apply[T](m: Mutate[T]): Mutate[T] = m
  def withRate[T](m: Mutate[T])(implicit rate: MutationRate): Mutate[T] =
    Mutate((t, random) => if (random.nextDouble() < rate.probability) m(t, random) else t)

  def mutate[T](t: T, random: Random)(implicit m: Mutate[T]): T = m(t, random)

  implicit val perElementForHNil: Mutate[HNil] = Mutate((_, _) => HNil)
  implicit def perElementForHCons[H, T <: HList](implicit hMutate: Mutate[H], tMutate: Mutate[T]): Mutate[H :: T] =
    Mutate { (t, random) =>
      t match {
        case head :: tail => hMutate(head, random) :: tMutate(tail, random)
      }
    }

  trait WithGeneric[T, R] {
    def get(implicit rMutate: Mutate[R]): Mutate[T]
  }
  def generic[T](implicit tGen: Generic[T]): WithGeneric[T, tGen.Repr] = new WithGeneric[T, tGen.Repr] {
    def get(implicit rMutate: Mutate[tGen.Repr]): Mutate[T] =
      Mutate((t, random) => tGen.from(rMutate(tGen.to(t), random)))
  }

  implicit def mutateBySampling[T](implicit sample: Sample[T], rate: MutationRate): Mutate[T] =
    Mutate.withRate { (_, random) => sample(random) }
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

  trait WithGeneric[T, R] {
    def get(implicit rCross: Crossover[R]): Crossover[T]
  }
  def generic[T](implicit tGen: Generic[T]): WithGeneric[T, tGen.Repr] = new WithGeneric[T, tGen.Repr] {
    def get(implicit rCross: Crossover[tGen.Repr]): Crossover[T] =
      Crossover { (t1, t2, random) =>
        val (n1, n2) = rCross(tGen.to(t1), tGen.to(t2), random)
        (tGen.from(n1), tGen.from(n2))
      }
  }

  implicit def swap[T] /*(swapProb: Double = 0.5)*/ : Crossover[T] =
    Crossover { (t1, t2, random) =>
      if (random.nextDouble() < 0.5) (t2, t1)
      else (t1, t2)
    }

  def disable[T]: Crossover[T] = Crossover((t1, t2, _) => (t1, t2))
}

case class MutationRate(probability: Double)

case class IntRange[T](from: Int, until: Int, create: Int => T) {
  def sample(random: Random): T =
    create(from + random.nextInt(until - from))
}

case class DistinctValues[T](set: Set[T]) {
  private val ordered = set.toVector
  def sample(random: Random): T =
    ordered(random.nextInt(ordered.size))
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

  sealed trait Color
  object Color {
    case object Orange extends Color
    case object Pink extends Color
    case object Green extends Color

    implicit val colorValues: DistinctValues[Color] =
      DistinctValues(Set(Orange, Pink, Green))
  }

  case class Creature(
      age:   Age,
      kind:  Kind,
      color: Color
  )
  object Creature {
    implicit val mutationRate = MutationRate(0.05)
    //    implicit val kindCross = Crossover.swap[Kind]()
    //    implicit val colorCross = Crossover.swap[Color]()
    //    implicit val AgeCross = Crossover.swap[Age]()

    //implicitly[Crossover[Kind]]
    implicit val creatureCreation = Sample.generic[Creature].get
    implicit val creatureMutation = Mutate.generic[Creature].get
    implicit val creatureCrossover = Crossover.generic[Creature].get
  }

  ""
}
