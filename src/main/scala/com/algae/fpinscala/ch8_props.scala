package com.algae.fpinscala

import com.algae.fpinscala.ch6._
object ch8_props {

  //8.3
  /*trait Prop { self =>
    def check: Boolean
    def &&(p: Prop): Prop = new Prop {
      def check = self.check && p.check
    }

  }*/

  //8.4
  case class Gen[A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap { f(_).sample })

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))
    
    def unsized: SGen[A] = SGen( _ => this)
  }

  object Gen {

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State[RNG, Int](r => ch6.nonNegativeInt(r).swap).map(start + _ % (stopExclusive - start)))

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = Gen(State[RNG, Int](r => nonNegativeInt(r).swap).map(_ % 2 == 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      Gen(State(bool)).flatMap(if (_) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val treshold = g1._2 / (g1._2 + g2._2)
      Gen(State(x => double(x).swap)).flatMap(d => if (d > treshold) g1._1 else g2._1)
    }
    def listOf[A](g: Gen[A]): SGen[List[A]] =  SGen(listOfN(_,g))
  }

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  
  
  case class Prop(run: (TestCases,RNG) => Result){
    
    def &&(p:Prop): Prop = Prop((t,r) => run(t,r) match {
      case Passed => p.run(t,r)
      case x => x
    })
    
    
    def ||(p:Prop):Prop = Prop( (t,r) => run(t,r) match {
      case f: Falsified => p.run(t,r)
      case x => x
    })
  }
  
  case class SGen[A](forSize: Int => Gen[A])
  
  
  
}