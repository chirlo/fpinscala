package com.algae.fpinscala

import java.util.concurrent.TimeUnit

object ch7_par {

  trait ExecutorService {
    def submit[A](a: Callable[A]): Future[A]
  }

  trait Callable[A] { def call: A }
  trait Future[A] {

    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

  //not a container, but a description of a computation
  type Par[A] = ExecutorService => Future[A]

  object Par {

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    val sameThreadExecutor = new ExecutorService {
      override def submit[A](c: Callable[A]): Future[A] = UnitFuture(c.call)
    }

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    //will take care of starting execution, 'interprets' the computation
    //This is where purity ends!
    def run[A](s: ExecutorService)(a: Par[A]): A = ???

    //7.1
    def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] = es =>
      {

        val af = pa(es)
        val bf = pb(es)

        //7.3 respect time_outs
        new UnitFuture[C](f(af.get, bf.get)) {
          override def get(timeout: Long, units: TimeUnit) = {

            def nano[A](a: => A) = {
              val t0 = System.nanoTime()
              val res = a
              val delta = System.nanoTime() - t0
              (a, delta)
            }

            val (a, ta) = nano(af.get(timeout, units))
            val remaining = units.convert(ta, TimeUnit.NANOSECONDS)
            f(a, bf.get(remaining, units))
          }
        }
      }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    //demands that computation be forked off the main Thread 
    //could be like async, hmmm...
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    //7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      //this fork makes it return immediately ( the fold could take a long time for a big list)
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val ll = parMap(as)(a => if (f(a)) List(a) else List())
      map(ll)(_.flatten)
    }

    def parFold[A](as: IndexedSeq[A])(zero: A)(op: (A, A) => A): Par[A] = fork {
      if (as.size <= 1)
        unit(as.headOption.getOrElse(zero))
      else {

        val (l, r) = as.splitAt(as.length / 2)
        val sumL: Par[A] = parFold(l)(zero)(op)
        val sumR: Par[A] = parFold(r)(zero)(op)
        map2(sumL, sumR)(op)
      }
    }

    //7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
      val ind = run(es)(n) // Full source files
      choices(ind)(es)
    }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    //7.12, 7.13
    def choice[A, B](n: Par[A])(f: A => Par[B]): Par[B] = es => {
      val ind = run(es)(n) // Full source files
      f(ind)(es)
    }

    //7.14
    def join[A](ppa: Par[Par[A]]): Par[A] = es =>
      ppa(es).get(es)

  }
}