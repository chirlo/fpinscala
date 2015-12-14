package com.algae.fpinscala

/**
 * @author iago
 */
object ch6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  //6.1 
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => nonNegativeInt(r)
    case x                 => x
  }

  //6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  //6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r1) = double(r)
    ((i, d), r1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (t, r) = intDouble(rng)
    (t.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  //6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (List[Int](), rng)
    else {
      val (l, r) = ints(count - 1)(rng)
      val (i, r2) = r.nextInt
      (i :: l, r2)
    }

  def intsFol(count: Int)(rng: RNG): (List[Int], RNG) = {
    List.range(0, count).foldLeft((List[Int](), rng)) {
      case ((l, r), _) =>
        val (j, r2) = r.nextInt
        (j :: l, r2)
    }
  }
  
  
  val bool = (x:RNG) => {val y =  map(nonNegativeInt)(_ % 2 ==0)(x); y.swap}
  

  type Rand[+A] = RNG => (A, RNG)

  val rint: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  //6.5 
  def doubleViaMap: Rand[Double] = map(rint)(_ / Int.MaxValue.toDouble + 1)

  //6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r) = ra(rng)
    val (b, r0) = rb(r)
    (f(a, b), r0)
  }

  //6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List[A](), rng)) {
      case (ra, (l, r)) =>
        val (a, rn) = ra(r)
        (a :: l, rn)
    }
  }

  def sequenceViaMap[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  //6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  //6.9 

  def mapViaF[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaF[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  //6.10
  object State {

    def unit[A, S](a: A) = State[S, A]((s: S) => (s, a))

    def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
      ls.foldRight(State.unit[List[A], S](Nil))(_.map2(_)(_ :: _))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => (s, ()))

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  case class State[S, +A](run: S => (S, A)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (s1, a) = run(s)
      f(a).run(s1)
    })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  }

  //6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def takeInput(i: Input): State[Machine, Unit] =
      modify(m => (i, m) match {
        case (Coin, Machine(true, ca, co)) if ca > 0 => Machine(false, ca, co + 1)
        case (Turn, Machine(false, ca, co))          => Machine(true, ca - 1, co)
        case _                                       => m
      })

    val mods = inputs.map(takeInput)

    for {
      _ <- State.sequence(mods)
      m <- State.get
    } yield (m.candies, m.coins)
  }

}