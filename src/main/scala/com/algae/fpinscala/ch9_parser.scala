package com.algae.fpinscala

object ch9_parser {

  trait Parsers[ParseError, Parser[+_]] { self =>
    type Regex
    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
    implicit def string(s: String): Parser[String]

    def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = f(a)

    implicit class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def map[B](f: A => B): Parser[B] = flatMap(x => succeed(f(x)))

      def many[A](p: Parser[A]): Parser[List[A]] = ???

      def slice[A](p: Parser[A]): Parser[String] = ???

      def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = ???

      def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(f.tupled)

      def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

      def manyViaMap[A](p: Parser[A]): Parser[List[A]] = map2(p, manyViaMap(p))(_ :: _) or succeed(List())

      def succeed[A](a: A): Parser[A] = string("") map (_ => a)

      def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
        if (n <= 0) succeed(List())
        else map2(p, listOfN(n - 1, p))(_ :: _)

      def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

      def flatMap[B](f: A => Parser[B]): Parser[B] = flatMap(p)(f)

      implicit def regex(r: Regex): Parser[String] = ???

      def condParser(pi: Parser[Int]) = pi.flatMap(i => listOfN(i, p))

      def productViaFlatmap[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p.flatMap(a => p2.map((a, _)))

    }

    type JSON
    
    def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
      import P._
      val spaces = char(' ').many.slice
      
      
      
    }

  }
}