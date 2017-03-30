/**
  * Created by nmabrouk on 3/9/17.
  */

//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](head: A, tail: List[A]) extends List[A]

object List {

  // Exercise 3.2
  def tail[A](a: List[A]): List[A] = a match {
    case Nil => Nil
    case Const(h, t) => t
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    // with short circuiting for Nil: if (n<= 0 || (l match {case Nil => true})) l
    else drop(tail(l), n-1)
  }

  // Exercise 3.5.1
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Const(h, t) => Const(h, append(t, a2))
    }

  // Exercise 3.14
  def append2[A](as: List[A], bs: List[A]): List[A] =
    foldRight(as, bs)(Const(_, _))
  // same as foldRight(as, bs)((x, y) => Const(x, y))

  // Exercise 3.15
  def flattenWithFoldRight[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append2)

  // Exercise 3.15.b
  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A])(append2)

  // Exercise 3.16
  def addOne[A](l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((x, y) => Const(x + 1, y))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((x, y) => Const(f(x), y))

  // Exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    flatten(map(l)(f))

  //Exercise 3.17
  def toString(as: List[Double]): List[String] =  // can replace the Double with Number
    map(as)(_.toString)

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Const(h ,t) =>
        if (f(h)) Const(h, filter(t)(f))
        else filter(t)(f)
      case _ => Nil
    }

  // Exercise 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) Nil else List(x))

  // Exercise 3.22, can we do this with flatmap
  def addIntLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Const(h1, t1), Const(h2, t2)) => Const(h1 + h2, addIntLists(t1, t2))
  }

  // Exercise 3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Const(h1, t1), Const(h2, t2)) => Const(f(h1, h2), zipWith(t1, t2)(f))
  } // text as: scala> List.zipWith(List(1,2,3), List(1,2,3))( (x,y) => x+y)


  // Exercise 3.5.2
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => l
      case Const(h ,t) => if (f(h)) dropWhile(t, f)
      else Const(h, dropWhile(t, f))
      /*
        another implementation of dropWhile is:
        def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
         as match {
           case Cons(h,t) if f(h) => dropWhile(t)(f)
           case _ => as
         }
       The syntax for calling this version of dropWhile looks like dropWhile(xs)(f).
       That is, dropWhile(xs) is returning a function, which we then call with the argument f (in other words, dropWhile is curried)

       val xs: List[Int] = List(1,2,3,4,5)
       val ex1 = dropWhile(xs)(x => x < 4)

       Note that x is not annotated with its type anymore
         */
      /*
      Exercise 5 example:
      scala> def equalTwo(a: Int): Boolean =
           | a==2
      equalTwo: (a: Int)Boolean

      scala> equalTwo(3)
      res3: Boolean = false

      scala> val l = List(1,2,3,4,5,6)
      l: List[Int] = Const(1,Const(2,Const(3,Const(4,Const(5,Const(6,Nil))))))

      scala> l
      res4: List[Int] = Const(1,Const(2,Const(3,Const(4,Const(5,Const(6,Nil))))))

      scala> List.dropWhile(l, equalTwo)
      res5: List[Int] = Const(1,Const(3,Const(4,Const(5,Const(6,Nil)))))

      scala> List.dropWhile(l, equalTwo)
      res6: List[Int] = Const(1,Const(3,Const(4,Const(5,Const(6,Nil)))))

      scala> l
      res7: List[Int] = Const(1,Const(2,Const(3,Const(4,Const(5,Const(6,Nil))))))

      scala> List.dropWhile(l, equalTwo)
      res8: List[Int] = Const(1,Const(3,Const(4,Const(5,Const(6,Nil)))))

      scala> def greaterThanFour(a: Int): Boolean =
           | a>4
      greaterThanFour: (a: Int)Boolean

      scala> List.dropWhile(l, greaterThanFour)
      res9: List[Int] = Const(1,Const(2,Const(3,Const(4,Nil))))

      scala> List.dropWhile(List.dropWhile(l, equalTwo), greaterThanFour)
      res10: List[Int] = Const(1,Const(3,Const(4,Nil)))
       */
    }

  @annotation.tailrec
  def dropWhile2[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Const(h,t) if f(h) => dropWhile2(t, f)
      case _ => as
    }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Const(h, t) => if (tail(t) == Nil) Const(h, Nil)
      else Const(h, init(t))
    }

  // Exercise 3.3
  def setHead[A](h: A, a: List[A]): List[A] = a match {
    case Nil => Const(h, Nil)
    case Const(x, xs) => Const(h, tail(a))
    // or case Const(x, xs) => Const(h, xs)
    // or case Const(_, xs) => Const(h, xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Const(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Const(h, l) => foldLeft(l, f(z, h))(f)
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)
  // one can also replace (x, y) => x + y  with _ + _

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)((y, x) => y + x)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)((x, y) => x * y)
  // one can also replace (x, y) => x * y  with _ * _

  //Exercise 3.3.7
  /*
  def product3(ns: List[Double]): Double =
    ns match {
      case Const(h, t) if (h == 0.0) => foldRight (Nil, 1.0) ((x, y) =>  x * y)
      case Const(h, t) =>  foldRight (ns, 1.0) ((x, y) =>  x * y)
    }
    */

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Const(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Const(0.0, _) => 0.0
    case Const(x, xs) => x * product(xs)
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => if (x!=Nil) 1 + y
    else 0
    )

  def length2[A](as: List[A]): Int =
    as match {
      case Nil => 0
      case Const(h, t) => 1 + length2(t)
    }

  def reverseLeft[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((x,y) => Const(y,x))

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
}


/*
scala> List.product(List(1,2,3))
res18: Double = 6.0

scala> List.product(List(3,3,3))
res19: Double = 27.0

scala> List.product(List(13,23))
res20: Double = 299.0

scala> List.product(List(Nil))
<console>:16: error: type mismatch;
 found   : Nil.type
 required: Double
       List.product(List(Nil))
                         ^

scala> List.product(List())
res22: Double = 1.0

scala> List.product(List(0))
res23: Double = 0.0

scala> List.product(List(0,1,2))
res24: Double = 0.0

scala> List.sum(List(0))
res25: Int = 0


// Exercise 3.1
val x = List(1,2,3,4,5) match {
      case Const(x, Const(2, Const(3, _))) => x
      case Nil => 42
      case Const(x, Const(y, Const(3, Const(4, _)))) => x + y
      //case Const(h, t) => h + sum(t)
      case _ => 101
      }

Answer: 3


 */