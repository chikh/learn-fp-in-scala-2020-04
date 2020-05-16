package nonstrictness

import utest._
import Stream._
import dataStructures.List
import dataStructures.{Some, None, Option}

object StreamTests extends TestSuite {
  override def tests: Tests = Tests {
    test("cons and toList should work") {
      toList(cons(1, cons(2, nil)))
    }

    test("cons should trigger heads and tails evalutaion only once") {
      val (mutableValueToCount, valueWithSideEffect) =
        mutableCounterAndGenerator[Int]

      val stream = cons(
        valueWithSideEffect(1),
        cons(valueWithSideEffect(2), cons(valueWithSideEffect(3), nil))
      )

      def list = toList(stream)
      list
      list

      assert(!mutableValueToCount.values.exists(_ > 1))
      mutableValueToCount
    }

    test("cons shouldn't trigger anything itself") {
      val (counters, valueF) = mutableCounterAndGenerator[Int]
      cons(valueF(1), cons(valueF(2), nil))

      assert(!counters.values.exists(_ > 0))
    }

    test("take") {
      test("general case") {
        val r = cons(1, cons(2, cons(3, nil))).take(2).toList
        assert(r == List(1, 2))
        r
      }

      test("shouldn't trigger") {
        test("heads at all") {
          val (counter, value) = mutableCounterAndGenerator[Int]

          cons(value(1), cons(value(2), nil)).take(1)
          assert(counter.isEmpty)
          counter
        }

        test("tails at all") {
          val (counter, value) = mutableCounterAndGenerator[Stream[Int]]

          cons(1, value(cons(2, value(nil)))).take(1)
          assert(counter.isEmpty)

          counter
        }
      }
    }

    test("apply") {
      val r = Stream(1, 2, 3).toList
      assert(r == List(1, 2, 3))
      r
    }

    test("foldRight") {
      test("normal multiplication") {
        test("result is correct") {
          val r = Stream(1, 2, 3, 4).foldRight(1)(_ * _)
          assert(r == 24)
          r
        }
      }
      test("multiplication with zero shortcut") {
        val f: (=> Int, => Int) => Int = (a, b) => if (a == 0) a else a * b
        val (headsCounter, headsGen) = mutableCounterAndGenerator[Int]
        val (tailsCounter, tailsGen) = mutableCounterAndGenerator[Stream[Int]]

        val r = cons(
          headsGen(1),
          tailsGen(
            cons(headsGen(0), tailsGen(cons(headsGen(3), tailsGen(nil))))
          )
        ).foldRight(1)(f)

        test("gives 0") {
          assert(r == 0)
          r
        }
        test("only the elems before and including 0 are evaluated") {
          assert(headsCounter == Map(1 -> 1, 0 -> 1))
          headsCounter
        }
        test("only the tails before and including 0 are evaluated") {
          assert(tailsCounter.values.sum == 1)
          tailsCounter
        }
      }
    }

    test("forAll") {
      val r = Stream(1, 2, 42, 3).forAll(_ != 42)
      test("this case is not for all") {
        assert(r == false)
        r
      }
    }

    test("takeWhile") {
      val (s, headCount, tailCount) = countedStream(List(1, 2, 42, 3))
      val r = s.takeWhile(_ != 42)

      test("should return the beginning") {
        assert(r.toList == List(1, 2))
        r
      }
      test(
        "triggers only the first head (it returns stream, which is lazy, but to decide stream is empty or not, we have to trigger the first head)"
      ) {
        assert(headCount == Map(1 -> 1))
        headCount
      }
      test("triggers no tails (it returns stream, which is lazy)") {
        assert(
          tailCount == Map()
        )
        tailCount
      }
      test("triggers heads until predicate if evaluated") {
        r.toList
        assert(headCount == Map(1 -> 1, 2 -> 1, 42 -> 1))
        headCount
      }
      test("triggers tails until predicate if evaluated") {
        r.toList
        val tc = tailCountToReadble(tailCount)
        assert(tc == Map(Some(2) -> 1, Some(42) -> 1))
        tc
      }
    }

    test("headOption") {
      test("happy case") {
        val r = Stream(1, 2, 3).headOption
        assert(r == Some(1))
        r
      }
      test("unhappy case") {
        val r = nil[Int].headOption
        assert(r == None)
        r
      }
    }

    test("map") {
      val (s, headCounter, tailsCounter) = countedStream(List(1, 2, 3))
      val r = s.map(_.toString)
      test("maps") {
        assert(r.toList == List("1", "2", "3"))
        r
      }
      test("no heads") {
        assert(headCounter.values.sum == 0)
        headCounter
      }
      test("no tails") {
        assert(tailsCounter.values.sum == 0)
        tailsCounter
      }
    }

    test("append") {
      val (s1, s1Heads, s1Tails) = countedStream(List(1, 2, 3))
      val (s2, s2Heads, s2Tails) = countedStream(List(4, 5, 6))

      test("should concat") {
        val r = s1.append(s2)
        assert(r.toList == List(1, 2, 3, 4, 5, 6))
        r
      }

      test("shouldn't eval streams") {
        assertNoElemets(s1Heads)
        assertNoElemets(s1Tails)
        assertNoElemets(s2Heads)
        assertNoElemets(s2Tails)
      }
    }

    test("flatMap") {
      val (s, heads, tails) = countedStream(List(1, 2))
      val r = s.flatMap(a => Stream(a.toString))

      test("works") {
        assert(r.toList == List("1", "2"))
      }

      test(
        "only the first head should be evaluated (essentially the check if it's empty in the underlying foldRight)"
      ) {
        assert(heads == Map(1 -> 1))
      }

      test("tail non-strictness") {
        assertNoElemets(tails)
      }
    }

    test("filter") {
      val (s, heads, tails) = countedStream(List(1, 2, 3))
      val r = s.filter(_ % 2 == 0)

      test("works") {
        assert(r.toList == List(2))
      }

      test(
        "eval head until predicate if fulfilled due to limitations of impl (scala's LazyList doesn't have this problem)"
      ) {
        assert(heads == Map(1 -> 1, 2 -> 1))
      }

      test(
        "eval tails until predicate if fulfilled for the first time due to limitations of this impl (e.g. scala's LazyList doesn't have this problem)"
      ) {
        val readableTails = tailCountToReadble(tails)
        assert(readableTails == Map(Some(2) -> 1))
      }
    }

    test("a bigger chain of actions") {
      val (s, heads, tails) = countedStream(List(1, 2, 3, 4))
      val r = s.map(_ + 10).filter(_ % 2 == 0)

      test("works") {
        assert(r.toList == List(12, 14))
      }

      test("limited non-strictness (see filter's test)") {
        assert(heads == Map(1 -> 1, 2 -> 1))
        val readableTails = tailCountToReadble(tails)
        assert(readableTails == Map(Some(2) -> 1))
      }
    }

    test("fibs") {
      val s = fibs

      test("is correct") {
        val r = s.take(6).toList
        assert(r == List(0, 1, 1, 2, 3, 5))
      }
    }

    test("unfold") {
      test("generates fibs stream") {
        val s = unfold((0, 1)) {
          case (current, next) => Some((current, (next, current + next)))
        }
        val r = s.take(6).toList
        assert(r == List(0, 1, 1, 2, 3, 5))
      }
    }
  }

  def assertNoElemets[A](counter: scala.collection.Map[A, Int]) = {
    assert(counter.values.sum == 0)
    counter
  }

  def countedStream[A](es: List[A]): (
      Stream[A],
      scala.collection.mutable.Map[A, Int],
      scala.collection.mutable.Map[Stream[A], Int]
  ) = {
    val (headCount, headGen) = mutableCounterAndGenerator[A]
    val (tailCount, tailGen) = mutableCounterAndGenerator[Stream[A]]

    val s =
      List.foldRightViaLeft(es)(nil[A])((a, s) => cons(headGen(a), tailGen(s)))

    (s, headCount, tailCount)
  }

  def tailCountToReadble[A](
      tc: scala.collection.mutable.Map[Stream[A], Int]
  ): Map[Option[A], Int] =
    tc.map {
      case (Cons(h, _), c: Int) => Some(h()) -> c
      case (Nil, c)             => None -> c
    }.toMap

  def mutableCounterAndGenerator[A] = {
    val mutableValueToCount: scala.collection.mutable.Map[A, Int] =
      scala.collection.mutable.Map.empty

    def valueWithSideEffect(v: A) = {
      mutableValueToCount
        .get(v)
        .map(count => mutableValueToCount.addOne(v -> (count + 1)))
        .getOrElse {
          mutableValueToCount.addOne(v -> 1)
        }

      v
    }

    (mutableValueToCount, valueWithSideEffect _)
  }
}
