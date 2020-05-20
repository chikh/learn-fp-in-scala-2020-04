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

      test("non-strictness") {
        test("positive case: evals all the elems and all the tails") {
          val (s, heads, tails) = countedStream(List(1, 2, 3))
          s.forAll(_ != 42)

          assert(heads == Map(1 -> 1, 2 -> 1, 3 -> 1))
          val t = tailCountToReadble(tails)
          assert(t == Map(Some(2) -> 1, Some(3) -> 1, None -> 1))
        }

        test("negative case: evals all the elems and tails until test fails") {
          val (s, heads, tails) = countedStream(List(1, 2, 3, 4))
          s.forAll(_ != 2)

          assert(heads == Map(1 -> 1, 2 -> 1))
          val t = tailCountToReadble(tails)
          assert(t == Map(Some(2) -> 1))
        }
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
          case (current, next) =>
            Some((() => current, () => (next, current + next)))
        }
        val r = s.take(6).toList
        assert(r == List(0, 1, 1, 2, 3, 5))
      }
    }

    test("zipWith") {
      test("sums ints") {
        val r = zipWith(Stream(1, 2, 3))(Stream(4, 5))(_ + _)
        val l = r.toList
        assert(l == List(5, 7))
        l
      }
    }

    test("zipAll") {
      test("use case") {
        val r = zipAll(Stream(1, 2, 3))(Stream(4, 5))
        val l = r.toList
        assert(
          l == List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None))
        )
        l
      }

      test("full non-strictness") {
        val (s1, heads1, tails1) = countedStream(List(1, 2, 3))
        val (s2, heads2, tails2) = countedStream(List(1, 2))

        zipAll(s1)(s2)

        assert(heads1 == Map())
        assert(heads2 == Map())
        val t1 = tailCountToReadble(tails1)
        val t2 = tailCountToReadble(tails2)
        assert(t1 == Map())
        assert(t2 == Map())
      }
    }

    test("startsWith") {
      test("positive case") {
        val r = startsWith(Stream(1, 2, 3))(Stream(1, 2))
        assert(r == true)
      }
      test("negative case") {
        val r = startsWith(Stream(1, 2, 3))(Stream(2, 3))
        assert(r == false)
      }
      test("negative treaky case: prefix longer than sequence") {
        val r = startsWith(Stream(1))(Stream(1, 2))
        assert(r == false)
      }

      test("non-strictness") {
        test("positive case: evals heads and tails involved in comparison") {
          val (s1, heads1, tails1 @ _) = countedStream(List(1, 2, 3, 4, 5, 6))
          val (s2, heads2, tails2 @ _) = countedStream(List(1, 2))
          s1.startsWith(s2)

          // it's not `assert(heads1 == Map(1 -> 1, 2 -> 1))` since startsWith is `zipAll` and then `takeWhile`, and `zipAll` produces yet another value of (Some,None) for takeWhile to compare before cutting out the stream. But why it's evaluating the head, can't the underlying `unfold` generate the stream with lazy heads (and instances of head and tail still needs to be evaluated/generated in order to be pattern matched)? No, it's not a Stream[A] anymore then, but rather Stream[() => A].
          assert(heads1 == Map(1 -> 1, 2 -> 1, 3 -> 1))
          assert(heads2 == Map(1 -> 1, 2 -> 1))
          val t1 = tailCountToReadble(tails1)
          val t2 = tailCountToReadble(tails2)
          assert(t1 == Map(Some(2) -> 1, Some(3) -> 1))
          assert(t2 == Map(Some(2) -> 1, None -> 1))
        }
      }
    }

    test("tails") {
      test("works") {
        val r = tails(Stream(1, 2, 3)).toList.map(_.toList)
        assert(r == List(List(1, 2, 3), List(2, 3), List(3), List()))
        r
      }
      test(
        "full non-stricness"
      ) {
        val (s, heads, rawTails @ _) = countedStream(List(1, 2, 3))
        s.tails

        assert(heads == Map())
        assert(tailCountToReadble(rawTails) == Map())
      }
    }

    test("hasSubsequence (aggregator of many functions)") {
      test("positive") {
        val r = hasSubsequence(Stream(1, 2, 3, 4, 5))(Stream(3, 4))
        assert(r == true)
      }
      test("negative: normal") {
        val r = hasSubsequence(Stream(1, 2, 3, 4, 5))(Stream(5, 6))
        assert(r == false)
      }
      test("negative: incorrect input: sub is longer") {
        val r = hasSubsequence(Stream(1))(Stream(1, 2))
        assert(r == false)
      }
      test("non-strictness") {
        test(
          "only triggers heads and tails of this stream until matches and full head and tails of subsequence stream"
        ) {
          val (s1, heads1, rawTails1) = countedStream(List(1, 2, 3, 4, 5))
          val (s2, heads2, rawTails2) = countedStream(List(2, 3))
          s1 hasSubsequence s2

          // expected: assert(heads1 == Map(1 -> 1, 2 -> 1, 3 -> 1)) , but since it depends on .startsWith
          assert(heads1 == Map(1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1))
          assert(heads2 == Map(2 -> 1, 3 -> 1))

          // expected: assert(tails1 == Map(Some(2) -> 1, Some(3) -> 1)) , but since it depends on .startsWith:
          assert(
            tailCountToReadble(rawTails1) == Map(
              Some(2) -> 1,
              Some(3) -> 1,
              Some(4) -> 1
            )
          )
          assert(tailCountToReadble(rawTails2) == Map(Some(3) -> 1, None -> 1))
        }
      }
    }

    test("exists") {
      test("works") {
        test("positive") {
          val r = Stream(1, 2, 3).exists(_ % 2 == 0)
          assert(r == true)
          r
        }

        test("negative") {
          val r = Stream(1, 2, 3).exists(_ == 4)
          assert(r == false)
          r
        }
      }

      test("non-strictness: evals heads and tails only until matched") {
        val (s, heads, tails) = countedStream(List(1, 2, 3, 4))
        s.exists(_ % 2 == 0)

        assert(heads == Map(1 -> 1, 2 -> 1))
        val t = tailCountToReadble(tails)
        assert(t == Map(Some(2) -> 1))
      }

      test(
        "non-strictness: evals all heads and all tails if not matched at all"
      ) {
        val (s, heads, tails) = countedStream(List(1, 2, 3, 4))
        s.exists(_ == 0)

        assert(heads == Map(1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1))
        val t = tailCountToReadble(tails)
        assert(t == Map(Some(2) -> 1, Some(3) -> 1, Some(4) -> 1, None -> 1))
      }
    }

    test("scanRight") {
      test("intermediate sums") {
        val r = scanRight(Stream(1, 2, 3))(0)(_ + _).toList
        assert(r == List(6, 5, 3, 0))
      }
      test("be like `tails`") {
        test("works") {
          val r =
            scanRight(Stream(1, 2, 3))(nil[Int])(cons).toList.map(_.toList)
          assert(r == List(List(1, 2, 3), List(2, 3), List(3), List()))
          r
        }
        test("full strictness (without 'non')") {
          val (s, heads, rawTails @ _) = countedStream(List(1, 2, 3))
          s.scanRight(nil[Int])(cons)

          // that's the difference: in .tails is was: assert(heads == Map())
          assert(heads == Map(1 -> 1, 2 -> 1, 3 -> 1))
          // that's the difference: in .tails is was: assert(tailCountToReadble(rawTails) == Map())
          assert(tailCountToReadble(rawTails) == Map(Some(2) -> 1, Some(3) -> 1, None -> 1))
        }
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
