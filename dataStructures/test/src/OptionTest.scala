package dataStructures

import utest._

import Option._

object OptionTest extends TestSuite {
  def tests = Tests {
    test("flatMap") {
      test("to anyther type") {
        val r1 = flatMap(Some(42))(v => Some(v.toString))
        val r2 = flatMap(None: Option[Int])(v => Some(v.toString))
        assert(r1 == Some("42"))
        assert(r2 == None)
        (r1, r2)
      }

      test("to None") {
        val r1 = flatMap(Some(42))(_ => None)
        val r2 = flatMap(None)(_ => None)
        assert(r1 == None)
        assert(r2 == None)
        (r1, r2)
      }
    }

    test("sequence") {
      test("Nil -> Some(Nil)") {
        var r = sequence(Nil: List[Option[Int]])
        assert(r == Some(Nil))
        r
      }

      test("List last None -> None") {
        var r = sequence(List(Some(1), Some(2), None))
        assert(r == None)
        r
      }

      test("List all Some -> Some list") {
        var r = sequence(List(Some(1), Some(2)))
        assert(r == Some(List(1, 2)))
        r
      }
    }

    test("traverse") {
      test("successful type conversion") {
        val r = traverse(List("1", "2"))(v => Try(v.toInt))
        assert(r == Some(List(1, 2)))
        r
      }

      test("failing type conversion") {
        val r = traverse(List("1", "nan"))(v => Try(v.toInt))
        assert(r == None)
        r
      }
    }
  }
}
