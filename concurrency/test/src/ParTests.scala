package concurrency

import utest._
import Par._
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy
import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

object ParTests extends TestSuite {

  /**
    * Mostly copy-pasted from the original java DefaultThreadFactory
    *
    */
  class DefaultThreadFactory extends ThreadFactory {
    private final val poolNumber = new AtomicInteger(1)
    private var group: ThreadGroup = _
    private val threadNumber = new AtomicInteger(1)
    def getCreatedThreadCount = threadNumber.get() - 1
    private var namePrefix: String = _

    def apply(): DefaultThreadFactory = {
      val tf = new DefaultThreadFactory
      val s = System.getSecurityManager()
      tf.group =
        if (s != null) s.getThreadGroup()
        else
          Thread.currentThread().getThreadGroup()
      tf.namePrefix = "pool-" +
        poolNumber.getAndIncrement() +
        "-thread-"

      tf
    }

    def newThread(r: Runnable): Thread = {
      val t =
        new Thread(group, r, namePrefix + threadNumber.getAndIncrement(), 0)
      if (t.isDaemon())
        t.setDaemon(false)
      if (t.getPriority() != Thread.NORM_PRIORITY)
        t.setPriority(Thread.NORM_PRIORITY)

      t
    }
  }

  val nThreads = Int.MaxValue

  // Almost like Executors.newFixedThreadPool(Int.MaxValue) but number of thread are accessible
  def poolAndThreadFactory = {
    val tf = new DefaultThreadFactory
    val pool = new ThreadPoolExecutor(
      nThreads,
      nThreads,
      0L,
      TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable](),
      tf,
      new AbortPolicy
    )

    (pool, tf)
  }

  def tests = Tests {
    test("parallel sum") {
      val (fixedPool, tf) = poolAndThreadFactory
      val collection = Vector(1, 2, 3, 4, 5)
      val r = run(sum(collection))(fixedPool)
      val threadCount = tf.getCreatedThreadCount

      test("the result is correct") {
        assert(r == 15)
        r
      }

      test("the amount of theads created is not less than collection size") {
        assert(threadCount >= collection.size)
        threadCount
      }
    }

    test("sequence") {
      val (fixedPool, tf) = poolAndThreadFactory
      val collection = List(lazyUnit(42), lazyUnit(44))
      val r = run(sequence(collection))(fixedPool)
      val threadCount = tf.getCreatedThreadCount

      test("works properly") {
        assert(r == List(42, 44))
        r
      }

      test("the amount of theads created is not less than collection size") {
        assert(threadCount >= collection.size)
        threadCount
      }
    }

    test("parMap") {
      val (fixedPool, tf) = poolAndThreadFactory
      val l = (1 to 1024).toList
      val f: Int => String = _.toString
      val r = run(parMap(l)(f))(fixedPool)
      val threadCount = tf.getCreatedThreadCount

      test("is correct") {
        assert(r == l.map(f))
      }

      test("the amount of theads created is not less than collection size") {
        assert(threadCount >= l.size)
        threadCount
      }
    }

    test("filter") {
      val (fixedPool, tf) = poolAndThreadFactory
      val l = (1 to 1024).toList
      val p: Int => Boolean = _ % 2 == 0
      val r = run(parFilter(l)(p))(fixedPool)
      val threadCount = tf.getCreatedThreadCount

      test("is correct") {
        assert(r == l.filter(p))
      }

      test("the amount of theads created is not less than collection size") {
        assert(threadCount >= l.size)
        threadCount
      }
    }
  }
}
