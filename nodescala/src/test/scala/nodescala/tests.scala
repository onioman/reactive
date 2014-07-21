package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.NoSuchElementException

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("A list of successful futures") {
    val range = (0 until 2)
    val futures = (range map (i => Future{ Thread.sleep(100); i })).toList
    val values = range.toList
    val allFutures = Future.all(futures)
    val res = Await.result(allFutures, 1 second)
    assert(res == values)
  }

  test("A list with on failed") {
    val range = (0 until 2)
    val futures = (range map (i => Future{ Thread.sleep(1000); i })).toList
    val values = range.toList

    val failed = Future {
      Thread.sleep(100)
      throw new Exception("failed")
    }
    val withFailed = failed::futures
    val allFutures = Future.all(withFailed)
    try {
      assert(Await.result(allFutures, 2 second) == values)
      assert(false)
    } catch {
      case t: TimeoutException => // failed!
        assert(false)
      case e: Exception => // ok!
        futures map (f => assert(f.isCompleted))
    }
  }

  test("any first success") {
    val quick_ok = Future{ true }
    val slow_ok  = Future{ Thread.sleep(100); false }
    val slow_ng  = Future{ Thread.sleep(100); throw new Exception("Sucks!") }
    val futures = List(slow_ng, slow_ok, quick_ok)
    val any = Future.any(futures)
    try {
      assert(Await.result(any, 1 second))
    }
  }

  test("any first fail") {
    val quick_ok = Future{ Thread.sleep(100); true }
    val slow_ok  = Future{ Thread.sleep(100); false }
    val slow_ng  = Future{ throw new Exception("Sucks!") }
    val futures = List(slow_ng, slow_ok, quick_ok)
    val any = Future.any(futures)
    try {
      assert(Await.result(any, 1 second))
    } catch {
      case t: TimeoutException => // failed!
        assert(false)
      case e: Exception => // ok!
        assert(any.failed != null)
    }
  }

  test("delay") {
    val delayed = Future.delay(1 second)
    try {
      Await.result(delayed, 999 millisecond)
      assert(false)
    } catch {
      case t: TimeoutException => // failed!
        assert(true)
      case e: Exception => // ok!
        assert(false)
    }
  }

  test("now completed ok") {
    val ok = Future { true }
    Thread.sleep(10)
    assert(ok.now)
  }

  test("now completed fail") {
    val failed = Future { throw new Exception("failed") }
    Thread.sleep(10)
    try {
      failed.now
    } catch {
      case n : NoSuchElementException =>
        assert(false, "it is already completed")
      case e: Exception =>
        assert(e.getMessage == "failed", "should be failed but got "+e.getMessage)
    }
  }

  test("no completed yet") {
    val notYet = Future { Thread.sleep(100); throw new Exception("failed") }
    try {
      notYet.now
    } catch {
      case n : NoSuchElementException =>
        assert(true, "It isn't completed yet")
      case e: Exception =>
        assert(true, "should not have failed yet: "+e.getMessage)
    }
  }

  test("continueWith successful future") {
    val f = Future{ "something" }
    val continued = f continueWith {
      completed:Future[String] => completed.value match {
        case Some(Success(r)) => r + "_continued"
        case _ => "failed"
      }
    }
    Await.result(continued, 100 millisecond)
    assert(f.isCompleted)
    continued.value match {
      case Some(Success(r)) => assert(r == "something_continued")
      case what => assert(false, what)
    }
  }

  test("continueWith unsuccessful future") {
    val f:Future[String] = Future{ throw new Exception("just because") }
    val continued = f continueWith {
      completed:Future[String] => completed.value match {
        case Some(Success(r)) => r + "_continued"
        case _ => "failed"
      }
    }
    Await.result(continued, 100 millisecond)
    assert(f.isCompleted)
    continued.value match {
      case Some(Success(r)) => assert(r == "failed")
      case what => assert(false, what)
    }
  }

  test("continue successful future ") {
    val f = Future{ "something" }
    val continued = f continue {
      r => r match {
        case Success(s) => s+"_success"
        case Failure(e) => "failed"
      }
    }
    Await.result(continued, 100 millisecond)
    assert(f.isCompleted)
    continued.value match {
      case Some(Success(r)) => assert(r == "something_success")
      case Some(Failure(e)) => assert(false)
    }
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




