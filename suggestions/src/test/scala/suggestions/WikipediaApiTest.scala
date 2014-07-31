package suggestions



import language.postfixOps
import scala.collection._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rx.lang.scala.subjects.ReplaySubject


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin", "antonio  lancho")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 4, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should recover") {
    val withError = ReplaySubject[String]
    withError.onNext("test")
    withError.onError(new Exception("error"))

    val observed = mutable.Buffer[String]()
    var bads = 0
    var completed = false
    val recovered = withError.recovered
    recovered.subscribe(
      elem => { elem match {
        case Success(t) =>
          observed += t
        case Failure(e) => bads += 1
      }} ,
      t => assert(false, s"stream error $t"),
      () => completed = true
    )

    assert(completed && 1 == bads, bads)
    assert(observed == Seq("test"), "got: " + observed + "expected:" + Seq("test") )
  }

  test("WikipediaApi observable with timeout completes on time") {

    val numbers = ReplaySubject[Int]
    val producer = Future {
      numbers.onNext(1)
      Thread.sleep(1100)
      numbers.onNext(2)
    }

    val observed = mutable.Buffer[Int]()
    var bads = 0
    var completed = false
    val timedout = numbers.timedOut(1)
    timedout.subscribe(
      observed += _ ,
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    Thread.sleep(1000)
    assert(completed, completed)
    assert(observed == Seq(1), "got: " + observed + "expected:" + Seq(1) )
    Thread.sleep(100)
    assert(observed == Seq(1), "got: " + observed + "expected:" + Seq(1) )
  }

  test("WikipediaApi observable with timeout completes on error") {

    val numbers = ReplaySubject[Int]
    val producer = Future {
      numbers.onNext(1)
      numbers.onError(new Exception("failed"))
    }

    val observed = mutable.Buffer[Int]()
    var bads = 0
    var completed = false
    val timedout = numbers.timedOut(2)
    timedout.subscribe(
      observed += _ ,
      t => assert(true, s"stream error $t"),
      () => completed = true
    )
    assert(!completed, completed)
    assert(observed == Seq(1), "got: " + observed + "expected:" + Seq(1) )
  }

  test("WikipediaApi observable with timeout completes on complete") {

    val numbers = ReplaySubject[Int]
    val producer = Future {
      numbers.onNext(1)
      numbers.onNext(2)
      numbers.onCompleted()
    }

    val observed = mutable.Buffer[Int]()
    var bads = 0
    var completed = false
    val timedout = numbers.timedOut(2)
    timedout.subscribe(
      observed += _ ,
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed, completed)
    assert(observed == Seq(1, 2), "got: " + observed + "expected:" + Seq(1) )
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
}