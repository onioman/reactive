package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
import scala.util.Success

object Main {

  def main(args: Array[String]) {
    // TO IMPLEMENT
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val myServer = new NodeScala.Default(8191)
    val myServerSubscription = myServer.start("/test") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // TO IMPLEMENT
    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    val userInterrupted: Future[String] = {
      val input = Future.userInput("Write something:")
      input continue {
        r => r match {
          case Success(s)  => "You entered... " + s
        }
      }
    }

    // TO IMPLEMENT
    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    val timeOut: Future[String] = {
      val delayed = Future.delay(20 seconds)
      delayed continue { _ =>
        "Server timeout!"
      }
    }

    // TO IMPLEMENT
    // 4. create a future that completes when either 10 seconds elapse
    //    or the user enters some text and presses ENTER
    val terminationRequested: Future[String] = {
      Future.any(List(timeOut, userInterrupted))
    }


    // TO IMPLEMENT
    // 5. unsubscribe from the server
    terminationRequested onSuccess {
      case msg =>
        myServerSubscription.unsubscribe()
        print("Bye!")
    }
  }

}