import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class PingMessage(r: Int, d: Array[Byte])
case class StartMessage(r: Int, dest: ActorRef)
case object InitMessage
case object StopMessage

object Main extends App {

	val repetitions = args(0).toInt
	val data = Array.fill[Byte](args(1).toInt)(1)

 	class Ping(initData: Array[Byte]) extends Actor {
 		var dataCopy: Array[Byte] = null
	    def receive = {
	        case StartMessage(r, dest) =>
	            dest ! PingMessage(r-1, initData)
	        case PingMessage(r, data) =>
	            if (r < 1) {
	                Runner ! StopMessage
	            } else {
	            	dataCopy = data.clone()
	                sender ! PingMessage(r-1, dataCopy)
	            }
	    }
	}

 	class Runner(repetitions: Int) extends Actor {
 		var t0: Long = System.currentTimeMillis
    	var t1: Long = System.currentTimeMillis
    	var count = 0
	    def receive = {
	        case InitMessage =>
	            println("Used memory in beginning: " + (runtime.totalMemory - runtime.freeMemory)/(1024*1024) + "MB")
	        	t0 = System.currentTimeMillis
	        	P1!StartMessage(repetitions, P2)
	        	P2!StartMessage(repetitions, P1)
	        case StopMessage =>
	        	count += 1
	            if (count == 2) {
	                t1 = System.currentTimeMillis 

					  println("Time taken in seconds: " + (t1-t0)/1000.000)
					  println("Used memory in end: " + (runtime.totalMemory - runtime.freeMemory)/(1024*1024) + "MB")
	            } 
	        case _ => println("Run got something unexpected.")
	    }
	}	

  val system = ActorSystem("HelloSystem")
  val runtime = Runtime.getRuntime

  val P1 = system.actorOf(Props(new Ping(data)), name = "P1")
  val P2 = system.actorOf(Props(new Ping(data)), name = "P2")
  val Runner = system.actorOf(Props(new Runner(repetitions)), name = "S")

  Runner!InitMessage
}