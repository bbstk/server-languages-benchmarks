import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer

case class PingMessage(data: Array[Byte])
case class StartMessage(workers: List[ActorRef], data: Array[Byte])
case class AggregateMessage(i: Int)

object Main extends App {

  class Client extends Actor {
    var mailbox: Array[Byte] = null
    def receive = {
      case PingMessage(data) => 
            mailbox = data.clone()
            sender ! PingMessage(mailbox)
    }
  }

  class Server(manager: ActorRef) extends Actor {
    var client = system.actorOf(Props[Client])
    var messageCount = 0
    var mailbox: Array[Byte] = null
    def receive = {
      case PingMessage(data) =>
            mailbox = data.clone()
            messageCount += 1
            if(messageCount == 1000) {
              manager ! AggregateMessage(messageCount)
              messageCount = 0
            } 
            client ! PingMessage(mailbox)
    }
  }

  class Manager extends Actor {
    var aggregate = 0
    var t00: Long = 0
    var t11: Long = 0
    def receive = {
      case StartMessage(workers, data) =>
            t00 = System.currentTimeMillis 
            for (worker <- workers) worker ! PingMessage(data)
      case AggregateMessage(i) =>
            aggregate += i
      case "Show" =>
            t11 = System.currentTimeMillis
            println("Total number of messages after " + (t11-t00)/1000.000 + " seconds: " + aggregate)
            System.exit(0)
    }
  }

  val system = ActorSystem("HelloSystem")
  val runtime = Runtime.getRuntime
  println("Used memory in beginning: " + (runtime.totalMemory - runtime.freeMemory)/(1024*1024) + "MB")
  val t0: Long = System.currentTimeMillis 
 
  val numberOfActors = args(0).toInt
  val data = Array.fill[Byte](500)(1)

  ///////////////////////////// CREATE THE ACTORS ///////////////////////////////
  val manager = system.actorOf(Props(new Manager()), name = "manager")
  var workers = new ListBuffer[ActorRef]()
  for( a <- 1 to numberOfActors){
    workers += system.actorOf(Props(new Server(manager)))
  }
  val t1: Long = System.currentTimeMillis 

  println("Number of actors: " + numberOfActors)
  println("Time taken to create the actors: " + (t1-t0)/1000.000)
  println("Used memory in end: " + (runtime.totalMemory - runtime.freeMemory)/(1024*1024) + "MB")

  //////////////////////////// START THE ACTORS //////////////////////////////////
  val workersList = workers.toList

  manager ! StartMessage(workersList, data)

  Thread.sleep(2000)
  manager ! "Show"
  scala.sys.exit(0)
}