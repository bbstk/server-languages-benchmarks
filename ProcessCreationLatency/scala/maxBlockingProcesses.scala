import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import scala.collection.mutable.ListBuffer

class HelloActor extends Actor {
  def receive = {
    case "hello" => sender ! "hello"
  }
}

object Main extends App {
  val numberOfActors = args(0).toInt
  val system = ActorSystem("HelloSystem")
  val runtime = Runtime.getRuntime
  println("Used memory in beginning: " + (runtime.totalMemory - runtime.freeMemory)/(1024*1024) + "MB")
  val t0: Long = System.currentTimeMillis

  for( a <- 1 to numberOfActors){
    system.actorOf(Props[HelloActor])
  }
  val t1: Long = System.currentTimeMillis 

  println("Time taken in seconds: " + (t1-t0)/1000.000)
  println("Used memory in end: " + (runtime.totalMemory - runtime.freeMemory)/(1024*1024) + "MB")
}