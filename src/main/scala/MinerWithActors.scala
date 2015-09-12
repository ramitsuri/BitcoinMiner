/**
 * Created by ramit on 9/11/2015.
 */
import java.security.MessageDigest
import akka.actor.{ActorRef, Props, ActorSystem, Actor}
import akka.dispatch.ExecutionContexts._
import scala.util.Random
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

case class Generate(numberOfZeros: Int)
case class Hash(stringToHash: String, numberOfZeros: Int)
case class Done(hashedString: String, stringHashed: String)

class ParentActor extends Actor {
  private var requestSender: Option[ActorRef] = None
  def receive = {
    case Generate(numberOfZeros) => {
      requestSender = Some(sender)
      val randomString = Random.alphanumeric
      var stringToHash = "ramit90;"
      randomString take 10 foreach {
        stringToHash += _}
      context.actorOf(Props[ChildActor]) ! Hash(stringToHash, numberOfZeros)
    }
    case Done(hashedString, stringToHash) =>{
      requestSender.map(_ ! (stringToHash+"  "+hashedString))
    }
  }
}

class ChildActor extends Actor{
  def receive ={
    case Hash(stringToHash, numberOfZeros) => {
      val messageDigest = MessageDigest.getInstance("SHA-256")
      messageDigest.update(stringToHash.getBytes("UTF-8"))
      val hashedString = messageDigest.digest().map("%02X" format _).mkString
      var zeroString=""
      for(i<-1 to numberOfZeros){
        zeroString +="0"
      }
      if(hashedString.startsWith(zeroString)){
        sender() ! Done(hashedString, stringToHash)
      }
    }
  }
}

object project1 extends App{
  override def main(args: Array[String]) {
    implicit val ec = global
    val system = ActorSystem("System")
    val actor = system.actorOf(Props(new ParentActor))
    implicit val timeout = Timeout(25 seconds)
    while(true){
      /*val future = actor ? Generate(Integer.parseInt(args(0)))*/
      val future = actor ? Generate(1)
      future.map { result =>
        println(result)
      }
    }
    system.shutdown
  }
}
