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

case class RemoteConnect()
case class Generate()
case class Hash(stringToHash: String, numberOfZeros: Int)
case class Done(hashedString: String, stringHashed: String)
case class Start(numberOfZeros: Int)

class ParentActor extends Actor {
  private var requestSender: Option[ActorRef] = None

  var numberOfRequiredZeros = 0
  var count = 0
  private var remoteWorker: ActorRef = null
  def receive = {

    case Start(numberOfZeros) =>{
      numberOfRequiredZeros=numberOfZeros
    }
    case RemoteConnect()=> {
      remoteWorker = sender()
      println("remote worker connected")
  }

    case Generate() => {
      count += 1
      requestSender = Some(sender)
      val randomString = Random.alphanumeric
      var stringToHash = "ramit90;"
      randomString take 10 foreach {
        stringToHash += _}
      if(remoteWorker!=null)
        {
          if(count%2==0){
            context.actorOf(Props[ChildActor]) ! Hash(stringToHash, numberOfRequiredZeros)
            }
          else{
            remoteWorker ! Hash(stringToHash, numberOfRequiredZeros)
            }
        }
      else{
        context.actorOf(Props[ChildActor]) ! Hash(stringToHash, numberOfRequiredZeros)
      }
    }

    case Done(hashedString, stringToHash) =>{
      requestSender.map(_ ! (stringToHash+"  "+hashedString))
    }
  }
}

class ChildActor extends Actor{
  private var parentSender: Option[ActorRef] = None
  def receive ={
    case Hash(stringToHash, numberOfZeros) => {
      parentSender = Some(sender)
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
    val system = ActorSystem("MinerSystem")
    val actor = system.actorOf(Props(new ParentActor), name = "ParentActor")
    implicit val timeout = Timeout(25 seconds)
    actor ! Start(Integer.parseInt(args(0)))
    //actor ! Start(1)
    while(true){
      val future = actor ? Generate()
      future.map { result =>
        println(result)
      }
    }
   system.shutdown
  }
}
