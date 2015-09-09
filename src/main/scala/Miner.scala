/**
 * Created by ramit on 9/9/2015.
 */
import java.security.MessageDigest
import scala.util.Random
import akka.actor.ActorSystem

object Miner extends App{
  def getHash(stringToHash: String): String ={
    val messageDigest = MessageDigest.getInstance("SHA-256")
    messageDigest.update(stringToHash.getBytes("UTF-8"))
    messageDigest.digest().map("%02X" format _).mkString
  }

  def getRandomString(stringLength: Int): String ={
    val randomString = Random.alphanumeric
    var stringToHash = "ramit90;"
    randomString take stringLength foreach {stringToHash+=_}
    stringToHash
  }

  def getStartingString(numberOfZeros:Int): String ={
    var zeroString = "";
    for(i<-1 to numberOfZeros){
      zeroString += "0"
    }
    zeroString
  }

  var randomString = getRandomString(10)
  var hashedString = getHash(randomString)
  var run = 0
  while(!hashedString.startsWith(getStartingString(4))){
    randomString = getRandomString(10)
    hashedString = getHash(randomString)
    run += 1
    println(run+" run")
  }
  println("String that has been hashed is: " + randomString + " and the hash value is: " + hashedString.toLowerCase())
}
