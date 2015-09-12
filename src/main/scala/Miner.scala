/**
 * Created by ramit on 9/9/2015.
 */
import java.security.MessageDigest
import scala.util.Random
import akka.actor.ActorSystem

object Miner extends App{
  var count=0
  def getHash(stringToHash: String): String ={
    val messageDigest = MessageDigest.getInstance("SHA-256")
    messageDigest.update(stringToHash.getBytes("UTF-8"))
    messageDigest.digest().map("%02X" format _).mkString
  }

  def getRandomString(stringLength: Int): String ={
    count+=1
    val randomString = Random.alphanumeric
    var stringToHash = "ramit90;hellobhaikaisahaisabbadiyahaina"+count.toString
    //randomString take stringLength foreach {stringToHash+=_}
    stringToHash
  }

  def getStartingString(numberOfZeros:Int): String ={
    var zeroString = "";
    for(i<-1 to numberOfZeros){
      zeroString += "0"
    }
    zeroString
  }
  val timeStart = System.nanoTime()
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
  val timeEnd = System.nanoTime()
  println("Time taken to find the hash: " + (timeEnd - timeStart) + "ns")
}
