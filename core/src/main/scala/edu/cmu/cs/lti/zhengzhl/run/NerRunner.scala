package edu.cmu.cs.lti.zhengzhl.run

import edu.cmu.cs.lti.zhengzhl.algorithm.Decoder
import edu.cmu.cs.lti.zhengzhl.model.Model
import java.io.File
import edu.cmu.cs.lti.zhengzhl.io.{Gazetteer, TokenPerLineReader}
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/13/13
 * Time: 9:13 PM
 */
object NerRunner {
  def main(args: Array[String]) {

    println(System.getProperty("user.dir"))

    val modelPath = args(0)

    val dataPath = args(1)

    val gazePath = args(2)

    println("Reading tokens")
    val reader = new TokenPerLineReader(new File(dataPath))

    println("Reading gazetteer")
    val gaze = new Gazetteer(new File(gazePath))

    println("Reading weights")
    val model = new Model(new File(modelPath), gaze)

    println("Preparing decode")
    val decoder: Decoder = new Decoder(model)

    val tagNames = reader.getTags
    println("Tag names")
    tagNames.foreach(tag => print(tag+" "))
    println()

    while (reader.hasNext()) {
      val lattice = new ListBuffer[Array[Double]]

      val backPointers = new ListBuffer[Array[Int]]

      val sent = reader.nextSentence()

      println("Decoding sentence ")
      sent.foreach(t => println(t))

      println("Sentence length " + sent.length)


      sent.zipWithIndex.foreach {
        case (token, i) =>
          decoder.fillNext(sent, i, lattice, backPointers, tagNames)
      }

      printLattice(lattice)
      printBackPointer(backPointers)

      recover(lattice.toList,backPointers.toList,tagNames).foreach(s => print(s+" "))
      println
    }
  }


  def recover(lattice: List[Array[Double]], backPointers: List[Array[Int]], tagNames: Array[String]): Array[String] = {
    val sentLength = lattice.size
    val maxScoreIndex = lattice(sentLength-1).zipWithIndex.maxBy(_._1)

    val maxScore = maxScoreIndex._1
    var maxIndex = maxScoreIndex._2

    println("Initial max "+maxScore+" "+maxIndex)

    val seq = new Array[String](lattice.size)

    println(sentLength -1 )
    println(tagNames(maxIndex))
    seq(sentLength -1) = tagNames(maxIndex)

    backPointers.zipWithIndex.foreach{
      case(col,index)=>{
        if (index > 0){
         maxIndex = backPointers(index)(maxIndex)
         seq(index-1) = tagNames(maxIndex)
        }
      }
    }
  return seq
  }

  def printBackPointer(backPointers:  ListBuffer[Array[Int]]){
    backPointers.foreach(col => {
      col.foreach(
        v => {
          print(v + "\t")
        }
      )
      println
    })
  }

  def printLattice(lattice: ListBuffer[Array[Double]]) {
    lattice.foreach(col => {
      col.foreach(
        v => {
          print(v + "\t")
        }
      )
      println
    })
  }

}
