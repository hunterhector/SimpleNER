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

/**
 * The main function runner
 */
object NerRunner {
  def main(args: Array[String]) {
    val start = System.nanoTime


    println("Current working directory " + System.getProperty("user.dir"))

    val modelPath = args(0)

    val dataPath = args(1)

    val gazePath = args(2)

    val outputPath = args(3)

    val out = new java.io.FileWriter(outputPath)

    println("Reading tokens")
    val reader = new TokenPerLineReader(new File(dataPath))

    println("Reading gazetteer")
    val gaze = new Gazetteer(new File(gazePath))

    println("Reading weights")
    val model = new Model(new File(modelPath), gaze)

    println("Preparing decode")
    val decoder: Decoder = new Decoder(model)

    val tagNames = reader.getTags
    //decode each sentence
    while (reader.hasNext()) {
      val lattice = new ListBuffer[Array[Double]]()

      val backPointers = new ListBuffer[Array[Int]]()

      val sent = reader.nextSentence()

      //      println("Decoding sentence ")
      //      sent.foreach(t => println(t))
      //
      //      println("Sentence length " + sent.length)

      sent.zipWithIndex.foreach {

        case (token, i) => {
          decoder.fillNext(sent, i, lattice, backPointers, tagNames)
        }
      }

      // recover from the lattice and back pointers
      val results = recover(lattice.toList, backPointers.toList, tagNames)


      //      printLattice(lattice)
      //      printBackPointer(backPointers)
      //      sent.foreach(token => print(token.text+" "))
      //      println
      //      sent.foreach(token => print(token.ner+" "))
      //      println
      //      results.foreach(s => print(s+" "))
      //      println

      //      sent.foreach(token => println(token))

      sent.zip(results).foreach {
        case (token, predict) => out.write(token.toString + " " + predict + "\n")
      }
    }

    //    println("Tag names: ")
    //    tagNames.foreach(tag => print(tag+" "))
    //    println()

    out.close

    println("Finished in: " + (System.nanoTime - start) / 1e9 + "s")

  }


  /**
   * Recover the tag sequences from lattice and backpointers
   * @param lattice   Lattice output by Viterbi
   * @param backPointers  Backpointers output by Viterbi
   * @param tagNames The tag names
   * @return
   */
  def recover(lattice: List[Array[Double]], backPointers: List[Array[Int]], tagNames: Array[String]): Array[String] = {
    val sentLength = lattice.size

    //when trace back, any row will give the same score because it is STOP, this is like the final single backpointer start point
    var maxIndex = 0

    //initialize the seq of tags
    val seq = new Array[String](lattice.size)

    seq(sentLength - 1) = "" //give empty string to STOP symbol

    //fill the sequence from right to left
    backPointers.zipWithIndex.reverse.foreach {
      case (col, colNumber) => {
        if (colNumber > 0) {
          maxIndex = col(maxIndex)
          seq(colNumber - 1) = tagNames(maxIndex)
          //          println("Previous Max index "+maxIndex)
          //          println("Tag name "+tagNames(maxIndex))

        }
      }
    }



    return seq
  }

  def printBackPointer(backPointers: ListBuffer[Array[Int]]) {
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
