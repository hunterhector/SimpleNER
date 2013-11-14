package edu.cmu.cs.lti.zhengzhl.run

import edu.cmu.cs.lti.zhengzhl.algorithm.Decoder
import edu.cmu.cs.lti.zhengzhl.model.WeightBasedModel
import java.io.File
import edu.cmu.cs.lti.zhengzhl.io.{Gazetteer, TokenPerLineReader}
import scala.collection.mutable.ListBuffer
import edu.cmu.cs.lti.zhengzhl.feature.{BioNerFeatures, StandardNerFeatures}

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/13/13
 * Time: 9:13 PM
 */

/**
 * The main function runner
 */
object BioNerDecoderRunner {
  def main(args: Array[String]) {
    val start = System.nanoTime


    println("Current working directory " + System.getProperty("user.dir"))

    val modelPath = args(0)
    val dataPath = args(1)
    val outputPath = args(2)

    val out = new java.io.FileWriter(outputPath)

    println("Reading tokens")
    val reader = new TokenPerLineReader(new File(dataPath))
    println("Reading weights")
    val model = new WeightBasedModel(new File(modelPath), BioNerFeatures, new Gazetteer())
    println("Preparing decode")
    val decoder: Decoder = new Decoder(model)

    val tagNames = Array("B-DNA", "B-RNA", "B-cell line", "B-cell type", "B-protein", "I-DNA", "I- RNA", "I-cell line", "I-cell type", "I-protein", "O")

    //decode each sentence
    print("Decoding sentences ")
    var counter = 0
    while (reader.hasNext()) {
      val sent = reader.nextSentence()
      val results = decoder.decode(sent, tagNames)
      sent.zip(results).foreach {
        case (token, predict) => out.write(token.toString + " " + predict + "\n")
      }
      counter += 1
      if (counter % 100 == 0) {
        print(".")
      }
    }
    println()

    out.close

    println("Finished in: " + (System.nanoTime - start) / 1e9 + "s")

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