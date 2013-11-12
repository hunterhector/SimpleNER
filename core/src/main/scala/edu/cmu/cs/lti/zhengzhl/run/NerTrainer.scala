package edu.cmu.cs.lti.zhengzhl.run

import edu.cmu.cs.lti.zhengzhl.io.{SentenceReader, TokenPerLineReader}
import java.io.File
import edu.cmu.cs.lti.zhengzhl.algorithm.{Trainer,PerceptronTrainer}

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 9:02 PM
 */
object NerTrainer {
  def main(args: Array[String]) {
    val start = System.nanoTime


    println("Current working directory " + System.getProperty("user.dir"))

    val dataPath = args(1)

    val outputPath = args(2)

    val out = new java.io.FileWriter(outputPath)

    println("Reading training tokens")
    val reader:SentenceReader = new TokenPerLineReader(new File(dataPath))

    println("Preparing to train")

    val perceptronTrainer:Trainer = new PerceptronTrainer()

    //decode each sentence
    while (reader.hasNext()) {
      val sent = reader.nextSentence()
      sent.zipWithIndex.foreach{case (token, index) =>{
        perceptronTrainer.onlineTraining(sent,index)
      }}
    }

    val model = perceptronTrainer.getTrainedModel

    out.close

    println("Finished in: " + (System.nanoTime - start) / 1e9 + "s")

  }
}
