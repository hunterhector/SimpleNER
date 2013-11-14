package edu.cmu.cs.lti.zhengzhl.run

import edu.cmu.cs.lti.zhengzhl.io.{BioTokenPerLineReader, SentenceReader}
import java.io.File
import edu.cmu.cs.lti.zhengzhl.algorithm.{Trainer, PerceptronTrainer}

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 9:02 PM
 */
object NerTrainRunner {
  def main(args: Array[String]) {
    val start = System.nanoTime


    println("Current working directory " + System.getProperty("user.dir"))

    val dataPath = args(0)

    val outputPath = args(1)

    val out = new java.io.FileWriter(outputPath)

    println("Reading training tokens")
    val reader: SentenceReader = new BioTokenPerLineReader(new File(dataPath))

    println("Preparing to train")

    val tags = Array("B-DNA", "B-RNA", "B-cell line", "B-cell type", "B-protein", "I-DNA", "I- RNA", "I-cell line", "I-cell type", "I-protein", "O")

    val perceptronTrainer: Trainer = new PerceptronTrainer(tags)

    print("Training")
    //decode each sentence
    var counter = 0
    while (reader.hasNext()) {
      val sent = reader.nextSentence()
      perceptronTrainer.onlineTraining(sent)

      counter += 1
      if (counter % 100 == 0) {
        print(".")
      }
    }
    println

    val model = perceptronTrainer.getTrainedModel

    model.foreach {
      case (feature, score) => {
        out.write("%s %.2f\n".format(feature, score))
      }
    }

    out.close

    println("Finished in: " + (System.nanoTime - start) / 1e9 + "s")

  }
}
