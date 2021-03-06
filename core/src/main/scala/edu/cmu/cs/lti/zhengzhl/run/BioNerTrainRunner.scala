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
object BioNerTrainRunner {
  def main(args: Array[String]) {
    val start = System.nanoTime


    println("Current working directory " + System.getProperty("user.dir"))

    val dataPath = args(0)

    val outputPath = args(1)

    val out = new java.io.FileWriter(outputPath)

    println("Reading training tokens")
    val reader: SentenceReader = new BioTokenPerLineReader(new File(dataPath))

    println("Preparing to train")

    val tags = Array("B-DNA", "B-RNA", "B-cell_line", "B-cell_type", "B-protein", "I-DNA", "I-RNA", "I-cell_line", "I-cell_type", "I-protein", "O")

    val perceptronTrainer: Trainer = new PerceptronTrainer(tags)

    println("Training")
    //decode each sentence
    var counter = 0

    var sentStart = System.nanoTime()
    while (reader.hasNext()) {
      val sent = reader.nextSentence()
      perceptronTrainer.onlineTraining(sent)

      counter += 1
      if (counter % 10 == 0) {
        print(".")

        if (counter % 500 == 0) {
          println(" %d in %f s ".format(counter, (System.nanoTime - sentStart) / 1e9))
          sentStart = System.nanoTime()
        }
      }
    }

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
