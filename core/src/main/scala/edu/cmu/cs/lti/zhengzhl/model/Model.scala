package edu.cmu.cs.lti.zhengzhl.model

import java.io.File
import scala.io.Source
import scala.collection.mutable.ListBuffer
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 4:26 PM
 */
class Model(modelFile: File, gaze: Gazetteer) {

  val weights: Map[String, Double] = Source.fromFile(modelFile).getLines().map(line => line.split(" ")) map {
    t => (t(0), t(1).toDouble)
  } toMap


  def getCurrentScore(sentence: List[Token], index: Int, previousTag: String, currentTag: String): Double = {
    val features = getFeatureList(sentence, index, previousTag, currentTag)

//    println(index + " " + sentence(index).text)
//    features.sortBy(f => f).foreach(f => println(f))

    getCurrentScore(features)
  }


  def getCurrentScore(firedFeatures: List[String]): Double = {
    firedFeatures.foldLeft(0.0)((sum, featureName) => {
      sum + weights.getOrElse(featureName, 0.0)
    })
  }

  def getFeatureList(sentence: List[Token], index: Int, previousTag: String, currentTag: String): List[String] = {
    val token: Token = sentence(index)
    val nToken = sentence.length

    val text = token.text

    val allFeatures: ListBuffer[String] = new ListBuffer()

    val currentFeatures = localBaseFeatures(sentence, index, nToken, "i")

    val surroundingFeatures: ListBuffer[String] = new ListBuffer()
    surroundingFeatures.appendAll(localBaseFeatures(sentence, index - 1, nToken, "i-1"))
    surroundingFeatures.appendAll(localBaseFeatures(sentence, index + 1, nToken, "i+1"))

    //step 1-4
    allFeatures.appendAll(currentFeatures)
    //step 5
    allFeatures.appendAll(surroundingFeatures)

    //step 6
    currentFeatures.foreach(cFeature => {
      surroundingFeatures.foreach(sFeature => {
        allFeatures.append(conjoin(cFeature, sFeature))
      })
    })

    //step 7
    val previousTagFeature = format("T", "i-1", previousTag)
    allFeatures.append(previousTagFeature)
    currentFeatures.foreach(cFeature => {
      allFeatures.append(conjoin(cFeature, previousTagFeature))
    })
    surroundingFeatures.foreach(sFeature => {
      allFeatures.append(conjoin(sFeature, previousTagFeature))
    })

    //step 8
    1 to 4 foreach (k => {
      if (text.length > k) {
        allFeatures.append(format("PRE", "i", text.substring(0, k)))
      }
    })

    //step 9
    allFeatures.append(format("GAZ", "i", gazeMatch(token)))


    //step 10
    allFeatures.append(format("CAP", "i", if (text.charAt(0).isUpper) "True" else "False"))


    //step 11
    allFeatures.append(format("POS", "i", (index + 1).toString))

    allFeatures.toList.map(raw => String.format("%s:Ti=%s", raw, currentTag))
  }


  def gazeMatch(token: Token): String = {
    val parts = token.ner.split("-")

    if (parts.length <= 1)
      "False"
    else {
      if (gaze.contains(parts(1), token.text))
        "True"
      else
        "False"
    }
  }

  def conjoin(f1: String, f2: String): String = String.format("%s:%s", f1, f2)


  def localBaseFeatures(sent: List[Token], index: Int, nToken: Int, p: String): List[String] = {
    val baseFeatures: ListBuffer[String] = new ListBuffer()

    if (index < nToken) {
      var text = ""
      var lower = ""
      var pos = ""
      var wordShape = ""

      if (index == -1) {
        text = "<START>"
        pos = "<START>"
        wordShape = "<START>"
        lower = "<START>"
      } else if (index == nToken - 1) {
        text = "<STOP>"
        pos = "<STOP>"
        wordShape = "<STOP>"
        lower = "<STOP>"
      } else {
        val token = sent(index)
        text = token.text
        pos = token.pos
        lower = text.toLowerCase
        wordShape = getWordShape(text)
      }

      baseFeatures.append(format("W", p, text))

      baseFeatures.append(format("O", p, lower))

      baseFeatures.append(format("P", p, pos))

      baseFeatures.append(format("S", p, wordShape))

    }
    baseFeatures.toList
  }


  def format(f: String, i: String, v: String): String = {
    String.format("%s%s=%s", f, i, v)
  }

  def getWordShape(word: String): String = {
    word.map(char => {
      if (char.isLower)
        'a'
      else if (char.isUpper)
        'A'
      else
        'd'
    })
  }

}
