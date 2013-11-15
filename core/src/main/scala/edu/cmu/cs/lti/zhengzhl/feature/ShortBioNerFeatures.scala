package edu.cmu.cs.lti.zhengzhl.feature

import edu.cmu.cs.lti.zhengzhl.model.Token
import scala.collection.mutable.ListBuffer
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 8:27 PM
 */
object ShortBioNerFeatures extends FeatureFactory{
  /**
   * Construct the features here
   * @param sentence  The sentence
   * @param index index of the token to get feature
   * @param previousTag previous tag name for calculating feature
   * @param currentTag current tag name for calculating feature
   * @return
   */
  def getFeatureList(sentence: List[Token], index: Int, previousTag: String, currentTag: String, gaze: Gazetteer): List[String] = {
    val token: Token = sentence(index)
    val nToken = sentence.length

    val tagToUse = if (index == nToken - 1) "<STOP>" else currentTag

    val text = token.text

    val allFeatures: ListBuffer[String] = new ListBuffer()

    val currentFeatures = localBaseFeatures(sentence, index, nToken, "i")

    val surroundingFeatures: ListBuffer[String] = new ListBuffer()
    surroundingFeatures.appendAll(localBaseFeatures(sentence, index - 1, nToken, "i-1"))
    surroundingFeatures.appendAll(localBaseFeatures(sentence, index + 1, nToken, "i+1"))

    //step 1-4
    //    allFeatures.appendAll(currentFeatures)
    currentFeatures.foreach(cFeature =>{
      allFeatures.append(String.format("%s%s",cFeature,tagToUse))
    })

    //step 5
    //    allFeatures.appendAll(surroundingFeatures)
    surroundingFeatures.foreach(sFeature =>{
      allFeatures.append(String.format("%s%s",sFeature,tagToUse))
    })

    //    val appendTime = System.nanoTime() - start
    //    start = System.nanoTime()

    //step 6
    currentFeatures.foreach(cFeature => {
      surroundingFeatures.foreach(sFeature => {
        allFeatures.append(String.format("%s%s%s",cFeature, sFeature,tagToUse))
      })
    })

    //step 7
    val previousTagFeature =     String.format("T-1%s", previousTag)

    allFeatures.append(previousTagFeature)
    currentFeatures.foreach(cFeature => {
      allFeatures.append(String.format("%s%s%s",cFeature, previousTagFeature,tagToUse))
    })
    surroundingFeatures.foreach(sFeature => {
      allFeatures.append(String.format("%s%s%s",sFeature, previousTagFeature,tagToUse))
    })

    //step 8
    1 to 4 foreach (k => {
      if (text.length >= k) {
        //        allFeatures.append(String.format("%s:%s",formatWithIndex("PRE", "i", text.substring(0, k)),tagToUse))
        allFeatures.append(String.format("Pi%s%s", text.substring(0, k),tagToUse))

      }
    })

    //step 10
    //    allFeatures.append(String.format("%s:%s",formatWithIndex("CAP", "i", if (text.charAt(0).isUpper) "True" else "False"),tagToUse))
    allFeatures.append(String.format("Ci%s%s",if (text.charAt(0).isUpper) "T" else "F",tagToUse))

    //step 11
    //    allFeatures.append(format("POS", "i", "%d".format(index + 1)))
    allFeatures.append(("POSi%d%s").format((index + 1),tagToUse))

    val result = allFeatures.toList

    result
  }

  def conjoin(f1: String, f2: String): String = String.format("%s:%s", f1, f2)

  /**
   * Calculate some local features, no conjunction
   * @param sent
   * @param index
   * @param nToken
   * @param p
   * @return
   */
  def localBaseFeatures(sent: List[Token], index: Int, nToken: Int, p: String): List[String] = {
    val baseFeatures: ListBuffer[String] = new ListBuffer()

    if (index < nToken) {
      var text = ""
      var lower = ""
//      var pos = ""
      var wordShape = ""

      if (index == -1) {
        text = "<START>"
//        pos = "<START>"
        wordShape = "<START>"
        lower = "<START>"
      } else if (index == nToken - 1) {
        text = "<STOP>"
//        pos = "<STOP>"
        wordShape = "<STOP>"
        lower = "<STOP>"
      } else {
        val token = sent(index)
        text = token.text
//        pos = token.pos
        lower = text.toLowerCase
        wordShape = getWordShape(text)
      }

      baseFeatures.append(formatWithIndex("W", p, text))
      baseFeatures.append(formatWithIndex("O", p, lower))
//      baseFeatures.append(formatWithIndex("P", p, pos))
      baseFeatures.append(formatWithIndex("S", p, wordShape))
    }
    baseFeatures.toList
  }

  def formatWithIndex(f: String, i: String, v: String): String = {
    String.format("%s%s=%s", f, i, v)
  }

  /**
   * Get the word
   * @param word
   * @return
   */
  def getWordShape(word: String): String = {
    word.map(char => {
      if (char.isLetter)
        if (char.isLower)
          'a'
        else
          'A'
      else if (char.isDigit)
        'd'
      else
        char
    })
  }
}