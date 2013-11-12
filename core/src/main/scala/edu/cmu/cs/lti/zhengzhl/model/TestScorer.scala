package edu.cmu.cs.lti.zhengzhl.model

import java.io.File
import scala.io.Source
import scala.collection.mutable.ListBuffer
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer
import edu.cmu.cs.lti.zhengzhl.feature.StandardNerFeatures

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 4:26 PM
 */


/**
 * The class reads trained model, it essentially read the weights file and multiply by the features
 * @param modelFile The weight file
 * @param gaze The gazetteer object
 */
class TestScorer(modelFile: File, gaze: Gazetteer) {

  //initialize from weights
  val weights: Map[String, Double] = Source.fromFile(modelFile).getLines().map(line => line.split(" ")) map {
    t => (t(0), t(1).toDouble)
  } toMap

  /**
   * Score of this token given this tag, and previous tag
   * @param sentence The sentence
   * @param index Position of this token in the sentence
   * @param previousTag Previous tag name
   * @param currentTag Current tag name
   * @return
   */
  def getCurrentScore(sentence: List[Token], index: Int, previousTag: String, currentTag: String): Double = {
    val features = StandardNerFeatures.getFeatureList(sentence, index, previousTag, currentTag,gaze)

//    println("[TOKEN]"+sentence(index)+" "+previousTag+" "+currentTag)
//    features.foreach(f => println(f))

    val score = getCurrentScore(features)
    score
  }

  /**
   * Calculate score from feature list
   * @param firedFeatures the list of features fired
   * @return The score
   */
  def getCurrentScore(firedFeatures: List[String]): Double = {
    firedFeatures.foldLeft(0.0)((sum, featureName) => {
//      val weight = weights.getOrElse(featureName, 0.0)
//      if (weight != 0.0)
//      println(featureName+" "+sum+" + "+weight)
      sum + weights.getOrElse(featureName, 0.0)
    })
  }
}
