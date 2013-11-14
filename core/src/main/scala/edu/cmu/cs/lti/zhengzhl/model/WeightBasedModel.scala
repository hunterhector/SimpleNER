package edu.cmu.cs.lti.zhengzhl.model

import java.io.File
import scala.io.Source
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer
import edu.cmu.cs.lti.zhengzhl.feature.{FeatureFactory, StandardNerFeatures}

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 4:26 PM
 */


/**
 * The class reads trained model, it essentially read the weights file and multiply by the features
 * @param gaze The gazetteer object
 */
class WeightBasedModel(weights: Map[String, Double], val featureFactory: FeatureFactory, gaze: Gazetteer) {

  /**
   * A constructor that take a model file and a gazetteer
   * @param modelFile
   * @param gaze
   * @return
   */
  def this(modelFile: File, featureFactory: FeatureFactory, gaze: Gazetteer) = this(Source.fromFile(modelFile).getLines().map(line => line.split(" ")) map {
    t => (t(0), t(1).toDouble)
  } toMap, featureFactory, gaze)

  /**
   * A constructor that read in weights from a map and use a empty gazetteer
   * @param weights
   * @return
   */
  def this(weights: Map[String, Double], featureFactory: FeatureFactory) = this(weights, featureFactory, new Gazetteer())

  /**
   * Score of this token given this tag, and previous tag
   * @param sentence The sentence
   * @param index Position of this token in the sentence
   * @param previousTag Previous tag name
   * @param currentTag Current tag name
   * @return
   */
  def getCurrentScore(sentence: List[Token], index: Int, previousTag: String, currentTag: String): Double = {
    val features = featureFactory.getFeatureList(sentence, index, previousTag, currentTag, gaze)

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
