package edu.cmu.cs.lti.zhengzhl.model

import edu.cmu.cs.lti.zhengzhl.feature.BioNerFeatures

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 11:19 PM
 */
object TrainScorer {
  /**
   * Score of this token given this tag, and previous tag
   * @param sentence The sentence
   * @param index Position of this token in the sentence
   * @param previousTag Previous tag name
   * @param currentTag Current tag name
   * @return
   */
  def getCurrentScore(sentence: List[Token], index: Int, previousTag: String, currentTag: String, weights: Map[String, Double]): Double = {
    val features = BioNerFeatures.getFeatureList(sentence, index, previousTag, currentTag)

    //    println("[TOKEN]"+sentence(index)+" "+previousTag+" "+currentTag)
    //    features.foreach(f => println(f))

    val score = getCurrentScore(features, weights)
    score
  }

  /**
   * Calculate score from feature list
   * @param firedFeatures the list of features fired
   * @return The score
   */
  def getCurrentScore(firedFeatures: List[String], weights: Map[String, Double]): Double = {
    firedFeatures.foldLeft(0.0)((sum, featureName) => {
      //      val weight = weights.getOrElse(featureName, 0.0)
      //      if (weight != 0.0)
      //      println(featureName+" "+sum+" + "+weight)
      sum + weights.getOrElse(featureName, 0.0)
    })
  }
}
