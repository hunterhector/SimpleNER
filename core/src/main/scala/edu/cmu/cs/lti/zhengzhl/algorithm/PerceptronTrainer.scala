package edu.cmu.cs.lti.zhengzhl.algorithm

import edu.cmu.cs.lti.zhengzhl.model.{WeightBasedModel, Token}
import edu.cmu.cs.lti.zhengzhl.feature.BioNerFeatures
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 8:20 PM
 */
class PerceptronTrainer(tagNames: Array[String]) extends Trainer(tagNames) {

  val stepSize = 1.0


  def updateWeight(key: String, change: Double) {
    trainingModel.put(key, trainingModel.getOrElse(key, 0.0) + change)
  }

  def onlineTraining(sentence: List[Token]) {
    val currentModel = new WeightBasedModel(trainingModel.toMap, BioNerFeatures)
    val currentDecoder = new Decoder(currentModel)

    val decodedTags = currentDecoder.decode(sentence, tagNames)

    sentence.zipWithIndex.zip(decodedTags).foreach {
      case ((token, index), decodedTag) => {
        val realTag = token.ner

        val previousDecodedTag = if (index > 0) decodedTags(index - 1) else "<START>"
        val previousRealTag = if (index > 0) sentence(index - 1).ner else "<START>"

        val fDecode = currentModel.featureFactory.getFeatureList(sentence, index, previousDecodedTag, decodedTag, new Gazetteer()).toSet
        val fTrue = currentModel.featureFactory.getFeatureList(sentence, index, previousRealTag, realTag, new Gazetteer()).toSet


        fDecode.foreach(decodeFeature => {
          updateWeight(decodeFeature, -stepSize)
        })

        fTrue.foreach(tureFeature => {
          updateWeight(tureFeature, stepSize)
        })
      }
    }
  }

  def batchTraining(sentences: List[List[Token]]) {

  }
}
