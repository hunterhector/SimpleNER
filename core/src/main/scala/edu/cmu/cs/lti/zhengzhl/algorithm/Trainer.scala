package edu.cmu.cs.lti.zhengzhl.algorithm

import edu.cmu.cs.lti.zhengzhl.model.Token
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 9:50 PM
 */
abstract class Trainer(tagNames:Array[String]) {

  val trainingModel = new mutable.HashMap[String, Double]

  def onlineTraining(sentence: List[Token])

  def batchTraining(sentences: List[List[Token]])

  def getTrainedModel:Map[String,Double] = trainingModel.toMap

}
