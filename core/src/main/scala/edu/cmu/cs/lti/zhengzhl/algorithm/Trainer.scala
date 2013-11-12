package edu.cmu.cs.lti.zhengzhl.algorithm

import edu.cmu.cs.lti.zhengzhl.model.Token
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/11/13
 * Time: 9:50 PM
 */
abstract class Trainer {

  val trainedMap = new mutable.HashMap[String, Double]

  abstract def onlineTraining(sentence: List[Token], index: Int)

  abstract def batchTraining(sentences: List[List[Token]])


  def getTrainedModel:Map[String,Double] = trainedMap.toMap

}
