package edu.cmu.cs.lti.zhengzhl.feature

import edu.cmu.cs.lti.zhengzhl.model.Token
import edu.cmu.cs.lti.zhengzhl.io.Gazetteer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/13/13
 * Time: 10:30 PM
 */
trait FeatureFactory {
  def getFeatureList(sentence: List[Token], index: Int, previousTag: String, currentTag: String, gaze: Gazetteer): List[String]
}
