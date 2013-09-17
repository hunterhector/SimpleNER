package edu.cmu.cs.lti.zhengzhl.io

import edu.cmu.cs.lti.zhengzhl.model.Token

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/13/13
 * Time: 9:15 PM
 */
abstract class SentenceReader {

  def nextSentence(): List[Token]

  def hasNext(): Boolean

}
