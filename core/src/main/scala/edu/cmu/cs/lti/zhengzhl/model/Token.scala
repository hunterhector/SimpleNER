package edu.cmu.cs.lti.zhengzhl.model

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 1:52 PM
 */
class Token(val text: String, val pos: String, val chunkTag: String, val ner: String) {
  private var isStop: Boolean = false

  override def toString = if (!isStop) String.format("%s %s %s %s", text, pos, chunkTag, ner) else "" //empty for "STOP" symbol
}

object Token {
  def stop() = {
    val token = new Token("<STOP>", "<STOP>", "<STOP>", "<STOP>")
    token.isStop = true
    token
  }

  def apply(text: String, pos: String, chunkTag: String) = {
    new Token(text, pos, chunkTag, null)
  }

  def apply(text: String, pos: String, chunkTag: String, ner: String) = {
    new Token(text, pos, chunkTag, ner)
  }

  def apply(fields: Array[String]) = {
    if (fields.length == 1) {
      new Token(fields(0), null, null, null)
    } else if (fields.length == 2) {
      new Token(fields(0), fields(1), null, null)
    } else if (fields.length == 3) {
      new Token(fields(0), fields(1), fields(2), null)
    } else {
      new Token(fields(0), fields(1), fields(2), fields(3))
    }
  }

  def train(fields: Array[String]): Token = {
    if (fields.length < 2) {
      throw new IllegalArgumentException("Given field has length %d : %s".format(fields.length,fields.mkString(" ")))
    } else if (fields.length == 2) {
      new Token(fields(0), null, null, fields(1))
    } else if (fields.length == 3) {
      new Token(fields(0), fields(1), null, fields(2))
    } else {
      new Token(fields(0), fields(1), fields(2), fields(3))
    }
  }

}
