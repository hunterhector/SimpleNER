package edu.cmu.cs.lti.zhengzhl.model

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 1:52 PM
 */
class Token(val text:String,val pos:String,val chunkTag:String,val ner:String) {
  override def toString = String.format("[TOKEN] %s, pos : %s, chunk : %s, ner : %s",text,pos,chunkTag,ner)
}

object Token{
  def apply(text:String,pos:String,chunkTag:String) = {
    new Token(text,pos,chunkTag,null)
  }

  def apply(text:String,pos:String,chunkTag:String,ner:String) = {
    new Token(text,pos,chunkTag,ner)
  }

  def apply(fields:Array[String]) = {
    if (fields.length == 1){
      new Token(fields(0),null,null,null)
    } else if (fields.length ==2){
      new Token(fields(0),fields(1),null,null)
    } else if (fields.length ==3){
      new Token(fields(0),fields(1),fields(2),null)
    } else {
      new Token(fields(0),fields(1),fields(2),fields(3))
    }
  }

}
