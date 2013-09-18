package edu.cmu.cs.lti.zhengzhl.io

import java.io.File
import scala.io.Source
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 3:49 PM
 */
class Gazetteer(dataFile: File) {
  private val gaze = new mutable.HashSet[String]()

  Source.fromFile(dataFile).getLines().map(line => line.split(" |,")).foreach(parts => {
    val tag = parts(0)
    val words = parts.slice(1, parts.length)
    words.foreach(word =>{
//      if (word == "Samsung") println(tag+" "+word)
      gaze += (tag+word)
    })
  }
  )

  def contains(tag: String, text: String):Boolean = {
    gaze.contains((tag+text))
  }
}
