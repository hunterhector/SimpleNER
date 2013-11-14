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

/**
 * Read the gazetteer file and save it as a bug
 * @param gaze
 */
class Gazetteer(gaze: Set[String]) {


//  def file2Set(dataFile: File): Set[String] = {
//    val gaze = new mutable.HashSet[String]()
//
//    Source.fromFile(dataFile).getLines().map(line => line.split(" |,")).foreach(parts => {
//      val tag = parts(0)
//      val words = parts.slice(1, parts.length)
//      words.foreach(word => {
//        gaze += (tag + word)
//      })
//    }
//    )
//
//    gaze.toSet
//  }



  /**
   * A constructor that create a gazetteer from file
   * @param dataFile
   * @return
   */
  def this(dataFile: File) = this(
  {
    val gaze = new mutable.HashSet[String]()
    Source.fromFile(dataFile).getLines().map(line => line.split(" |,")).foreach(parts => {
      val tag = parts(0)
      val words = parts.slice(1, parts.length)
      words.foreach(word => {
        gaze += (tag + word)
      })
    }
    )
    gaze.toSet
  }
  )

  /**
   * An constructor that create a trivial empty gazetteer
   * @return
   */
  def this() = this(Set.empty[String])

  def contains(tag: String, text: String): Boolean = {
    gaze.contains((tag + text))
  }
}
