package edu.cmu.cs.lti.zhengzhl.algorithm

import edu.cmu.cs.lti.zhengzhl.model.{Model, Token}
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 8:03 PM
 */
class Decoder(model: Model) {


  /**
   * Fill the column index of the lattice
   * @param sentence
   * @param index
   */
  def fillNext(sentence: List[Token], index: Int, lattice: ListBuffer[Array[Double]], backPointers: ListBuffer[Array[Int]], tagNames: Array[String]) {
//    println("Processing token at " + index)

    val previousColumn = if (index > 0) lattice(index - 1) else null

    val backPointerHere = new Array[Int](tagNames.length)

    val allFeatures:ListBuffer[String] = new ListBuffer[String]()

    val nextColumn =
      if (index > 0) {
        tagNames.zipWithIndex.map {
          case (currentTag, tagIndex) => {
            var row: Int = -1
            previousColumn.zip(tagNames).foldLeft(0.0) {
              case (maxScore, (previousMax, previousTag)) => {
//                val features = model.getFeatureList(sentence, index, previousTag, currentTag)
//                allFeatures.appendAll(features)
//                val   currentScore  = model.getCurrentScore(features);
                val currentScore = model.getCurrentScore(sentence, index, previousTag, currentTag)
                val sequenceScore = currentScore + previousMax
                row += 1
                if (row == 0) {
                  backPointerHere(tagIndex) = row
                  sequenceScore
                }
                else {
                  if (maxScore > sequenceScore)
                    maxScore
                  else {
                    backPointerHere(tagIndex) = row
                    sequenceScore
                  }
                }
              }
            }
          }
        }
      } else {
        tagNames.zipWithIndex.map {
          case (currentTag, tagIndex) => {
            model.getCurrentScore(sentence, index, "<START>", currentTag)
          }
        }
      }

//    allFeatures.sortBy(f=>f).foreach(f => println(f))

    lattice += nextColumn
    backPointers += backPointerHere
  }
}
