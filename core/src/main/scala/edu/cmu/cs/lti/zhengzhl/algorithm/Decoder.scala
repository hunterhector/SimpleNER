package edu.cmu.cs.lti.zhengzhl.algorithm

import edu.cmu.cs.lti.zhengzhl.model.{TestScorer, Token}
import scala.collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 8:03 PM
 */
class Decoder(model: TestScorer) {

  /**
   * Fill the column(index) of the lattice
   * @param sentence a sentence is a list of tokens
   * @param index  the (index)th column to be filled
   */
  def fillNext(sentence: List[Token], index: Int, lattice: ListBuffer[Array[Double]], backPointers: ListBuffer[Array[Int]], tagNames: Array[String]) {
//    println("Processing token at " + index)

    val previousColumn = if (index > 0) lattice(index - 1) else null

    val backPointerHere = new Array[Int](tagNames.length)

    val allFeatures:ListBuffer[String] = new ListBuffer[String]()

    val nextColumn =
      if (index > 0) {
        //loop over the cells in this column
        tagNames.zipWithIndex.map {
          case (currentTag, tagIndex) => {
            var row: Int = -1
            //loop over the cells in previous column
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
//                  println("Max set to "+sequenceScore+"  with "+currentScore +" "+ previousMax+ " "+currentTag+ " "+previousTag)
                  sequenceScore
                }
                else {
                  if (maxScore > sequenceScore) {
                    maxScore
                  }
                  else {
//                    println("Max updated to "+sequenceScore+"  with "+currentScore +" "+ previousMax+ " "+currentTag+ " "+previousTag)
                    backPointerHere(tagIndex) = row
                    sequenceScore
                  }
                }
              }
            }
          }
        }
      } else {//sepcial treatment for <START>
        tagNames.zipWithIndex.map {
          case (currentTag, tagIndex) => {
            model.getCurrentScore(sentence, index, "<START>", currentTag)
          }
        }
      }

    lattice += nextColumn
    backPointers += backPointerHere
  }
}
