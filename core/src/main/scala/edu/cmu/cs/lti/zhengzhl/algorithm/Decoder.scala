package edu.cmu.cs.lti.zhengzhl.algorithm

import edu.cmu.cs.lti.zhengzhl.model.{WeightBasedModel, Token}
import scala.collection.mutable.ListBuffer
import edu.cmu.cs.lti.zhengzhl.feature.StandardNerFeatures

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/16/13
 * Time: 8:03 PM
 */
class Decoder(model: WeightBasedModel) {


  def decode(sent: List[Token], tagNames: Array[String]): Array[String] = {


    val lattice =  Array.ofDim[Double](sent.length,tagNames.length)

    val backPointers = Array.ofDim[Int](sent.length,tagNames.length)

//    val start = System.nanoTime()

    sent.zipWithIndex.foreach {
      case (token, i) => {
        fillNext(sent, i, lattice, backPointers, tagNames)
      }
    }

//    println("##########Fill " + (System.nanoTime - start) / 1e9)

    // recover from the lattice and back pointers
    val results = recover(lattice.toList, backPointers.toList, tagNames)

    return results
  }

  /**
   * Fill the column(index) of the lattice
   * @param sentence a sentence is a list of tokens
   * @param index  the (index)th column to be filled
   */
  def fillNext(sentence: List[Token], index: Int, lattice: Array[Array[Double]], backPointers: Array[Array[Int]], tagNames: Array[String]) {
    //    println("Processing token at " + index)

    var scoringTime = 0.0

//    val start = System.nanoTime()

//    val previousColumn = if (index > 0) lattice(index - 1) else null

//    val backPointerHere = new Array[Int](tagNames.length)

//    val nextColumn = new Array[Double](tagNames.length)
    if (index > 0) {
      for (tagIndex <- 0 until tagNames.length) {
        var maxScore = 0.0
        val fakeFeatures = StandardNerFeatures.getFeatureList(sentence, index, "#PRE#", tagNames(tagIndex), model.gaze)

        for (row <- 0 until tagNames.length) {
          val previousMax = lattice(index-1)(row)
          val previousTag = tagNames(row)

//          val realFeatures = fakeFeatures.map(f => f.replace("#PRE#",previousTag))
          val currentScore = model.getCurrentScore(fakeFeatures,"#PRE#",previousTag)

          //          val scoreStart = System.nanoTime()
//          val currentScore = model.getCurrentScore(sentence, index, previousTag, tagNames(tagIndex))
//          scoringTime += System.nanoTime()-scoreStart
//          println(System.nanoTime()-scoreStart)

          val sequenceScore = currentScore + previousMax

          if (row == 0) {
            backPointers(index)(tagIndex) = row
            maxScore = sequenceScore
          } else {
            if (maxScore < sequenceScore) {
              backPointers(index)(tagIndex) = row
              maxScore = sequenceScore
            }
          }
        }
        lattice(index)(tagIndex) =maxScore
      }
    } else {
      //sepcial treatment for <START>
      for (tagIndex <- 0 until tagNames.length) {
        val currentTag = tagNames(tagIndex)
        lattice(index)(tagIndex) =model.getCurrentScore(sentence, index, "<START>", currentTag)
      }
    }
//
//    val fillTime= (System.nanoTime - start)
//        println("Fill next" +  fillTime/ 1e9 +"s")
//        println("Scoring time" + (scoringTime) / 1e9 +"s")
//
//      println("Percent "+scoringTime/fillTime)

    //    val nextColumn =
//      if (index > 0) {
//        //loop over the cells in this column
//        tagNames.zipWithIndex.map {
//          case (currentTag, tagIndex) => {
//            var row: Int = -1
//            //loop over the cells in previous column
//            previousColumn.zip(tagNames).foldLeft(0.0) {
//              case (maxScore, (previousMax, previousTag)) => {
//                //                val features = model.getFeatureList(sentence, index, previousTag, currentTag)
//                //                allFeatures.appendAll(features)
//                //                val   currentScore  = model.getCurrentScore(features);
//
//                val currentScore = model.getCurrentScore(sentence, index, previousTag, currentTag)
//
//                val sequenceScore = currentScore + previousMax
//
//                row += 1
//                if (row == 0) {
//                  backPointerHere(tagIndex) = row
//                  //                  println("Max set to "+sequenceScore+"  with "+currentScore +" "+ previousMax+ " "+currentTag+ " "+previousTag)
//                  sequenceScore
//                }
//                else {
//                  if (maxScore > sequenceScore) {
//                    maxScore
//                  }
//                  else {
//                    //                    println("Max updated to "+sequenceScore+"  with "+currentScore +" "+ previousMax+ " "+currentTag+ " "+previousTag)
//                    backPointerHere(tagIndex) = row
//                    sequenceScore
//                  }
//                }
//              }
//            }
//          }
//        }
//      } else {
//        //sepcial treatment for <START>
//        tagNames.zipWithIndex.map {
//          case (currentTag, tagIndex) => {
//            model.getCurrentScore(sentence, index, "<START>", currentTag)
//          }
//        }
//      }

//    lattice(index) = nextColumn
//    backPointers(index) = backPointerHere
  }

  /**
   * Recover the tag sequences from lattice and backpointers
   * @param lattice   Lattice output by Viterbi
   * @param backPointers  Backpointers output by Viterbi
   * @param tagNames The tag names
   * @return
   */
  def recover(lattice: List[Array[Double]], backPointers: List[Array[Int]], tagNames: Array[String]): Array[String] = {
    val sentLength = lattice.size

    //when trace back, any row will give the same score because it is STOP, this is like the final single backpointer start point
    var maxIndex = 0

    //initialize the seq of tags
    val seq = new Array[String](lattice.size)

    seq(sentLength - 1) = "" //give empty string to STOP symbol

    //fill the sequence from right to left
    backPointers.zipWithIndex.reverse.foreach {
      case (col, colNumber) => {
        if (colNumber > 0) {
          maxIndex = col(maxIndex)
          seq(colNumber - 1) = tagNames(maxIndex)
          //          println("Previous Max index "+maxIndex)
          //          println("Tag name "+tagNames(maxIndex))

        }
      }
    }
    return seq
  }
}
