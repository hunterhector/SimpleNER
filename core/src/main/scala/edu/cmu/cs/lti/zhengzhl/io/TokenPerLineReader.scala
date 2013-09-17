package edu.cmu.cs.lti.zhengzhl.io

import java.io.File
import scala.io.Source
import edu.cmu.cs.lti.zhengzhl.model.Token
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 9/14/13
 * Time: 12:24 AM
 */
class TokenPerLineReader(dataFile: File) extends SentenceReader {

  private var sentCounter = -1

  private var tags = new mutable.HashSet[String]()

  val sentences = getSentences(dataFile)

  def getSentences(dataFile: File): ListBuffer[ListBuffer[Token]] = {
    val sents = ListBuffer[ListBuffer[Token]]()
    Source.fromFile(dataFile).getLines().foreach(line => {
      if (line == null || line.isEmpty() || line.trim().isEmpty()) {
        val emptySent = new ListBuffer[Token]
        sents.append(emptySent)
      } else {
        if (sents.length == 0) {
          val emptySent = new ListBuffer[Token]
          sents.append(emptySent)
        }

        val token = Token(line.stripSuffix("\n").split(" "))
        tags += token.ner
        sents(sents.length - 1).append(token)
      }
    })
    sents.map(sent => {
      sent.append(Token.stop())
      sent
    })
  }

  def getTags = tags.toArray

  def nextSentence(): List[Token] = {
    sentCounter += 1
    sentences(sentCounter).toList
  }

  def hasNext(): Boolean = sentCounter + 1 < sentences.size
}
