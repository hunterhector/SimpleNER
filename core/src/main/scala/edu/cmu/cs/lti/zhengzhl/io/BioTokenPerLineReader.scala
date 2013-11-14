package edu.cmu.cs.lti.zhengzhl.io

import java.io.File
import edu.cmu.cs.lti.zhengzhl.model.Token

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 11/13/13
 * Time: 9:49 PM
 */
class BioTokenPerLineReader(dataFile: File) extends TokenPerLineReader(dataFile) {
  /**
   * The bio task has a different file format
   * @param line
   * @return
   */
  override def constructToken(line: String): Token = Token.train(line.stripSuffix("\n").split("\t"))
}
