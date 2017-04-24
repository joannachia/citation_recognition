import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Main {
  val reportersFile = "src/main/resources/reporters.txt"
  val reporters: Seq[String] = Source.fromFile(reportersFile).getLines().toSeq
  val documentDirectory = "/Users/joanna.chia/Downloads/446c8aa0-6eba-11e5-bc7f-4851b79b387c"

  def main(args: Array[String]) = {
    val outputFileName = "results.csv"
    val listOfDocuments: List[File] = getListOfFiles(documentDirectory)

    val file = new File(outputFileName)
    val bw = new BufferedWriter(new FileWriter(file))

    //for each file in directory, find citation and write results
    listOfDocuments.foreach {
      d =>
        val citations = findCitation(documentDirectory, d.getName)
        writeResults(bw, citations)
    }

    bw.close()
  }

  def findCitation(directory: String, fileName: String): Seq[(String, Citation, Int)] = {
    val doc = readDocument(directory, fileName)

    //regex to match all types of citations
    val rCitation = """([0-9]+) (.*?) ([0-9]+(?!\w))""".r
    val rPincities = """([0-9]+) (.*?) ([0-9]+(?!\w)), ([0-9]+)""".r
    val rShorthand = """([0-9]+) (.*?),? at ([0-9]+(?!\w))""".r
    val citations = rCitation.findAllIn(doc.content).toSeq.map{
      m => m match {
        case rCitation(vol, rep, page) => Citation(vol.toLong, rep, page.toLong)
      }
    }
    val pincities = rPincities.findAllIn(doc.content).toSeq.map {
      m => m match {
        case rPincities(vol, rep, page, pin) => Citation(vol.toLong, rep, page.toLong, pin.toLong)
      }
    }
    val shortHand = rShorthand.findAllIn(doc.content).toSeq.map {
      m => m match {
        case rShorthand(vol, rep, pin) => Citation(vol.toLong, rep, 0, pin.toLong)
      }
    }

    // combining all regex matches, and cleaning out citations that are not found in reporters
    val allCitations = citations ++ pincities ++ shortHand
    val cleanedCitations = allCitations.flatMap{ c =>
      if (reporters.contains(c.reporter)) Some(c)
      else None}

    // grouping them based on reporter and summing them.
    val countedCitations: Map[String, Seq[Citation]] = cleanedCitations.groupBy(c => c.reporter )
    countedCitations.toSeq.map( c => (fileName, c._2.head, c._2.length))
  }

  def readDocument(directory: String, fileName: String): Document = {
    val content = Source.fromFile(directory + "/" + fileName).getLines().toSeq.foldLeft("")((b,a) => b+a)
    Document(fileName, content)
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def writeResults(bufferedWriter: BufferedWriter, results: Seq[(String, Citation, Int)]) = {
    results.foreach(line => bufferedWriter.write(line._1 + ", " + line._2.toString + ", " + line._3.toString + "\n"))
  }

}

case class Document (name: String, content: String)
case class Citation (volume: Long, reporter: String, page: Long, pinCities: Long = -1)


