import scala.xml._
import scala.util.Try
import scala.io.Source
import play.api.libs.json._
import java.nio.file.{Paths, Files, StandardOpenOption}
import com.opencsv.CSVParser

object MainWorld {
  def main(args: Array[String]): Unit = {
    val csvFilePath = "/home/vinod/Code/Latha/Files/claims.csv"
    val xmlFilePath = "/home/vinod/Code/Latha/Files/out.xml"
    val headerFilePath = "/home/vinod/Code/Latha/Files/header.json"

    val csvParser = new CSVParser()
    val csvData = scala.io.Source.fromFile(csvFilePath).getLines().toList
    val data: List[Array[String]] = csvData.tail.map { line =>
      csvParser.parseLine(line)
    }

    val jsonString = Source.fromFile(headerFilePath).getLines().mkString
    val json: JsValue = Json.parse(jsonString)
    val columns: Array[String] =
      (json \ "columns").asOpt[Seq[String]].getOrElse(Seq.empty[String]).toArray

    val xml = convertToXML(columns, data)
    val xmlString = new PrettyPrinter(120, 2).format(xml)
    Files.write(
      Paths.get(xmlFilePath),
      xmlString.getBytes,
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    println("Done");
  }

  def convertToXML(columns: Array[String], data: List[Array[String]]): Elem = {
    var rootNode = <request></request>
    val firstRow = data(0)

    for (colIndex <- columns.indices) {
      val path = columns(colIndex).split("\\.")
      val columnValue = firstRow(colIndex)
      if (columnValue.trim() != "")
        rootNode = XmlUpdater.modifyXml(rootNode, path.toList, columnValue)
    }

    rootNode = XmlUpdater.removeEmptyXml(rootNode).asInstanceOf[Elem]

    rootNode
  }

}
