import scala.xml._
import scala.xml.transform._
import scala.compiletime.ops.boolean
import XmlUpdater.modifyXml


object TestCases {
  def main(args: Array[String]): Unit = {
    runcases()
  }

  def runcase(
      cse: Int,
      xml: String,
      path: Array[String],
      value: String,
      expected: String
  ): Boolean = {
    val modifiedXml = XmlUpdater.modifyXml(XML.loadString(xml), path.toList, value)
    if (modifiedXml.toString() != expected) {
      println("Case failed: " + cse)
      println("Source:   " + xml)
      println("Modified: " + modifiedXml)
      println("Expected: " + expected)
      println()
      false
    } else
      println("Case passed: " + cse)
      true
  }

  def runcases(): Unit = {
    var failedCases: Int = 0

    // Create multiple non-existing child path
    if (
      !runcase(
        1,
        "<root><child1><child2></child2></child1></root>",
        Array("child1", "child2", "child3", "newChild", "#text"),
        "Updated Value",
        "<root><child1><child2><child3><newChild>Updated Value</newChild></child3></child2></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // Create path at root level
    if (
      !runcase(
        2,
        "<root><child1><child2/></child1></root>",
        Array("child2", "#text"),
        "Updated Value",
        "<root><child1><child2/></child1><child2>Updated Value</child2></root>"
      )
    )
      failedCases = failedCases + 1

    // Update value of existing element
    if (
      !runcase(
        3,
        "<root><child1><child2/></child1></root>",
        Array("child1", "child2", "#text"),
        "Updated child",
        "<root><child1><child2>Updated child</child2></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // override an existing child path
    if (
      !runcase(
        4,
        "<root><child1><child2/></child1></root>",
        Array("child1", "#text"),
        "Updated child",
        "<root><child1>Updated child</child1></root>"
      )
    )
      failedCases = failedCases + 1

    // No #text, do nothing as path exists
    if (
      !runcase(
        5,
        "<root><child1><child2/></child1></root>",
        Array("child1", "child2"),
        "Updated child",
        "<root><child1><child2/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // No #text, create path and leave it
    if (
      !runcase(
        6,
        "<root><child1><child2/></child1></root>",
        Array("child1", "child3"),
        "Updated child",
        "<root><child1><child2/><child3/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // Add attribute to existing element
    if (
      !runcase(
        7,
        "<root><child1><child2/></child1></root>",
        Array("child1", "child2", "@myatr"),
        "atrval",
        "<root><child1><child2 myatr=\"atrval\"/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // Add attribute to non-existing element
    if (
      !runcase(
        8,
        "<root><child1><child2/></child1></root>",
        Array("child1", "child3", "@myatr"),
        "atrval",
        "<root><child1><child2/><child3 myatr=\"atrval\"/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // update attribute
    if (
      !runcase(
        9,
        "<root><child1 myatr=\"value1\"><child2/></child1></root>",
        Array("child1", "@myatr"),
        "atrval",
        "<root><child1 myatr=\"atrval\"><child2/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // Add attribute at multiple non-existing child path
    if (
      !runcase(
        10,
        "<root><child1><child2></child2></child1></root>",
        Array("child1", "child2", "child3", "newChild", "@atre"),
        "value attr",
        "<root><child1><child2><child3><newChild atre=\"value attr\"/></child3></child2></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // Add second attribute
    if (
      !runcase(
        11,
        "<root><child1 myatr=\"value1\"><child2/></child1></root>",
        Array("child1", "@myatr2"),
        "atrval",
        "<root><child1 myatr2=\"atrval\" myatr=\"value1\"><child2/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // Add text when multiple elements exist
    if (
      !runcase(
        12,
        "<root><child1><child2/><child2/></child1></root>",
        Array("child1", "child2", "#text"),
        "text value",
        "<root><child1><child2>text value</child2><child2/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // different element exists in same path
    if (
      !runcase(
        13,
        "<root><child1><child2/><child2/><child3/></child1></root>",
        Array("child1", "child2", "#text"),
        "text value",
        "<root><child1><child2>text value</child2><child2/><child3/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // counter on elements
    if (
      !runcase(
        14,
        "<root><child1><child2/><child2/><child3/></child1></root>",
        Array("child1", "child2", "1", "#text"),
        "text value",
        "<root><child1><child2/><child2>text value</child2><child3/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // counter on elements, with another item in middle
    if (
      !runcase(
        15,
        "<root><child1><child2/><child4/><child2/><child3/></child1></root>",
        Array("child1", "child2", "1", "#text"),
        "text value",
        "<root><child1><child2/><child4/><child2>text value</child2><child3/></child1></root>"
      )
    )
      failedCases = failedCases + 1

    // counter on elements, big counter
    if (
      !runcase(
        16,
        "<root><child1><child2/><child2/><child3/></child1></root>",
        Array("child1", "child2", "6", "#text"),
        "text val",
        "<root><child1><child2/><child2/><child3/><child2/><child2/><child2/><child2/><child2>text val</child2></child1></root>"
      )
    )
      failedCases = failedCases + 1

    println()
    println("Failed cases: " + failedCases)
  }
}
