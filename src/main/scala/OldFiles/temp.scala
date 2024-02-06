import scala.xml._

object XmlTagRemover extends App {
  // Example XML content
  val xmlContent =
    <root>
      <element1>Value1</element1>
      <element2></element2>
      <element3>Value3</element3>
      <element4/>
    </root>

  // Function to remove empty tags
  def removeEmptyTags(node: Node): Node = node match {
    case elem: Elem =>
      val updatedChild = elem.child.map(removeEmptyTags).filter {
        case e: Elem if e.text.trim.nonEmpty => true
        case t: Text if t.text.trim.nonEmpty => true
        case _                               => false
      }
      elem.copy(child = updatedChild)
    case other => other
  }

  // Remove empty tags from the XML content
  val modifiedXml = removeEmptyTags(xmlContent)

  // Print the modified XML
  println(modifiedXml)
}
