import scala.xml._
import scala.util.Try

object XmlUpdater {
  def modifyXml(node: Elem, path: List[String], value: String): Elem = {
    path match {
      case Nil => node
      case "#text" :: Nil =>
        node.copy(child = Text(value))
      case attr :: Nil if attr.startsWith("@") =>
        node % Attribute(None, attr.drop(1), Text(value), Null)
      case tag :: rest => {
        val existingChild = node \ tag
        val isNextPathNumber = rest.length > 0 && isInt(rest.head)
        // If next path is a number, go to that element in the existing child. Else take the first one
        val childNum = if (isNextPathNumber) rest.head.toInt else 0
        // If next path is a number, remove that from path because we will process it here
        val restPath = if (isNextPathNumber) rest.tail else rest

        var missingChildren: Seq[Elem] = Seq.empty[Elem]
        if (existingChild.length < childNum + 1) {
          // If the child does not exist, create it
          missingChildren = (existingChild.length to childNum).map { _ =>
            Elem(null, tag, Null, TopScope, minimizeEmpty = true)
          }
        }
        val newElem = node.copy(child = node.child ++ missingChildren)
        var tagIndex: Int = 0
        val updatedChildren = newElem.child.zipWithIndex.map {
          case (child: Elem, index) if child.label == tag => {
            val updatedChild =
              if (tagIndex == childNum) modifyXml(child, restPath, value)
              else child.asInstanceOf[Node]

            tagIndex = tagIndex + 1
            updatedChild
          }
          case (otherChild, _) => otherChild
        }
        node.copy(child = updatedChildren)
      }
    }
  }

  def isInt(str: String): Boolean = Try(str.toInt).isSuccess

  def removeEmptyXml(node: Node): Node = node match {
    case elem: Elem =>
      val updatedChild = elem.child.map(removeEmptyXml).filter {
        case e: Elem if e.text.trim.nonEmpty => true
        case t: Text if t.text.trim.nonEmpty => true
        case _                               => false
      }
      elem.copy(child = updatedChild)
    case other => other
  }
}
