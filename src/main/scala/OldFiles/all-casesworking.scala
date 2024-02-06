import scala.xml._
import scala.util.Try

object XmlUpdater_all {
  def modifyXml(xml: Elem, path: Array[String], value: String): Elem = {
    def modifyNode(node: Node, path: List[String], value: String): Node = {
      path match {
        case Nil => node
        case "#text" :: Nil =>
          node match {
            case elem: Elem => elem.copy(child = Text(value))
            case _          => node
          }
        case attr :: Nil if attr.startsWith("@") =>
          node match {
            case elem: Elem =>
              elem % Attribute(None, attr.drop(1), Text(value), Null)
            case _ => node
          }
        case tag :: rest =>
          node match {
            case elem: Elem =>
              val existingChild = elem \ tag
              val isNextPathNumber = rest.length > 0 && isInt(rest.head)
              // If next path is a number, go to that element in the existing child. Else take the first one
              val childNum = if (isNextPathNumber) rest.head.toInt else 0
              val restPath = if (isNextPathNumber) rest.tail else rest

              var missingChildren: Seq[Elem] = Seq.empty[Elem]
              if (existingChild.length < childNum + 1) {
                // If the child does not exist, create it
                missingChildren = (existingChild.length to childNum).map { _ =>
                  Elem(null, tag, Null, TopScope, minimizeEmpty = true)
                }
              }
              val newElem = elem.copy(child = elem.child ++ missingChildren)
              var tagIndex: Int = 0
              val updatedChildren = newElem.child.zipWithIndex.map {
                case (child: Elem, index) if child.label == tag => {
                  val updatedChild =
                    if (tagIndex == childNum) modifyNode(child, restPath, value)
                    else child.asInstanceOf[Node]

                  tagIndex = tagIndex + 1
                  updatedChild
                }
                case (otherChild, _) => otherChild
              }
              elem.copy(child = updatedChildren)
            case _ => node
          }
      }
    }
    // Start modification from the root
    val modified = modifyNode(xml, path.toList, value)
    modified.headOption.getOrElse(xml).asInstanceOf[Elem]
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
