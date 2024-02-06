import scala.xml._

def modifyXml_6(xml: Elem, path: Array[String], value: String): Elem = {
  def modifyNode(node: Node, path: List[String], value: String): Seq[Node] = {
    path match {
      case Nil => node
      case "#text" :: Nil =>
        node match {
          case elem: Elem => Seq(elem.copy(child = Text(value)))
          case _          => Seq(Text(value))
        }
      case tag :: rest =>
        node match {
          case elem: Elem =>
            val existingChild = elem \ tag
            if (existingChild.isEmpty) {
              // If the child does not exist, create it
              val newChild = tag match {
                case "#text" => Text(value)
                case _ =>
                  modifyNode(
                    Elem(null, tag, Null, TopScope, minimizeEmpty = true),
                    rest,
                    value
                  ).headOption.getOrElse(<dummy/>)
              }
              Seq(elem.copy(child = elem.child ++ newChild))
            } else {
              // If the child exists, update it
              val updatedChildren = elem.child.map {
                case child: Elem if child.label == tag =>
                  modifyNode(child, rest, value).headOption.getOrElse(child)
                case other => other
              }
              Seq(elem.copy(child = updatedChildren))
            }
          case _ => Seq(node)
        }
    }
  }

  // Start modification from the root
  val modified = modifyNode(xml, path.toList, value)
  modified.headOption.getOrElse(xml).asInstanceOf[Elem]
}
