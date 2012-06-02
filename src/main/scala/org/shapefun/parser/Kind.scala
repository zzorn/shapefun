package org.shapefun.parser

import syntaxtree.StaticContext

/**
 *
 */
trait Kind {
  def name: Symbol

  def isAssignableFrom(otherKind: Kind): Boolean

  def commonSuperType(otherKind: Kind): Kind = {
    if (this.isAssignableFrom(otherKind)) this
    else if (otherKind.isAssignableFrom(this)) otherKind
    else AnyRefKind
  }
}

case class JavaKind(classType: Class[_]) extends Kind {
  def name = Symbol(classType.getName)
  def isAssignableFrom(otherKind: Kind): Boolean = {
    otherKind match {
      case o: JavaKind => classType.isAssignableFrom(o.classType)
      case UnitKind => false
      case _ => classType == classOf[AnyRef]
    }
  }
}

/*
case class KindRef(kindName: Symbol) extends Kind {
  def getReferencedKind(context: StaticContext): Kind = context.getKind(kindName)
  def name = kindName
  def isAssignableFrom(otherKind: Kind): Boolean = false
}
*/

object NumKind extends JavaKind(classOf[Double])

object BoolKind extends JavaKind(classOf[Boolean])

object UnitKind extends Kind {
  val name = 'Unit
  def isAssignableFrom(otherKind: Kind): Boolean = false
}

object AnyRefKind extends Kind {
  val name = 'AnyRef
  def isAssignableFrom(otherKind: Kind): Boolean = otherKind != UnitKind
}
