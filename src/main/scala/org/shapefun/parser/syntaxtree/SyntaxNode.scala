package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context

/**
 *
 */
trait SyntaxNode {

  def checkTypes()

  def returnType(): Class[_]

  def calculate(context: Context): AnyRef


}