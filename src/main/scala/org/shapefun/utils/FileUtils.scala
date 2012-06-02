package org.shapefun.utils

import java.io.File

/**
 *
 */
object FileUtils {

  // So, why doesn't Scala offer File.read(), like every sane programming language invented this milennia?
  def readFile(file: File): String = {
    val source = scala.io.Source.fromFile(file)
    val lines = source .mkString
    source.close ()
    lines
  }


}