package org.shapefun.parser.syntaxtree

import org.shapefun.parser.Context
import org.shapefun.utils.StepRange
import collection.immutable.BitSet
import java.util.ArrayList

/**
 *
 */
case class ForLoop(loopVars: List[ForLoopVar], body: SyntaxNode) extends SyntaxNode {
  def checkTypes() {
    loopVars foreach {_.checkTypes()}
  }

  def returnType() = classOf[Unit]

  def calculate(context: Context): AnyRef = {
    // Create loop vars in subcontext

    // TODO: Optimized version for loop with only one loop variable

    val loopContext = context.createSubContext()

    var ranges: List[StepRange] = Nil

    val numLoops = loopVars.size
    val loopStarts = new Array[Double](numLoops)
    val loopEnds = new Array[Double](numLoops)
    val loopValues = new Array[Double](numLoops)
    val loopIncrements = new Array[Double](numLoops)
    val loopNames = new ArrayList[Symbol]()
    var inclusive = BitSet()
    var endWhenLower = BitSet()

    var i = 0
    loopVars foreach {loopVar =>
      // TODO: Support collection ranges too
      val range: StepRange = loopVar.range.calculate(context).asInstanceOf[StepRange]

      loopContext.addVar(loopVar.name, range.start)
      loopStarts(i) = range.start
      loopEnds(i) = range.end
      loopValues(i) = range.start
      loopIncrements(i) = range.stepIncrement
      loopNames.add(loopVar.name)
      if (range.inclusive) inclusive += i
      if (range.end < range.start) endWhenLower += i

      ranges ::= range
      i += 1
    }

    // Loop last loop once, next to last loop once for each step in the last loop, and so on

    val epsilon: Double = 0.00000001
    def rollLoop(loop: Int): Boolean = {
      if (loop < 0) true
      else {
        loopValues(loop) = loopStarts(loop)
        loopContext.setVar(loopNames.get(loop), Double.box(loopValues(loop)))
        stepLoop(loop - 1)
      }
    }

    def loopCompleted(loop: Int): Boolean = {
      if (endWhenLower(loop)){
        if (inclusive(loop)){
          loopValues(loop) < loopEnds(loop) + epsilon
        }
        else {
          loopValues(loop) <= loopEnds(loop) + epsilon
        }
      }
      else {
        if (inclusive(loop)){
          loopValues(loop) > loopEnds(loop) - epsilon
        }
        else {
          loopValues(loop) >= loopEnds(loop) - epsilon
        }
      }
    }

    def stepLoop(loop: Int): Boolean = {
      if (loop < 0) true
      else {
        loopValues(loop) += loopIncrements(loop)
        loopContext.setVar(loopNames.get(loop), Double.box(loopValues(loop)))

        if (loopCompleted(loop)) rollLoop(loop) else false
      }
    }

    var completed = true

    var l = 0
    while(completed && l < numLoops) {
      if (!loopCompleted(l)) completed = false
      l += 1
    }

    while (!completed) {
      body.calculate(loopContext)

      // Increment first loop, if it completes, reset it and increment next loop, etc
      completed = stepLoop(numLoops - 1)
    }

    Unit
  }
}