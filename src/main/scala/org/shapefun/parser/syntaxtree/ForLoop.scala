package org.shapefun.parser.syntaxtree

import org.shapefun.utils.StepRange
import collection.immutable.BitSet
import java.util.ArrayList
import org.shapefun.parser.{UnitKind, JavaKind, Kind, Context}

/**
 *
 */
case class ForLoop(loopVars: List[ForLoopVar], body: Expr) extends Expr {


  protected def doCalculateTypes(staticContext: StaticContext): Kind = {

    // Check loop ranges
    loopVars foreach { loopVar =>
      ensureExprIsAssignableTo(JavaKind(classOf[RangeExpr]), loopVar.range, staticContext)
    }

    // Check body
    body.calculateTypes(staticContext)

    UnitKind
  }

  def calculate(context: Context): AnyRef = {
    // Create loop vars in subcontext

    // TODO: Optimized version for loop with only one loop variable

    val loopContext = context.createSubContext()

    var ranges: List[StepRange] = Nil

    val numLoops = loopVars.size
    val loopStarts = new Array[Double](numLoops)
    val loopEnds = new Array[Double](numLoops)
    val loopValues = new Array[Double](numLoops)
    val loopCounts = new Array[Int](numLoops)
    val loopTimes = new Array[Int](numLoops)
    val loopIncrements = new Array[Double](numLoops)
    val loopNames = new ArrayList[Symbol]()
    val loopCountNames = new ArrayList[Symbol]()
    val loopProgressNames = new ArrayList[Symbol]()
    var inclusive = BitSet()
    var endWhenLower = BitSet()

    var i = 0
    loopVars foreach {loopVar =>
      // TODO: Support collection ranges too
      val range: StepRange = loopVar.range.calculate(context).asInstanceOf[StepRange]

      loopContext.addVar(loopVar.name, range.start)
      loopCounts(i) = 0
      loopStarts(i) = range.start
      loopEnds(i) = range.end
      loopValues(i) = range.start
      loopIncrements(i) = range.stepIncrement
      loopTimes(i) = range.totalSteps
      loopNames.add(loopVar.name)
      loopCountNames.add(Symbol(loopVar.name.name + "_index"))
      loopProgressNames.add(Symbol(loopVar.name.name + "_place"))
      if (range.inclusive) inclusive += i
      if (range.countDown) endWhenLower += i

      loopContext.addVar(loopCountNames.get(i), Double.box(0))
      loopContext.addVar(loopProgressNames.get(i), Double.box(0))

      ranges ::= range
      i += 1
    }

    // Do not run loops with any zero increments
    if (loopIncrements exists(increment => math.abs(increment) < Num.Epsilon)) return Unit

    // Loop last loop once, next to last loop once for each step in the last loop, and so on

    def rollLoop(loop: Int): Boolean = {
      if (loop < 0) true
      else {
        loopCounts(loop) = 0
        loopValues(loop) = loopStarts(loop)
        loopContext.setVar(loopNames.get(loop),         Double.box(loopValues(loop)))
        loopContext.setVar(loopCountNames.get(loop),    Double.box(loopCounts(loop)))
        loopContext.setVar(loopProgressNames.get(loop), Double.box(0))
        stepLoop(loop - 1)
      }
    }

    def loopCompleted(loop: Int): Boolean = {
      if (endWhenLower(loop)){
        if (inclusive(loop)){
          loopValues(loop) < loopEnds(loop) - Num.Epsilon
        }
        else {
          loopValues(loop) <= loopEnds(loop) + Num.Epsilon
        }
      }
      else {
        if (inclusive(loop)){
          loopValues(loop) > loopEnds(loop) + Num.Epsilon
        }
        else {
          loopValues(loop) >= loopEnds(loop) - Num.Epsilon
        }
      }
    }

    def stepLoop(loop: Int): Boolean = {
      if (loop < 0) true
      else {
        loopCounts(loop) += 1
        loopValues(loop) += loopIncrements(loop)
        loopContext.setVar(loopNames.get(loop), Double.box(loopValues(loop)))
        loopContext.setVar(loopCountNames.get(loop), Double.box(loopCounts(loop)))
        loopContext.setVar(loopProgressNames.get(loop), Double.box(1.0 * loopCounts(loop) / loopTimes(loop)))

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