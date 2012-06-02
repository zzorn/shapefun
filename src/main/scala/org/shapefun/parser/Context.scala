package org.shapefun.parser

import defs._
import syntaxtree.{CallExpr, Num, StaticContext}


/**
 *
 */
trait Context {

  /**
   * @return True if definition exists locally or inherited from parent context
   */
  def hasDef(name: Symbol): Boolean

  def hasVal(name: Symbol): Boolean = hasDef(name) && classOf[ValDef].isInstance(getDef(name))
  def hasVar(name: Symbol): Boolean = hasDef(name) && classOf[VarDef].isInstance(getDef(name))
  def hasFun(name: Symbol): Boolean = hasDef(name) && classOf[FunDef].isInstance(getDef(name))
  def hasValue(name: Symbol): Boolean = hasDef(name) && classOf[ValueDef].isInstance(getDef(name))
  def hasExtFun(key: (Class[_], Symbol)): Boolean

  /**
   * Adds a definition, throws an exception if it is already defined locally
   */
  def addDef(definition: Def)

  def addVal(name: Symbol, value: AnyRef) {addDef(new ValDef(name, value))}
  def addVal(name: Symbol, value: Double) {addDef(new ValDef(name, Double.box(value)))}
  def addVar(name: Symbol, value: AnyRef) {addDef(new VarDef(name, value))}
  def addVar(name: Symbol, value: Double) {addDef(new VarDef(name, Double.box(value)))}
  def addFun(funDef: FunDef) {addDef(funDef)}
  def addExtFun(hostType: Class[_], funDef: FunDef)

  /**
   * @return the definition with the specified name, or throw exception if not found
   */
  def getDef(name: Symbol): Def

  def getVal(name: Symbol): AnyRef = getDef(name).asInstanceOf[ValDef].value
  def getVar(name: Symbol): AnyRef = getDef(name).asInstanceOf[VarDef].value
  def getValue(name: Symbol): AnyRef = getDef(name).asInstanceOf[ValueDef].value
  def getFun(name: Symbol): FunDef = getDef(name).asInstanceOf[FunDef]
  def getExtFun(key: (Class[_], Symbol)): FunDef

  /**
   * Sets variable, throws exception if not found.
   * Throws class cast if it is not a variable.
   */
  def setVar(name: Symbol, value: AnyRef)

  /**
   * @return parent context, or null if this context has no parent
   */
  def parent: Context

  /**
   * @return a new context with this context as parent
   */
  def createSubContext(): Context

  def createStaticContext(): StaticContext




  // Helper functions for adding pre-defined functions to the context

  def addFunc2[H <: AnyRef, P1 <: AnyRef, P2 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             p2: Symbol = 'b,
             k1: Kind   = AnyRefKind,
             k2: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1, P2) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1),
      ParamInfo(p2, k2)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2]
        )
    }))
  }

  def addFun0[R <: AnyRef](
             name: Symbol,
             result: Kind = AnyRefKind)
           (f: () => R) {
    addFun(ExternalFunDef(name, List(), result, {
      args =>
        f()
    }))
  }

  def addFun1[P1 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             k1: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1]
        )
    }))
  }

  def addFun2[P1 <: AnyRef, P2 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             p2: Symbol = 'b,
             k1: Kind   = AnyRefKind,
             k2: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1, P2) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1),
      ParamInfo(p2, k2)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2]
        )
    }))
  }


  def addFun3[P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             p2: Symbol = 'b,
             p3: Symbol = 'c,
             k1: Kind   = AnyRefKind,
             k2: Kind   = AnyRefKind,
             k3: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1, P2, P3) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3]
        )
    }))
  }


  def addFun4[P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, P4 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             p2: Symbol = 'b,
             p3: Symbol = 'c,
             p4: Symbol = 'd,
             k1: Kind   = AnyRefKind,
             k2: Kind   = AnyRefKind,
             k3: Kind   = AnyRefKind,
             k4: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1, P2, P3, P4) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3),
      ParamInfo(p4, k4)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3],
          args(p4).asInstanceOf[P4]
        )
    }))
  }


  def addFun5[P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, P4 <: AnyRef, P5 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             p2: Symbol = 'b,
             p3: Symbol = 'c,
             p4: Symbol = 'd,
             p5: Symbol = 'e,
             k1: Kind   = AnyRefKind,
             k2: Kind   = AnyRefKind,
             k3: Kind   = AnyRefKind,
             k4: Kind   = AnyRefKind,
             k5: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1, P2, P3, P4, P5) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3),
      ParamInfo(p4, k4),
      ParamInfo(p5, k5)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3],
          args(p4).asInstanceOf[P4],
          args(p5).asInstanceOf[P5]
        )
    }))
  }


  def addFun6[P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, P4 <: AnyRef, P5 <: AnyRef, P6 <: AnyRef, R <: AnyRef](
             name: Symbol,
             p1: Symbol = 'a,
             p2: Symbol = 'b,
             p3: Symbol = 'c,
             p4: Symbol = 'd,
             p5: Symbol = 'e,
             p6: Symbol = 'f,
             k1: Kind   = AnyRefKind,
             k2: Kind   = AnyRefKind,
             k3: Kind   = AnyRefKind,
             k4: Kind   = AnyRefKind,
             k5: Kind   = AnyRefKind,
             k6: Kind   = AnyRefKind,
             result: Kind = AnyRefKind)
           (f: (P1, P2, P3, P4, P5, P6) => R) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3),
      ParamInfo(p4, k4),
      ParamInfo(p5, k5),
      ParamInfo(p6, k6)
    ), result, {
      args =>
        f(
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3],
          args(p4).asInstanceOf[P4],
          args(p5).asInstanceOf[P5],
          args(p6).asInstanceOf[P6]
        )
    }))
  }



  def addExtFun0[H <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  result: Kind = AnyRefKind)
                 (f: (H) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType))
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H]
        )
    }))
  }

  def addExtFun1[H <: AnyRef, P1 <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  p1: Symbol = 'a,
                  k1: Kind   = AnyRefKind,
                  result: Kind = AnyRefKind)
                 (f: (H, P1) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType)),
      ParamInfo(p1, k1)
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H],
          args(p1).asInstanceOf[P1]
        )
    }))
  }

  def addExtFun2[H <: AnyRef, P1 <: AnyRef, P2 <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  k1: Kind   = AnyRefKind,
                  k2: Kind   = AnyRefKind,
                  result: Kind = AnyRefKind)
                 (f: (H, P1, P2) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType)),
      ParamInfo(p1, k1),
      ParamInfo(p2, k2)
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H],
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2]
        )
    }))
  }



  def addExtFun3[H <: AnyRef, P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  k1: Kind   = AnyRefKind,
                  k2: Kind   = AnyRefKind,
                  k3: Kind   = AnyRefKind,
                  result: Kind = AnyRefKind)
                 (f: (H, P1, P2, P3) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType)),
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3)
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H],
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3]
        )
    }))
  }


  def addExtFun4[H <: AnyRef, P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, P4 <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  p4: Symbol = 'd,
                  k1: Kind   = AnyRefKind,
                  k2: Kind   = AnyRefKind,
                  k3: Kind   = AnyRefKind,
                  k4: Kind   = AnyRefKind,
                  result: Kind = AnyRefKind)
                 (f: (H, P1, P2, P3, P4) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType)),
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3),
      ParamInfo(p4, k4)
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H],
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3],
          args(p4).asInstanceOf[P4]
        )
    }))
  }

  def addExtFun5[H <: AnyRef, P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, P4 <: AnyRef, P5 <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  p4: Symbol = 'd,
                  p5: Symbol = 'e,
                  k1: Kind   = AnyRefKind,
                  k2: Kind   = AnyRefKind,
                  k3: Kind   = AnyRefKind,
                  k4: Kind   = AnyRefKind,
                  k5: Kind   = AnyRefKind,
                  result: Kind = AnyRefKind)
                 (f: (H, P1, P2, P3, P4, P5) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType)),
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3),
      ParamInfo(p4, k4),
      ParamInfo(p5, k5)
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H],
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3],
          args(p4).asInstanceOf[P4],
          args(p5).asInstanceOf[P5]
        )
    }))
  }


  def addExtFun6[H <: AnyRef, P1 <: AnyRef, P2 <: AnyRef, P3 <: AnyRef, P4 <: AnyRef, P5 <: AnyRef, P6 <: AnyRef, R <: AnyRef](
                  hostType: Class[H],
                  name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  p4: Symbol = 'd,
                  p5: Symbol = 'e,
                  p6: Symbol = 'f,
                  k1: Kind   = AnyRefKind,
                  k2: Kind   = AnyRefKind,
                  k3: Kind   = AnyRefKind,
                  k4: Kind   = AnyRefKind,
                  k5: Kind   = AnyRefKind,
                  k6: Kind   = AnyRefKind,
                  result: Kind = AnyRefKind)
                 (f: (H, P1, P2, P3, P4, P5, P6) => R) {
    addExtFun(hostType, ExternalFunDef(name, List(
      ParamInfo(CallExpr.SelfParamName, JavaKind(hostType)),
      ParamInfo(p1, k1),
      ParamInfo(p2, k2),
      ParamInfo(p3, k3),
      ParamInfo(p4, k4),
      ParamInfo(p5, k5),
      ParamInfo(p6, k6)
    ), result, {
      args =>
        f(
          args(CallExpr.SelfParamName).asInstanceOf[H],
          args(p1).asInstanceOf[P1],
          args(p2).asInstanceOf[P2],
          args(p3).asInstanceOf[P3],
          args(p4).asInstanceOf[P4],
          args(p5).asInstanceOf[P5],
          args(p5).asInstanceOf[P6]
        )
    }))
  }






  def addNumFun0(name: Symbol)
                 (f: => Double) {
    addFun(ExternalFunDef(name, List(), Num.Kind, { args =>
      Double.box(f)
    }))
  }

  def addNumFun1(name: Symbol,
                  p1: Symbol = 'a)
                 (f: Double => Double) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, Num.Kind)
    ), Num.Kind, {
      args =>
        Double.box(f(
          Double.unbox(args(p1))
        ))
    }))
  }

  def addNumFun2(name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b)
                 (f: (Double, Double) => Double) {
    addFun(    ExternalFunDef(name, List(
      ParamInfo(p1, Num.Kind),
      ParamInfo(p2, Num.Kind)
    ), Num.Kind, {
      args =>
        Double.box(f(
          Double.unbox(args(p1)),
          Double.unbox(args(p2))
        ))
    }))
  }

  def addNumFun3(name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c)
                 (f: (Double, Double, Double) => Double) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, Num.Kind),
      ParamInfo(p2, Num.Kind),
      ParamInfo(p3, Num.Kind)
    ), Num.Kind, {
      args =>
        Double.box(f(
          Double.unbox(args(p1)),
          Double.unbox(args(p2)),
          Double.unbox(args(p3))
        ))
    }))
  }

  def addNumFun4(name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  p4: Symbol = 'd)
                 (f: (Double, Double, Double, Double) => Double) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, Num.Kind),
      ParamInfo(p2, Num.Kind),
      ParamInfo(p3, Num.Kind),
      ParamInfo(p4, Num.Kind)
    ), Num.Kind, {
      args =>
        Double.box(f(
          Double.unbox(args(p1)),
          Double.unbox(args(p2)),
          Double.unbox(args(p3)),
          Double.unbox(args(p4))
        ))
    }))
  }

  def addNumFun5(name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  p4: Symbol = 'd,
                  p5: Symbol = 'e)
                 (f: (Double, Double, Double, Double, Double) => Double) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, Num.Kind),
      ParamInfo(p2, Num.Kind),
      ParamInfo(p3, Num.Kind),
      ParamInfo(p4, Num.Kind),
      ParamInfo(p5, Num.Kind)
    ), Num.Kind, {
      args =>
        Double.box(f(
          Double.unbox(args(p1)),
          Double.unbox(args(p2)),
          Double.unbox(args(p3)),
          Double.unbox(args(p4)),
          Double.unbox(args(p5))
        ))
    }))
  }

  def addNumFun6(name: Symbol,
                  p1: Symbol = 'a,
                  p2: Symbol = 'b,
                  p3: Symbol = 'c,
                  p4: Symbol = 'd,
                  p5: Symbol = 'e,
                  p6: Symbol = 'f)
                 (f: (Double, Double, Double, Double, Double, Double) => Double) {
    addFun(ExternalFunDef(name, List(
      ParamInfo(p1, Num.Kind),
      ParamInfo(p2, Num.Kind),
      ParamInfo(p3, Num.Kind),
      ParamInfo(p4, Num.Kind),
      ParamInfo(p5, Num.Kind),
      ParamInfo(p6, Num.Kind)
    ), Num.Kind, {
      args =>
        Double.box(f(
          Double.unbox(args(p1)),
          Double.unbox(args(p2)),
          Double.unbox(args(p3)),
          Double.unbox(args(p4)),
          Double.unbox(args(p5)),
          Double.unbox(args(p6))
        ))
    }))
  }




}