package org.shapefun.parser


import defs.{ExprFunDef, ParamInfo}
import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import java.lang.String
import syntaxtree._


/**
 *
 */
class ShapeLangParser extends Parser {

  def parse(expression: String): Expr = {
    val parsingResult = ReportingParseRunner(InputLine).run(expression)
    parsingResult.result match {
      case Some(i) => i
      case None => throw new ParsingException("Invalid expression:\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }
  }

  def InputLine = rule { BlockContents ~ WhiteSpace ~ EOI }

  def BlockContents: Rule1[Expr] = rule {
    zeroOrMore(Statement, StatementSeparator) ~~> {(statements) => BlockExpr(statements)}  ~ optional(" ;")
  }

  def StatementSeparator: Rule0 = rule {
    NonNewlineWhiteSpace ~ "\n" ~ WhiteSpace |
    " ;"
  }

  def Statement: Rule1[Expr] = rule {
    For |
    VarDef |
    Func |
    VarAssig |
    Block |
    Expression
  }

  def KindConstruct: Rule1[Kind] = rule {
    " :" ~ SimpleKind
  }

  def SimpleKind: Rule1[Kind] = rule {
    AllowedName ~~> {s: Symbol => Kind(s)}
  }

  def Block: Rule1[Expr] = rule {
    " {" ~ BlockContents ~ " }"
  }

  def VarDef: Rule1[Expr] = rule {
    " var" ~ AllowedName ~ " =" ~ Expression ~~> {(name: Symbol, initialValue: Expr) => VarDefinition(name, initialValue)}
  }

  def Func: Rule1[Expr] = rule {
    " def" ~ AllowedName ~ " (" ~ zeroOrMore(FuncParam, " ,") ~ " )" ~ optional(KindConstruct) ~ (" =" ~ Expression | Block) ~~>
      {(name: Symbol, params: List[ParamInfo], kind, body) =>
        FunDefExpr(name, params, kind.getOrElse(null), body)}
  }
  def FuncParam: Rule1[ParamInfo] = rule {
    AllowedName ~ optional(KindConstruct) ~ optional(" =" ~ Expression) ~~>
      {(id, kind, defVal) =>
        ParamInfo(id, kind.getOrElse(Num.Kind), defVal.getOrElse(null))}
  }

  def VarAssig: Rule1[Expr] = rule {
    AllowedName ~ AssignmentOp ~ Expression ~~> {(name: Symbol, op: Symbol, value: Expr) => VarAssignment(name, op, value)}
  }
  def AssignmentOp: Rule1[Symbol] = rule {
    WhiteSpace ~ group("+=" | "-=" | "*=" | "/=" | "=") ~> {s => Symbol(s)}
  }

  def For: Rule1[Expr] = rule {
    " for" ~ oneOrMore(ForLoopVarDef, " ,") ~ " do" ~ Statement ~~> {(loopVars, block) => ForLoop(loopVars, block)}
  }
  def ForLoopVarDef: Rule1[ForLoopVar] = rule {
    AllowedName ~ " in" ~ Expression ~~> {(name: Symbol, range: Expr) => ForLoopVar(name, range)}
  }

  def Expression: Rule1[Expr] = OrExpr

  def OrExpr: Rule1[Expr] = rule {
    XorExpr ~ zeroOrMore(
      " or" ~ XorExpr ~~> ((a:Expr, b:Expr) => BooleanOp(a, 'or, b).asInstanceOf[Expr])
    )
  }

  def XorExpr: Rule1[Expr] = rule {
    AndExpr ~ zeroOrMore(
      " xor" ~ AndExpr ~~> ((a:Expr, b:Expr) => BooleanOp(a, 'xor, b).asInstanceOf[Expr])
    )
  }

  def AndExpr: Rule1[Expr] = rule {
    NotExpr ~ zeroOrMore(
      " and" ~ NotExpr ~~> ((a:Expr, b:Expr) => BooleanOp(a, 'and, b).asInstanceOf[Expr])
    )
  }

  def NotExpr: Rule1[Expr] = rule {
    " not" ~ EqualityExpr ~~> {expr => Not(expr)} |
    EqualityExpr
  }

  def EqualityExpr: Rule1[Expr] = rule {
    ComparisonExpr ~ EqualitySymbol ~ ComparisonExpr ~~> {(a, sym, b) => EqualityComparisonOp(a, sym, b)} |
    ComparisonExpr
  }
  def EqualitySymbol: Rule1[Symbol] = rule { WhiteSpace ~ group("==" | "!=") ~> {s => Symbol(s)} }


  def ComparisonExpr: Rule1[Expr] = rule {
    Range ~ ComparisonSymbol ~ Range  ~ ComparisonSymbol ~ Range ~~> {(a, sym1, b, sym2, c) => ComparisonOp(a, sym1, b, sym2, c)} |
    Range ~ ComparisonSymbol ~ Range ~~> {(a, sym, b) => ComparisonOp(a, sym, b)} |
    Range
  }
  def ComparisonSymbol: Rule1[Symbol] = rule { WhiteSpace ~ group("<=" | ">=" | "<" | ">" ) ~> {s => Symbol(s)} }

  def Range: Rule1[Expr] = rule {
    RangeStartEndInclusive ~ " steps" ~ TermExpr ~~> {(start, end, inclusive, steps) => RangeExpr(start, end, inclusive, steps=steps)} |
    RangeStartEndInclusive ~ " step" ~ TermExpr ~~> {(start, end, inclusive, step) => RangeExpr(start, end, inclusive, step=step)} |
    RangeStartEndInclusive ~~> {(start, end, inclusive) => RangeExpr(start, end, inclusive)} |
    TermExpr
  }
  def RangeStartEndInclusive: Rule3[Expr, Expr, Boolean] = rule {
    TermExpr ~ " ..." ~ TermExpr ~ push(true) |
    TermExpr ~ " .." ~ TermExpr ~ push(false)
  }

  def TermExpr: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
        " +" ~ Term ~~> ((a:Expr, b:Expr) => NumberOp('plus, a, b).asInstanceOf[Expr])
      | " -" ~ Term ~~> ((a:Expr, b:Expr) => NumberOp('minus, a, b).asInstanceOf[Expr])
    )
  }

  def Term: Rule1[Expr] = rule {
    Factor ~ zeroOrMore(
        " *" ~ Factor ~~> ((a:Expr, b:Expr) => NumberOp('mul, a, b).asInstanceOf[Expr])
      | " /" ~ Factor ~~> ((a:Expr, b:Expr) => NumberOp('div, a, b).asInstanceOf[Expr])
    )
  }


  def Factor: Rule1[Expr] = rule {
    Call |
    Callable |
    NegativeExpr
  }

  def Callable: Rule1[Expr] = rule {
      Number |
      BooleanConst |
      Parens |
      If |
      VarInc |
      VarDec |
      VariableRef
  }


  def If: Rule1[Expr] = rule {
    " if" ~ Expression ~ " then" ~ Statement ~ " else" ~ Statement ~~> {(c: Expr, t: Expr, e: Expr) => IfExpr(c, t, e)} |
    " if" ~ Expression ~ " then" ~ Statement ~~> {(c: Expr, t: Expr) => IfExpr(c, t, Const(Unit))}
  }

  def NegativeExpr: Rule1[Expr] = rule { " -" ~ Factor ~~> {exp => Neg(exp)} }

  def VarInc: Rule1[Expr] = rule {AllowedName ~ " ++" ~~> {name => IncDecOp(name, increment = true)}}
  def VarDec: Rule1[Expr] = rule {AllowedName ~ " --" ~~> {name => IncDecOp(name, increment = false)}}

  def Parens: Rule1[Expr] = rule { " (" ~ Expression ~ " )" }

  def VariableRef: Rule1[Expr] = rule { AllowedName ~~> {s => ValueRefExpr(s)} }

  def Call: Rule1[Expr] = rule {
    FirstCall ~ zeroOrMore(
      " ." ~ CallIdAndParams ~~>
        {(obj: Expr, ident: Symbol, params: List[Expr]) =>
          CallExpr(Some(obj), ident, params).asInstanceOf[Expr]}
    )
  }
  def FirstCall: Rule1[Expr] = rule {
    optional(Callable ~ " .") ~ CallIdAndParams ~~>
      {(obj, ident, params) =>
        CallExpr(obj, ident, params)}
  }
  def CallIdAndParams: Rule2[Symbol, List[Expr]] = rule { Identifier ~ " (" ~ zeroOrMore(CallParam, separator=" ,") ~ " )"  }
  def CallParam: Rule1[Expr] = rule { Expression }

  def AllowedName: Rule1[Symbol] = Identifier // TODO: Exclude keywords
  def Identifier: Rule1[Symbol] = rule { WhiteSpace ~ group(LetterOrUnderscore ~ zeroOrMore(LetterOrUnderscore | Digit)) ~> {s => Symbol(s) }  }
  def LetterOrUnderscore = rule { "a" - "z" | "A" - "Z" | "_" }

  def BooleanConst: Rule1[Expr] = rule {
    " true"  ~> {_ => True } |
    " false" ~> {_ => False}
  }

  def Number: Rule1[Expr] = rule { WhiteSpace ~ group(Integer ~ optional(Fraction)) ~> (s => {Num(s.toDouble)}) }
  def Integer: Rule0 = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) } // No leading zero in an integer, except if there is just one digit
  def Fraction: Rule0 = rule { "." ~ Digits }
  def Digits: Rule0 = rule { oneOrMore(Digit) }
  def Digit: Rule0 = rule { "0" - "9" }

  def WhiteSpace: Rule0     = rule { zeroOrMore(anyOf("\n\r\t\f ").label("whitespace")) }
  def NonNewlineWhiteSpace: Rule0     = rule { zeroOrMore(anyOf("\t\f ").label("NonNewlineWhitespace")) }


  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with a blank.
   */
  override implicit def toRule(string: String) =
    if (string.startsWith(" "))
      WhiteSpace ~ str(string.substring(1))
    else
      str(string)


}