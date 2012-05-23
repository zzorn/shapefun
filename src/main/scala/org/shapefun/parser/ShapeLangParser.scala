package org.shapefun.parser


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

  def InputLine = rule { WhiteSpace ~ Expression ~ EOI }


  def Expression: Rule1[Expr] = rule {
    Term ~ zeroOrMore(
        "+ " ~ Term ~~> ((a:Expr, b:Expr) => NumberOp('plus, a, b).asInstanceOf[Expr])
      | "- " ~ Term ~~> ((a:Expr, b:Expr) => NumberOp('minus, a, b).asInstanceOf[Expr])
    )
  }

  def Term: Rule1[Expr] = rule {
    Factor ~ zeroOrMore(
        "* " ~ Factor ~~> ((a:Expr, b:Expr) => NumberOp('mul, a, b).asInstanceOf[Expr])
      | "/ " ~ Factor ~~> ((a:Expr, b:Expr) => NumberOp('div, a, b).asInstanceOf[Expr])
    )
  }


  def Factor: Rule1[Expr] = rule {
    Call |
    Callable |
    NegativeExpr
  }

  def Callable: Rule1[Expr] = rule {
      Number |
      Parens |
      VariableRef
  }


  def NegativeExpr: Rule1[Expr] = rule { "- " ~ Factor ~~> {exp => Neg(exp)} }

  def Parens: Rule1[Expr] = rule { "( " ~ Expression ~ ") " }

  def VariableRef: Rule1[Expr] = rule { Identifier ~~> {s => VarRefExpr(s)} ~ WhiteSpace }

  def Call: Rule1[Expr] = rule {
    FirstCall ~ zeroOrMore(
      ". " ~ CallIdAndParams ~~>
        {(obj: Expr, ident: Symbol, params: List[Expr]) =>
          CallExpr(Some(obj), ident, params).asInstanceOf[Expr]}
    )
  }
  def FirstCall: Rule1[Expr] = rule {
    optional(Callable ~ ". ") ~ CallIdAndParams ~~>
      {(obj, ident, params) =>
        CallExpr(obj, ident, params)}
  }
  def CallIdAndParams: Rule2[Symbol, List[Expr]] = rule { Identifier ~ "( " ~ zeroOrMore(CallParam, separator=", ") ~ ") "  }
  def CallParam: Rule1[Expr] = rule { Expression }

  def Identifier: Rule1[Symbol] = rule { group(LetterOrUnderscore ~ zeroOrMore(LetterOrUnderscore | Digit)) ~> {s => Symbol(s) }  }
  def LetterOrUnderscore = rule { "a" - "z" | "A" - "Z" | "_" }

  def Number: Rule1[Expr] = rule { group(Integer ~ optional(Fraction)) ~> (s => {Num(s.toDouble)}) } ~ WhiteSpace
  def Integer: Rule0 = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) } // No leading zero in an integer, except if there is just one digit
  def Fraction: Rule0 = rule { "." ~ Digits }
  def Digits: Rule0 = rule { oneOrMore(Digit) }
  def Digit: Rule0 = rule { "0" - "9" }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  /**
   * We redefine the default string-to-rule conversion to also match trailing whitespace if the string ends with a blank.
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" "))
      str(string.trim) ~ WhiteSpace
    else
      str(string)


}