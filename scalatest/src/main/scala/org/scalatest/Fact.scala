/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest

import org.scalactic.Prettifier
import java.text.MessageFormat
import scala.collection.GenTraversable

sealed abstract class Fact {
    val rawFailureMessage: String
    val rawNegatedFailureMessage: String
    val rawMidSentenceFailureMessage: String
    val rawMidSentenceNegatedFailureMessage: String
    val failureMessageArgs: IndexedSeq[Any]
    val negatedFailureMessageArgs: IndexedSeq[Any]
    val midSentenceFailureMessageArgs: IndexedSeq[Any]
    val midSentenceNegatedFailureMessageArgs: IndexedSeq[Any]
    val prettifier: Prettifier


  /**
   * Construct failure message to report if a fact fails, using <code>rawFailureMessage</code>, <code>failureMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message to report if a fact fails
   */
  def failureMessage: String = makeString(rawFailureMessage, failureMessageArgs)

  /**
   * Construct message with a meaning opposite to that of the failure message, using <code>rawNegatedFailureMessage</code>, <code>negatedFailureMessageArgs</code> and <code>prettifier</code>
   *
   * @return message with a meaning opposite to that of the failure message
   */
  def negatedFailureMessage: String = makeString(rawNegatedFailureMessage, negatedFailureMessageArgs)

  /**
   * Construct failure message suitable for appearing mid-sentence, using <code>rawMidSentenceFailureMessage</code>, <code>midSentenceFailureMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message suitable for appearing mid-sentence
   */
  def midSentenceFailureMessage: String = makeString(rawMidSentenceFailureMessage, midSentenceFailureMessageArgs)

  /**
   * Construct negated failure message suitable for appearing mid-sentence, using <code>rawMidSentenceNegatedFailureMessage</code>, <code>midSentenceNegatedFailureMessageArgs</code> and <code>prettifier</code>
   *
   * @return negated failure message suitable for appearing mid-sentence
   */
  def midSentenceNegatedFailureMessage: String = makeString(rawMidSentenceNegatedFailureMessage, midSentenceNegatedFailureMessageArgs)

  private def makeString(rawString: String, args: IndexedSeq[Any]): String = {
    if (args.isEmpty) {
      rawString
    } else {
      MessageFormat.format(rawString, args.map(prettifier):_*)
    }
  }

  /**
   * Get a negated version of this Fact, sub type will be negated and all messages field will be substituted with its counter-part.
   *
   * @return a negated version of this Fact
   */
    def unary_!(): Fact

    def ||(rhs: => Fact): Fact

    def &&(rhs: => Fact): Fact

    //override def toString: String = Fact.buildToString(this)

    def asString: String = Fact.buildToString(this)

    private[scalatest] def complexity: Int

    private[scalatest] def isYes: Boolean
}

private[scalatest] object Fact {
  private[scalatest] def buildToString(fact: Fact): String = if (fact.complexity < 3) simpleToString(fact) else complexToString(fact)

  private[scalatest] def simpleToString(fact: Fact): String = {
    import FactOperator._
    fact match {
      case composite: CompositeFact =>
        val innerText = if (composite.isYes) {
          composite.operator match {
            case && => MessageFormat.format(Resources("commaAnd"), simpleToString(composite.lhs), simpleToString(composite.rhs))
            case || if composite.lhs.isYes =>
              simpleToString(composite.lhs) // short-circuit
            case || => MessageFormat.format(Resources("commaBut", simpleToString(composite.lhs), simpleToString(composite.rhs)))
          }
        } else {
          composite.operator match {
            case && if (!composite.lhs.isYes) =>
              simpleToString(composite.lhs) // short-circuit
            case && => MessageFormat.format(Resources("commaBut", simpleToString(composite.lhs), simpleToString(composite.rhs)))
            case || => MessageFormat.format(Resources("commaAnd"), simpleToString(composite.lhs), simpleToString(composite.rhs))
          }
        }
        yesOrNo(composite) + "(" + innerText + ")"
      case negated: NegatedFact => negated.negated match {
        case simple: SimpleFact => println("Here: " + negated.getClass + ", " + simple); simple.negatedFailureMessage
        case negated: NegatedFact => simpleToString(negated.negated)
        case composite: CompositeFact => "!" + simpleToString(negated.negated) // TODO: Work in progress
      }
      case simple: SimpleFact =>
        if (simple.isYes) simple.negatedFailureMessage else simple.failureMessage
    }
  }

  def yesOrNo(fact: Fact) = if (fact.isYes) "Yes" else "No"

  def indentation(level: Int) = "  " * level
  def indentLines(level: Int, lines: GenTraversable[String]) =
    lines.map(line => line.split("\n").map(indentation(level) + _).mkString("\n"))

  private[scalatest] def complexToString(fact: Fact): String = {
    fact match {
      case composite: CompositeFact =>
        yesOrNo(fact) + "(\n" +
        indentLines(1, Seq(complexToString(composite.lhs), complexToString(composite.rhs))).mkString(" " + composite.operator+"\n") +
        "\n)"
      case negated: NegatedFact => "!" + complexToString(negated.negated)
      case simple: SimpleFact => yesOrNo(fact) + "(" + simpleToString(fact) + ")"
    }
  }

/*
  // Idea is to override toString each time it is used.
  private[scalatest] sealed abstract class LazyMessage {
    val nestedArgs: IndexedSeq[Any]
  }

  private[scalatest] case class FailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.failureMessageArgs
    override def toString: String = fact.failureMessage
  }

  private[scalatest] case class NegatedFailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.negatedFailureMessageArgs
    override def toString: String = fact.negatedFailureMessage
  }

  private[scalatest] case class MidSentenceFailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.failureMessageArgs
    override def toString: String = fact.midSentenceFailureMessage
  }

  private[scalatest] case class MidSentenceNegatedFailureMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.negatedFailureMessageArgs
    override def toString: String = fact.midSentenceNegatedFailureMessage
  }
  */
}

import org.scalatest.Fact._

private[scalatest] sealed trait SimpleFact { self: Fact =>
  def complexity = 1
  val rawFailureMessage: String
  val rawNegatedFailureMessage: String
  val rawMidSentenceFailureMessage: String
  val rawMidSentenceNegatedFailureMessage: String
  val failureMessageArgs: IndexedSeq[Any]
  val negatedFailureMessageArgs: IndexedSeq[Any]
  val midSentenceFailureMessageArgs: IndexedSeq[Any]
  val midSentenceNegatedFailureMessageArgs: IndexedSeq[Any]
  val prettifier: Prettifier

}

private[scalatest] sealed trait Yes extends Fact {
  def unary_!() = NegatedToNo(this)
  def &&(rhs: => Fact) = rhs match {
    case yes: Yes => CompositeYes(FactOperator.&&, this, rhs)
    case no:  No  => CompositeNo(FactOperator.&&, this, rhs)
  }
  def ||(rhs: => Fact) = CompositeYes(FactOperator.||, this, rhs)

  def isYes = true
}

private[scalatest] sealed trait No extends Fact {
  def unary_!() = NegatedToYes(this)
  def &&(rhs: => Fact) = CompositeNo(FactOperator.&&, this, rhs)
  def ||(rhs: => Fact) = rhs match {
    case yes: Yes => CompositeYes(FactOperator.||, this, rhs)
    case no:  No  => CompositeNo(FactOperator.||, this, rhs)
  }

  def isYes = false
}

private[scalatest] case class NegatedToNo(negated: Yes) extends No with NegatedFact
private[scalatest] case class NegatedToYes(negated: No) extends Yes with NegatedFact

private[scalatest] sealed trait FactOperator
private[scalatest] object FactOperator {
  private[scalatest] case object && extends FactOperator
  private[scalatest] case object || extends FactOperator
}

private[scalatest] trait CompositeFact { self: Fact =>
  val operator: FactOperator
  val lhs: Fact
  val rhs: Fact

  val composite: Boolean = ???
  val failureMessageArgs: IndexedSeq[Any] = ???
  val midSentenceFailureMessageArgs: IndexedSeq[Any] = ???
  val midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = ???
  val negatedFailureMessageArgs: IndexedSeq[Any] = ???
  val prettifier: org.scalactic.Prettifier = ???
  val rawFailureMessage: String = ???
  val rawMidSentenceFailureMessage: String = ???
  val rawMidSentenceNegatedFailureMessage: String = ???
  val rawNegatedFailureMessage: String = ???

  def complexity = lhs.complexity + rhs.complexity
}

private[scalatest] case class CompositeYes(operator: FactOperator, lhs: Fact, rhs: Fact) extends Yes with CompositeFact
private[scalatest] case class CompositeNo(operator: FactOperator, lhs: Fact, rhs: Fact)  extends No  with CompositeFact

private[scalatest] trait NegatedFact { self: Fact =>
  val negated: Fact

  def complexity = negated.complexity

  val failureMessageArgs: IndexedSeq[Any] = negated.negatedFailureMessageArgs
  val midSentenceFailureMessageArgs: IndexedSeq[Any] = negated.midSentenceNegatedFailureMessageArgs
  val midSentenceNegatedFailureMessageArgs: IndexedSeq[Any] = negated.midSentenceFailureMessageArgs
  val negatedFailureMessageArgs: IndexedSeq[Any] = negated.failureMessageArgs
  val prettifier: org.scalactic.Prettifier = negated.prettifier
  val rawFailureMessage: String = negated.rawNegatedFailureMessage
  val rawMidSentenceFailureMessage: String = negated.rawMidSentenceNegatedFailureMessage
  val rawMidSentenceNegatedFailureMessage: String = negated.rawMidSentenceFailureMessage
  val rawNegatedFailureMessage: String = negated.rawFailureMessage
}


private[scalatest] case class SimpleNo(
	rawFailureMessage: String,
    rawNegatedFailureMessage: String,
    rawMidSentenceFailureMessage: String,
    rawMidSentenceNegatedFailureMessage: String,
    failureMessageArgs: IndexedSeq[Any],
    negatedFailureMessageArgs: IndexedSeq[Any],
    midSentenceFailureMessageArgs: IndexedSeq[Any],
    midSentenceNegatedFailureMessageArgs: IndexedSeq[Any],
    prettifier: Prettifier = Prettifier.default
) extends No with SimpleFact {
}

/**
 * Companion object for the <code>No</code> case class.
 *
 * @author Bill Venners
 */
private[scalatest] object No {

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>failureMessage</code>, 
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>, 
   * <code>midSentenceNegatedFailureMessage</code>, <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
   * <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFailureMessageArgs</code>
   * and <code>midSentenceNegatedFailureMessageArgs</code>.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]): No =
    new SimpleNo(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>failureMessage</code>,
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>,
   * <code>midSentenceNegatedFailureMessage</code>, <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
   * <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFailureMessageArgs</code>
   * and <code>midSentenceNegatedFailureMessageArgs</code>.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @param midSentenceFailureMessageArgs arguments for constructing failure message to report if a match fails
   * @param midSentenceNegatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any],
      midSentenceFailureMessageArgs: IndexedSeq[Any], midSentenceNegatedFailureMessageArgs: IndexedSeq[Any]): No =
    new SimpleNo(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      midSentenceFailureMessageArgs,
      midSentenceNegatedFailureMessageArgs,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFailureMessage</code>, and
   * <code>rawMidSentenceNegatedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create No with eager error messages, and its mid-sentence messages need to be different.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String): No =
    new SimpleNo(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>, and
   * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create No with eager error messages that have same mid-sentence messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String): No =
    new SimpleNo(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code> and <code>args</code> fields.  The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will use <code>args</code> as arguments.
   * This is suitable to create No with lazy error messages that have same mid-sentence messages and arguments.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param args arguments for error messages construction
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, args: IndexedSeq[Any]): No =
    new SimpleNo(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      args,
      args,
      args,
      args,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>No</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>failureMessageArgs</code> and <code>negatedFailureMessageArgs</code> fields.
   * The <code>rawMidSentenceFailureMessage</code> will return the same string as <code>rawFailureMessage</code>, and the
   * <code>rawMidSentenceNegatedFailureMessage</code> will return the same string as <code>rawNegatedFailureMessage</code>.
   * The <code>midSentenceFailureMessageArgs</code> will return the same as <code>failureMessageArgs</code>, and the
   * <code>midSentenceNegatedFailureMessageArgs</code> will return the same as <code>negatedFailureMessageArgs</code>.
   * This is suitable to create No with lazy error messages that have same mid-sentence and use different arguments for
   * negated messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>No</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]): No =
    new SimpleNo(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      Prettifier.default
    )
}


private[scalatest] case class SimpleYes(
	rawFailureMessage: String,
    rawNegatedFailureMessage: String,
    rawMidSentenceFailureMessage: String,
    rawMidSentenceNegatedFailureMessage: String,
    failureMessageArgs: IndexedSeq[Any],
    negatedFailureMessageArgs: IndexedSeq[Any],
    midSentenceFailureMessageArgs: IndexedSeq[Any],
    midSentenceNegatedFailureMessageArgs: IndexedSeq[Any],
    prettifier: Prettifier = Prettifier.default) extends Yes with SimpleFact {

  override def toString: String = s"Yes($negatedFailureMessage)"
}

/**
 * Companion object for the <code>Yes</code> case class.
 *
 * @author Bill Venners
 */
private[scalatest] object Yes {

  /**
   * Factory method that constructs a new <code>Yes</code> with passed code>failureMessage</code>, 
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>, 
   * <code>midSentenceNegatedFailureMessage</code>, <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
   * <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFailureMessageArgs</code>
   * and <code>midSentenceNegatedFailureMessageArgs</code>.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]): Yes =
    new SimpleYes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      Prettifier.default
    )


  /**
   * Factory method that constructs a new <code>Yes</code> with passed code>failureMessage</code>,
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>,
   * <code>midSentenceNegatedFailureMessage</code>, <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> fields.
   * <code>failureMessageArgs</code>, and <code>negatedFailureMessageArgs</code> will be used in place of <code>midSentenceFailureMessageArgs</code>
   * and <code>midSentenceNegatedFailureMessageArgs</code>.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @param midSentenceFailureMessageArgs arguments for constructing failure message to report if a match fails
   * @param midSentenceNegatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any],
             midSentenceFailureMessageArgs: IndexedSeq[Any], midSentenceNegatedFailureMessageArgs: IndexedSeq[Any]): Yes =
    new SimpleYes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      midSentenceFailureMessageArgs,
      midSentenceNegatedFailureMessageArgs,
      Prettifier.default
    )


  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFailureMessage</code>, and
   * <code>rawMidSentenceNegatedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create Yes with eager error messages, and its mid-sentence messages need to be different.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param rawMidSentenceFailureMessage raw failure message to report if a match fails
   * @param rawMidSentenceNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, rawMidSentenceFailureMessage: String,
      rawMidSentenceNegatedFailureMessage: String): Yes =
    new SimpleYes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawMidSentenceFailureMessage,
      rawMidSentenceNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>, and
   * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
   * This is suitable to create Yes with eager error messages that have same mid-sentence messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String): Yes =
    new SimpleYes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code> and <code>args</code> fields.  The <code>rawMidSentenceFailureMessage</code> will return the same
   * string as <code>rawFailureMessage</code>, and the <code>rawMidSentenceNegatedFailureMessage</code> will return the
   * same string as <code>rawNegatedFailureMessage</code>.  All argument fields will use <code>args</code> as arguments.
   * This is suitable to create Yes with lazy error messages that have same mid-sentence messages and arguments.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param args arguments for error messages construction
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, args: IndexedSeq[Any]) =
    new SimpleYes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      args,
      args,
      args,
      args,
      Prettifier.default
    )

  /**
   * Factory method that constructs a new <code>Yes</code> with passed <code>rawFailureMessage</code>,
   * <code>rawNegativeFailureMessage</code>, <code>failureMessageArgs</code> and <code>negatedFailureMessageArgs</code> fields.
   * The <code>rawMidSentenceFailureMessage</code> will return the same string as <code>rawFailureMessage</code>, and the
   * <code>rawMidSentenceNegatedFailureMessage</code> will return the same string as <code>rawNegatedFailureMessage</code>.
   * The <code>midSentenceFailureMessageArgs</code> will return the same as <code>failureMessageArgs</code>, and the
   * <code>midSentenceNegatedFailureMessageArgs</code> will return the same as <code>negatedFailureMessageArgs</code>.
   * This is suitable to create Yes with lazy error messages that have same mid-sentence and use different arguments for
   * negated messages.
   *
   * @param rawFailureMessage raw failure message to report if a match fails
   * @param rawNegatedFailureMessage raw message with a meaning opposite to that of the failure message
   * @param failureMessageArgs arguments for constructing failure message to report if a match fails
   * @param negatedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
   * @return a <code>Yes</code> instance
   */
  def apply(rawFailureMessage: String, rawNegatedFailureMessage: String, failureMessageArgs: IndexedSeq[Any], negatedFailureMessageArgs: IndexedSeq[Any]) =
    new SimpleYes(
      rawFailureMessage,
      rawNegatedFailureMessage,
      rawFailureMessage,
      rawNegatedFailureMessage,
      failureMessageArgs,
      negatedFailureMessageArgs,
      failureMessageArgs,
      negatedFailureMessageArgs,
      Prettifier.default
    )
}
