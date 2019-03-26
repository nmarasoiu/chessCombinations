package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable

sealed abstract class ConveyorBelt[A] {
  def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B]

  def toFlowable: Flowable[A]
}

abstract class IteratorConveyorBelt[A] extends ConveyorBelt[A] {
  def toIterator: Iterator[A]
}

object ConveyorBelt {
  def apply[A](): ConveyorBelt[A] = EmptyConveyorBelt[A]()

  def apply[A](a: A): ConveyorBelt[A] = SingletonConveyorBelt(a)

  def apply[A](iterator: Iterator[A], inParallel: Boolean): ConveyorBelt[A] = {
    if (inParallel) ParallelFlowableConveyorBelt(iterator) else FlowableConveyorBelt(iterator)
  }
}

object EmptyConveyorBelt extends IteratorConveyorBelt[Nothing] {
  private val emptyIterator = Iterator()
  private val emptyFlowable = Flowable.empty()

  override def toFlowable: Flowable[Nothing] = emptyFlowable

  override def toIterator: Iterator[Nothing] = emptyIterator

  def apply[A](): ConveyorBelt[A] = EmptyConveyorBelt.asInstanceOf[ConveyorBelt[A]]

  override def flatMap[B](f: Nothing => ConveyorBelt[B]): ConveyorBelt[B] = EmptyConveyorBelt()
}

case class SingletonConveyorBelt[A](a: A) extends IteratorConveyorBelt[A] {
  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = f(a)

  override def toFlowable: Flowable[A] = Flowable.just(a)

  override def toIterator: Iterator[A] = Iterator(a)
}

object FlowableConveyorBelt {
  def apply[A](iterator: Iterator[A]): ConveyorBelt[A] = FlowableConveyorBelt(fromIterator(iterator))
}

object ParallelFlowableConveyorBelt {
  def apply[A](iteratorA: Iterator[A]): ParallelFlowableConveyorBelt[A] = apply(fromIterator(iteratorA))

  def apply[A](flowable: Flowable[A]): ParallelFlowableConveyorBelt[A] = ParallelFlowableConveyorBelt(flowable.parallelOnComputation())
}

case class FlowableConveyorBelt[A](flowableA: Flowable[A]) extends ConveyorBelt[A] {
  override def toFlowable: Flowable[A] = flowableA

  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] =
    FlowableConveyorBelt(flowableA.flatMap(a => f(a).toFlowable))
}

case class ParallelFlowableConveyorBelt[A](parFlowableA: ParallelFlowable[A]) extends ConveyorBelt[A] {
  override def toFlowable: Flowable[A] = parFlowableA.sequential()

  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] =
    ParallelFlowableConveyorBelt(parFlowableA.flatMap(a => f(a).toFlowable))
}

abstract class ScalaConveyorBelt[A] extends IteratorConveyorBelt[A] {
  def toIterator: Iterator[A]

  override def toFlowable: Flowable[A] = fromIterator(toIterator)
}

case class ScalaIteratorConveyorBelt[A](iteratorA: Iterator[A]) extends ScalaConveyorBelt[A] {
  override def toIterator: Iterator[A] = iteratorA

  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = {
    ScalaIteratorConveyorBelt(iteratorA.flatMap(a => f(a).asInstanceOf[IteratorConveyorBelt[B]].toIterator))
  }
}
