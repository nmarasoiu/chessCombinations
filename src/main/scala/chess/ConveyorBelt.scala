package chess

import chess.FlowableUtils._
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable

import scala.collection.parallel.ParIterable

sealed abstract class ConveyorBelt[A] {
  def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B]
  def toFlowable: Flowable[A]
  def toIterable: Iterable[A]
}

abstract class ScalaConveyorBelt[A] extends ConveyorBelt[A] {
  override def toFlowable: Flowable[A] = fromIterable(toIterable)
  def toParIterable: ParIterable[A]
}

abstract class RxConveyorBelt[A] extends ConveyorBelt[A] {
  def toParallelFlowable: ParallelFlowable[A] = toFlowable.parallelOnComputation()
  def toIterable: Iterable[A] = ??? // we do not want to block the thread in prod code
}

object ConveyorBelt {
  def apply[A](): ConveyorBelt[A] = EmptyConveyorBelt[A]()
  def apply[A](a: A): ConveyorBelt[A] = SingletonConveyorBelt(a)
  def apply[A](iterable: Iterable[A])(inParallel: Boolean): ConveyorBelt[A] = {
    if (inParallel)
      ParallelFlowableConveyorBelt(iterable)
    else
      FlowableConveyorBelt(iterable)
  }
}

object EmptyConveyorBelt extends ConveyorBelt[Nothing] {
  private val emptyIterable = Iterable()
  private val emptyFlowable = Flowable.empty()
  override def toIterable: Iterable[Nothing] = emptyIterable
  override def toFlowable: Flowable[Nothing] = emptyFlowable
  def apply[A](): ConveyorBelt[A] = EmptyConveyorBelt.asInstanceOf[ConveyorBelt[A]]
  override def flatMap[B](f: Nothing => ConveyorBelt[B]): ConveyorBelt[B] = EmptyConveyorBelt()
}

case class SingletonConveyorBelt[A](a: A) extends ConveyorBelt[A] {
  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = f(a)
  override def toIterable: Iterable[A] = Iterable(a)
  override def toFlowable: Flowable[A] = Flowable.just(a)
}

object FlowableConveyorBelt {
  def apply[A](iterable: Iterable[A]): ConveyorBelt[A] = FlowableConveyorBelt(fromIterable(iterable))
}

object ParallelFlowableConveyorBelt {
  def apply[A](iterableA: Iterable[A]): ParallelFlowableConveyorBelt[A] = apply(fromIterable(iterableA))
  def apply[A](flowable: Flowable[A]): ParallelFlowableConveyorBelt[A] = ParallelFlowableConveyorBelt(flowable.parallelOnComputation())
}

case class FlowableConveyorBelt[A](flowableA: Flowable[A]) extends RxConveyorBelt[A] {
  override def toFlowable: Flowable[A] = flowableA
  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = FlowableConveyorBelt(flowableA.flatMap(a => f(a).toFlowable))
}

case class ParallelFlowableConveyorBelt[A](parFlowableA: ParallelFlowable[A]) extends RxConveyorBelt[A] {
  override def toParallelFlowable: ParallelFlowable[A] = parFlowableA
  override def toFlowable: Flowable[A] = parFlowableA.sequential()
  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = ParallelFlowableConveyorBelt(parFlowableA.flatMap(a => f(a).toFlowable))
}
//todo why it generates so much memory consumption, both as uncollectable as well as collectable?
case class IterableConveyorBelt[A](iterableA: Iterable[A]) extends ScalaConveyorBelt[A] {
  override def toIterable: Iterable[A] = iterableA
  override def toParIterable: ParIterable[A] = toIterable.par
  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = IterableConveyorBelt(iterableA.flatMap(a => f(a).toIterable))
}

case class ParallelIterableConveyorBelt[A](parIterableA: ParIterable[A]) extends ScalaConveyorBelt[A] {
  override def toParIterable: ParIterable[A] = parIterableA
  override def toIterable: Iterable[A] = parIterableA.seq
  override def flatMap[B](f: A => ConveyorBelt[B]): ConveyorBelt[B] = ParallelIterableConveyorBelt(parIterableA.flatMap(a => f(a).toIterable))
}