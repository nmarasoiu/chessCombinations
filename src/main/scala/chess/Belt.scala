package chess

import chess.FlowableUtils.{fromIterable, _}
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable

import scala.collection.parallel.ParIterable

sealed abstract class Belt[A] {
  def flatMap[B](f: A => Belt[B]): Belt[B]
  def toIterable: Iterable[A]
  def toFlowable: Flowable[A]
  def toParallelFlowable: ParallelFlowable[A] = toFlowable.parallelOnComputation()
}

object Belt {
  def apply[A](): Belt[A] = EmptyBelt[A]()
  def apply[A](a: A): Belt[A] = SingletonBelt(a)
  def apply[A](iterable: Iterable[A])(inParallel: Boolean): Belt[A] =
    if (inParallel) ParallelFlowableBelt(iterable) else FlowableBelt(iterable)
}

object EmptyBelt extends Belt[Nothing] {
  def apply[A](): Belt[A] = EmptyBelt.asInstanceOf[Belt[A]]
  override def flatMap[B](f: Nothing => Belt[B]): Belt[B] = EmptyBelt()
  override def toIterable: Iterable[Nothing] = Iterable()
  override def toFlowable: Flowable[Nothing] = Flowable.empty()
}

case class SingletonBelt[A](a: A) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] = f(a)
  override def toFlowable: Flowable[A] = Flowable.just(a)
  override def toIterable: Iterable[A] = Iterable(a)
}

object FlowableBelt {
  def apply[A](iterable: Iterable[A]): Belt[A] = FlowableBelt(fromIterable(iterable))
}

object ParallelFlowableBelt {
  def apply[A](iterableA: Iterable[A]): ParallelFlowableBelt[A] = apply(fromIterable(iterableA))
  def apply[A](flowable: Flowable[A]): ParallelFlowableBelt[A] = ParallelFlowableBelt(flowable.parallelOnComputation())
}

case class IterableBelt[A](iterableA: Iterable[A]) extends Belt[A] {
  override def toIterable: Iterable[A] = iterableA
  override def toFlowable: Flowable[A] = fromIterable(toIterable)
  override def flatMap[B](f: A => Belt[B]): Belt[B] = IterableBelt(iterableA.flatMap(a => f(a).toIterable))
}

case class ParallelIterableBelt[A](parIterableA: ParIterable[A]) extends Belt[A] {
  override def toIterable: Iterable[A] = parIterableA.seq
  override def toFlowable: Flowable[A] = fromIterable(toIterable)
  override def flatMap[B](f: A => Belt[B]): Belt[B] = ParallelIterableBelt(parIterableA.flatMap(a => f(a).toIterable))
}

case class FlowableBelt[A](flowableA: Flowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] = FlowableBelt(flowableA.flatMap(a => f(a).toFlowable))

  override def toFlowable: Flowable[A] = flowableA
  override def toIterable: Iterable[A] = ???
}

case class ParallelFlowableBelt[A](parFlowableA: ParallelFlowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] = ParallelFlowableBelt(parFlowableA.flatMap(a => f(a).toFlowable))

  override def toParallelFlowable: ParallelFlowable[A] = parFlowableA
  override def toFlowable: Flowable[A] = parFlowableA.sequential()
  override def toIterable: Iterable[A] = ???
}
