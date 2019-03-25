package chess

import chess.FlowableUtils.fromIterable
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable
import io.reactivex.schedulers.Schedulers

import scala.collection.AbstractIterable

//todo in parallel using cores;
// gc high again
// compilations errors - https://www.jetbrains.com/help/idea/troubleshoot-common-scala-issues.html
// remove redundancies in this file
sealed abstract class Belt[A] {

  def flowable(): Flowable[A]

  def flatMap[B](f: A => Belt[B]): Belt[B]
}


object Belt {

  def apply[A](): Belt[A] = EmptyBelt[A]()

  def apply[A](a: A): Belt[A] = SingletonBelt(a)

  def apply[A](iterator: Iterator[A])(inParallel: Boolean): Belt[A] = {

    implicit class RichIterator(iteratorA: Iterator[A]) {// 7.5S to 18S todo compute median
      def toOneTimeIterable: Iterable[A] = new AbstractIterable[A](){
        override def iterator: Iterator[A] = iteratorA
      }
    }
    if (inParallel)
      ParallelFlowableBelt(iterator.toOneTimeIterable)
    else
      FlowableBelt(iterator.toOneTimeIterable)
  }
}

object EmptyBelt extends Belt[Nothing] {
  def apply[A](): Belt[A] = EmptyBelt.asInstanceOf[Belt[A]]

  override def flatMap[B](f: Nothing => Belt[B]): Belt[B] = EmptyBelt()

  override def flowable(): Flowable[Nothing] = Flowable.empty()
}

object FlowableBelt {
  def apply[A](iterableA: Iterable[A]): Belt[A] = {
    val flow: Flowable[A] = fromIterable(iterableA)
    FlowableBelt(flow)
  }
}

object ParallelFlowableBelt {
  def apply[A](iterableA: Iterable[A]): ParallelFlowableBelt[A] = apply(fromIterable(iterableA))

  def apply[A](flowable: Flowable[A]): ParallelFlowableBelt[A] =
    ParallelFlowableBelt(flowable.parallel().runOn(Schedulers.computation()))
}

case class SingletonBelt[A](a: A) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] = f(a)

  override def flowable(): Flowable[A] = Flowable.just(a)
}

case class IteratorBelt[A](iteratorA: Iterator[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    IteratorBelt(iteratorA.flatMap(a => {
      f(a) match {
        case EmptyBelt => Iterator()
        case SingletonBelt(aa) => Iterator(aa)
        case IteratorBelt(iteratorB) => iteratorB
        case IterableBelt(iterableB) => iterableB.iterator
      }
    }))

  override def flowable(): Flowable[A] = ??? // fromIterable(iteratorA.toIterable)
}


case class IterableBelt[A](iterableA: Iterable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    IterableBelt(iterableA.flatMap(a => {
      f(a) match {
        case EmptyBelt => Iterable()
        case SingletonBelt(aa) => Iterable(aa)
        case IterableBelt(iterableB) => iterableB
      }
    }))

  override def flowable(): Flowable[A] = fromIterable(iterableA)
}

case class FlowableBelt[A](flowableA: Flowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    FlowableBelt(flowableA.flatMap(a => f(a).flowable()))

  override def flowable(): Flowable[A] = flowableA
}

case class ParallelFlowableBelt[A](parFlowableA: ParallelFlowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    ParallelFlowableBelt(
      parFlowableA.flatMap(a => {
        f(a).flowable()
      }))

  override def flowable(): Flowable[A] = parFlowableA.sequential()
}
