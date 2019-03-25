package chess

import java.util

import chess.Enrichments.RichIterator
import chess.FlowableUtils.fromIterable
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable
import io.reactivex.schedulers.Schedulers

import scala.collection.parallel.ParIterable
import scala.collection.parallel.mutable.ParSeq

sealed abstract class Belt[A] {
  def flatMap[B](f: A => Belt[B]): Belt[B]

  def toFlowable: Flowable[A]
}

object EmptyBelt extends Belt[Nothing] {
  def apply[A](): Belt[A] = EmptyBelt.asInstanceOf[Belt[A]]

  override def flatMap[B](f: Nothing => Belt[B]): Belt[B] = EmptyBelt()

  override def toFlowable: Flowable[Nothing] = Flowable.empty()
}

case class SingletonBelt[A](a: A) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] = f(a)

  override def toFlowable: Flowable[A] = Flowable.just(a)
}

case class FlowableBelt[A](flowableA: Flowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    FlowableBelt(flowableA.flatMap(a => f(a).toFlowable))

  override def toFlowable: Flowable[A] = flowableA
}

object FlowableBelt {
  def apply[A](iterableA: Iterable[A]): Belt[A] = {
    FlowableBelt(fromIterable(iterableA))
  }
}

case class ParallelFlowableBelt[A](parFlowableA: ParallelFlowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    ParallelFlowableBelt(parFlowableA.flatMap(a => f(a).toFlowable))

  override def toFlowable: Flowable[A] = parFlowableA.sequential()
}

object ParallelFlowableBelt {
  def apply[A](iterableA: Iterable[A]): ParallelFlowableBelt[A] = apply(fromIterable(iterableA))

  def apply[A](flowable: Flowable[A]): ParallelFlowableBelt[A] =
    ParallelFlowableBelt(flowable.parallel().runOn(Schedulers.computation()))
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

  override def toFlowable: Flowable[A] = fromIterable(iteratorA.toOneTimeIterable)
}

case class ParIterableBelt[A](parIterableA: ParIterable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    ParIterableBelt(parIterableA.flatMap(a => {
      f(a) match {
        case EmptyBelt => Iterable()
        case SingletonBelt(aa) => Iterable(aa)
        case IteratorBelt(iteratorB) => iteratorB.toOneTimeIterable
        case IterableBelt(iterableB) => iterableB
      }
    }))

  override def toFlowable: Flowable[A] = fromIterable(parIterableA.seq)
}

object ParIteratorBelt {
  def apply[A](iterator: Iterator[A]): Belt[A] = {
    val vector = iterator.toVector
    val trackCount = Runtime.getRuntime.availableProcessors()
    val batchSize = vector.length / trackCount
    import scala.collection.JavaConverters._
    val flow: Flowable[A] = fromIterable(vector)
    val batchedFlow: Flowable[util.List[A]] = flow.buffer(batchSize)

    val par: ParSeq[util.List[A]] = batchedFlow.toList.blockingGet().asScala.par
    ParIterableBelt(par)
      .flatMap(lst => IterableBelt(lst.asScala))
  }
}

case class IterableBelt[A](iterableA: Iterable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    IterableBelt(iterableA.flatMap(a => {
      f(a) match {
        case EmptyBelt => Iterable()
        case SingletonBelt(aa) => Iterable(aa)
        case IteratorBelt(iteratorB) => iteratorB.toOneTimeIterable
        case IterableBelt(iterableB) => iterableB
      }
    }))

  override def toFlowable: Flowable[A] = fromIterable(iterableA)
}

object Belt {
  def apply[A](): Belt[A] = EmptyBelt[A]()

  def apply[A](a: A): Belt[A] = SingletonBelt(a)

  def apply[A](iterator: Iterator[A])(inParallel: Boolean): Belt[A] = {
    if (inParallel) ParallelFlowableBelt(iterator.toOneTimeIterable) else FlowableBelt(iterator.toOneTimeIterable)
    //        if (inParallel) ParIteratorBelt(iterator) else IteratorBelt(iterator)
  }
}
