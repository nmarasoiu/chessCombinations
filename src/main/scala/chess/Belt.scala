package chess

import chess.FlowableUtils.fromIterable
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable
import io.reactivex.schedulers.Schedulers

import scala.collection.parallel.ParIterable

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


case class IterableBelt[A](iterableA: Iterable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    IterableBelt(iterableA.flatMap(a => {
      f(a) match {
        case EmptyBelt => Iterable()
        case SingletonBelt(aa) => Iterable(aa)
        case IterableBelt(iterableB) => iterableB
      }
    }))

  override def toFlowable: Flowable[A] = fromIterable(iterableA)
}

case class ParIterableBelt[A](parIterableA: ParIterable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    ParIterableBelt(parIterableA.flatMap(a => {
      f(a) match {
        case EmptyBelt => Iterable()
        case SingletonBelt(aa) => Iterable(aa)
        case IterableBelt(iterableB) => iterableB
      }
    }))

  override def toFlowable: Flowable[A] = fromIterable(parIterableA.seq)
}


case class FlowableBelt[A](flowableA: Flowable[A]) extends Belt[A] {
  override def flatMap[B](f: A => Belt[B]): Belt[B] =
    FlowableBelt(flowableA.flatMap(a => f(a).toFlowable))

  override def toFlowable: Flowable[A] = flowableA
}

object FlowableBelt {
  def apply[A](iterable: Iterable[A]): Belt[A] = {
    FlowableBelt(fromIterable(iterable))
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

object Belt {
  def apply[A](): Belt[A] = EmptyBelt[A]()

  def apply[A](a: A): Belt[A] = SingletonBelt(a)

  def apply[A](iterable: Iterable[A])(inParallel: Boolean): Belt[A] = {
    if (inParallel) ParallelFlowableBelt(iterable) else FlowableBelt(iterable)
  }
}
