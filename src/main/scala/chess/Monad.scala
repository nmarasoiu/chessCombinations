package chess

import chess.FlowableUtils.fromIterable
import io.reactivex.Flowable
import io.reactivex.parallel.ParallelFlowable
import io.reactivex.schedulers.Schedulers

sealed abstract class Monad[A] {

  def flowable(): Flowable[A]

  def parallelFlowable(): ParallelFlowable[A] = flowable().parallel()

  def flatMap[B](f: A => Monad[B]): Monad[B]
}

object EmptyMonad extends Monad[Nothing] {
  def apply[A](): Monad[A] = EmptyMonad.asInstanceOf[Monad[A]]

  override def flatMap[B](f: Nothing => Monad[B]): Monad[B] = EmptyMonad()

  override def flowable(): Flowable[Nothing] = Flowable.empty()
}

object FlowableMonad {
  def apply[A](iterableA: Iterable[A]): Monad[A] = FlowableMonad(fromIterable(iterableA))
}

object ParallelFlowableMonad {
  def apply[A](iterableA: Iterable[A]): ParallelFlowableMonad[A] =
    ParallelFlowableMonad(fromIterable(iterableA).parallel().runOn(Schedulers.computation()))
}

case class SingletonMonad[A](a: A) extends Monad[A] {
  override def flatMap[B](f: A => Monad[B]): Monad[B] = f(a)

  override def flowable(): Flowable[A] = Flowable.just(a)
}

case class IteratorMonad[A](iteratorA: Iterator[A]) extends Monad[A] {
  override def flatMap[B](f: A => Monad[B]): Monad[B] =
    IteratorMonad(iteratorA.flatMap(a => {
      f(a) match {
        case EmptyMonad => Iterator()
        case SingletonMonad(aa) => Iterator(aa)
        case IteratorMonad(iteratorB) => iteratorB
        case IterableMonad(iterableB) => iterableB.iterator
      }
    }))

  override def flowable(): Flowable[A] = fromIterable(iteratorA.toIterable)
}

case class IterableMonad[A](iterableA: Iterable[A]) extends Monad[A] {
  override def flatMap[B](f: A => Monad[B]): Monad[B] =
    IterableMonad(iterableA.flatMap(a => {
      f(a) match {
        case EmptyMonad => Iterable()
        case SingletonMonad(aa) => Iterable(aa)
        case IterableMonad(iterableB) => iterableB
      }
    }))

  override def flowable(): Flowable[A] = fromIterable(iterableA)
}

case class FlowableMonad[A](flowableA: Flowable[A]) extends Monad[A] {
  override def flatMap[B](f: A => Monad[B]): Monad[B] = FlowableMonad(flowableA.flatMap(a => f(a) match {
    case FlowableMonad(flowableB) => flowableB
    case IterableMonad(iterableB) => fromIterable(iterableB)
  }))

  override def flowable(): Flowable[A] = flowableA
}

case class ParallelFlowableMonad[A](parFlowableA: ParallelFlowable[A]) extends Monad[A] {
  override def flatMap[B](f: A => Monad[B]): Monad[B] =
    ParallelFlowableMonad(
      parFlowableA.flatMap(a => {
        val monadB: Monad[B] = f(a)
        val flowB: Flowable[B] =
          monadB match {
            case FlowableMonad(flowableB) => flowableB
            case IterableMonad(iterableB) => fromIterable(iterableB)
            case IteratorMonad(iteratorB) => fromIterable(iteratorB.toIterable)
            case ParallelFlowableMonad(parFlowableB) => parFlowableB.sequential()
          }
        flowB
      }))

  override def parallelFlowable(): ParallelFlowable[A] = parFlowableA

  override def flowable(): Flowable[A] = parFlowableA.sequential()
}
