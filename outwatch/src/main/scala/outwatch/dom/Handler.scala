package outwatch
package dom

import monix.reactive.subjects.{BehaviorSubject, ReplaySubject}

object Handler {
  def empty[T]: Handler[T] = create[T]

  def create[T]: Handler[T] = unsafe[T]
  def create[T](seed:T): Handler[T] = unsafe[T](seed)

  def unsafe[T]: Handler[T] = ReplaySubject.createLimited(1)
  def unsafe[T](seed:T): Handler[T] = BehaviorSubject[T](seed)
}
