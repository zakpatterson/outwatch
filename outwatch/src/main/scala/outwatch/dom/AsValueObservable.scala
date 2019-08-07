package outwatch.dom

import monix.reactive.{Observable, ObservableLike}
import monix.reactive.subjects.Var

trait AsValueObservable[-F[_]] {
  def as[T](stream: F[T]): ValueObservable[T]
}

trait AsValueObservableInstances0 {
  implicit def observable[F[_]](implicit F: ObservableLike[F]) = new AsValueObservable[F] {
    def as[T](stream: F[T]): ValueObservable[T] = new ValueObservable[T] {
      def observable: Observable[T] = F(stream)
      def value: Option[T] = None
    }
  }
}

object AsValueObservable extends AsValueObservableInstances0  {
  implicit object valueObservable extends AsValueObservable[ValueObservable] {
    @inline def as[T](stream: ValueObservable[T]): ValueObservable[T] = stream
  }

  implicit object variable extends AsValueObservable[Var] {
    def as[T](stream: Var[T]): ValueObservable[T] = new ValueObservable[T] {
      def observable: Observable[T] = stream.drop(1)
      def value: Option[T] = Some(stream.apply())
    }
  }
}

