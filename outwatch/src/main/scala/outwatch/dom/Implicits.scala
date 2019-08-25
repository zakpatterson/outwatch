package outwatch.dom

import com.raquo.domtypes.generic.keys
import monix.reactive.Observer
import outwatch.AsVDomModifier
import outwatch.dom.helpers.BasicStyleBuilder

trait Implicits {

  implicit def asVDomModifier[T](value: T)(implicit vm: AsVDomModifier[T]): VDomModifier = vm.asVDomModifier(value)

  //TODO: would be better to have typeclass AsObserver on all functions using observer.
  implicit def asObserver[T, F[_]](value: F[T])(implicit ao: AsObserver[F]): Observer[T] = ao.as(value)

  implicit def StyleIsBuilder[T](style: keys.Style[T]): BasicStyleBuilder[T] = new BasicStyleBuilder[T](style.cssName)
}
