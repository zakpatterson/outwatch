package outwatch.dom

object ProHandler {
  def create[I,O](f: I => O): ProHandler[I,O] =
    Handler.create[I].mapObservable[O](f)
}
