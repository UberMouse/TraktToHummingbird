package nz.ubermouse.hummingbirdsyncer

// https://gist.github.com/akiellor/1308190
object Tap {
  class Tap[A](any: A) {
    def tap(f: (A) => Unit): A = {
      f(any)
      any
    }
  }

  implicit def tap[A](toTap: A): Tap[A] = new Tap(toTap)
}
