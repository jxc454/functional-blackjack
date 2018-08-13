package com.blackjack

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State(initialState => {
      val (a, newState) = this.run(initialState)
      g(a).run(newState)
    })
  }

  def map[B](f: A => B): State[S, B] = {
    flatMap((a: A) => State.unit(f(a)))
  }

  def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap((a: A) => s2.map((b: B) => f(a, b)))
  }

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: Seq[State[S, A]]): State[S, Seq[A]] = {
    l.foldRight(unit[S, Seq[A]](Nil: Seq[A]))((f, acc) => {
      State(s => {
        val (a, s2) = f.run(s)
        val (list2, s3) = acc.run(s2)
        (a +: list2, s3)
      })
    })

    l.foldRight(unit[S, List[A]](Nil: List[A]))((f, acc) => {
      f.map2(acc)(_ :: _)
    })
  }

}
