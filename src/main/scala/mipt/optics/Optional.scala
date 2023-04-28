package mipt.optics

trait Optional[S, A]:
  def set(s: S, a: A): S
  def narrow(s: S): Either[S, A]

  def modify(s: S, f: A => A): S = narrow(s).fold(identity, a => set(s, f(a)))

  def compose[B](other: Optional[A, B]): Optional[S, B] =
    Optional(
      (s, b) => narrow(s).fold(identity, a => set(s, other.set(a, b))),
      // set : (S, A) => S    set : (A, B) => A
      s => narrow(s).flatMap(a => other.narrow(a).left.map(_ => s))
      //                           other.narrow: A => Either[A, B]
      //                                              Either[S, B]
    )
  def composeIsoS[T](iso: Iso[S, T]): Optional[T, A] = 
    Optional((t, a) => iso.extract(set(iso.back(t), a)), t => narrow(iso.back(t)).left.map(iso.extract))
  def composeIsoA[B](iso: Iso[A, B]): Optional[S, B] =
    Optional((s, b) => set(s, iso.back(b)), s => narrow(s).map(iso.extract))

object Optional:
  def apply[S, A](using Optional[S, A]): Optional[S, A] = summon[Optional[S, A]]

  def apply[S, A](setFunc: (S, A) => S, narrowFunc: S => Either[S, A]): Optional[S, A] = new Optional[S, A]:
    override def set(s: S, a: A): S = setFunc(s, a)
    override def narrow(s: S): Either[S, A] = narrowFunc(s)

  def fromLens[S, A](lens: Lens[S, A]): Optional[S, A] = new Optional[S, A]:
    override def set(s: S, a: A): S = lens.set(s, a)
    override def narrow(s: S): Either[S, A] = Right(lens.extract(s))

  def fromPrism[S, A](prism: Prism[S, A]): Optional[S, A] = new Optional[S, A]:
    override def set(s: S, a: A): S = prism.set(s, a)
    override def narrow(s: S): Either[S, A] = prism.narrow(s)
