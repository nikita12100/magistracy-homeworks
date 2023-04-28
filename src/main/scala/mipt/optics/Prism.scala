package mipt.optics

trait Prism[S, A]:
  def upcast(a: A): S
  def narrow(s: S): Either[S, A]
  
  def modify(s: S, f: A => A): S = narrow(s).fold(identity, a => upcast(f(a)))
  def set(s: S, a: A): S = modify(s, _ => a)

  def compose[B](other: Prism[A, B]): Prism[S, B] = 
    Prism(other.upcast.andThen(upcast), s => narrow(s).flatMap(a => other.narrow(a).left.map(upcast)))
  def composeIsoS[T](iso: Iso[S, T]): Prism[T, A] =
    Prism[T, A](a => iso.extract(upcast(a)), t => narrow(iso.back(t)).left.map(iso.extract))
  def composeIsoA[B](iso: Iso[A, B]): Prism[S, B] =
    Prism[S, B](b => upcast(iso.back(b)), s => narrow(s).map(iso.extract))

object Prism:
  def apply[S, A](using Prism[S, A]): Prism[S, A] = summon[Prism[S, A]]

  def apply[S, A](upcastFunc: A => S, narrowFunc: S => Either[S, A]): Prism[S, A] = new Prism[S, A]:
    override def upcast(a: A): S = upcastFunc(a)
    override def narrow(s: S): Either[S, A] = narrowFunc(s)
