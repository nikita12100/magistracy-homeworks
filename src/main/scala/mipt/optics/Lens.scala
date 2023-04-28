package mipt.optics

trait Lens[S, A]:
  def set(s: S, a: A): S
  def extract(s: S): A
  
  def update(s: S, f: A => A): S = set(s, f(extract(s)))
  
  def compose[B](other: Lens[A, B]): Lens[S, B] =
    Lens[S, B]((s, b) => set(s, other.set(extract(s), b)), extract.andThen(other.extract))
    // set: (S, A) => S     set: (A, B) => A
  def composeIsoS[T](iso: Iso[S, T]): Lens[T, A] =
    Lens[T, A]((t, a) => iso.extract(set(iso.back(t), a)), iso.back.andThen(extract))
  def composeIsoA[B](iso: Iso[A, B]): Lens[S, B] =
    Lens[S, B]((s, b) => set(s, iso.back(b)), extract.andThen(iso.extract))

object Lens:
  def apply[S, A](using Lens[S, A]): Lens[S, A] = summon[Lens[S, A]]

  def apply[S, A](setFunc: (S, A) => S, getFunc: S => A): Lens[S, A] = new Lens[S, A]:
    override def set(s: S, a: A): S = setFunc(s, a)
    override def extract(s: S): A = getFunc(s)
    
  def _1[A, B]: Lens[(A, B), A] = new Lens[(A, B), A]:
    override def set(s: (A, B), a: A): (A, B) = (a, s._2)
    override def extract(s: (A, B)): A = s._1

  def _2[A, B]: Lens[(A, B), B] = new Lens[(A, B), B]:
    override def set(s: (A, B), a: B): (A, B) = (s._1, a)
    override def extract(s: (A, B)): B = s._2
