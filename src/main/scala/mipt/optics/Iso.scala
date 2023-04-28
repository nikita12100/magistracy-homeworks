package mipt.optics

trait Iso[A, B]:
  def extract(a: A): B
  def back(b: B): A
  def update(f: B => B)(a: A): A = back(f(extract(a)))
  def compose[C](other: Iso[B, C]): Iso[A, C] = Iso(extract.andThen(other.extract), other.back.andThen(back))
                        // extract: B => C
                        // back: C => B
                        
                        // ToDo: extract: A => C = (extract: A => B).andThen(extract: B => C)
                        //       back: C => A = (back: C => B).andThen(back: B => A)

object Iso:
  def apply[A, B](using Iso[A, B]): Iso[A, B] = summon[Iso[A, B]]
  
  def apply[A, B](toFunc: A => B, fromFunc: B => A): Iso[A, B] = new Iso[A, B]:
    override def extract(a: A): B = toFunc(a)
    override def back(b: B): A = fromFunc(b)
    
  given [A, B](using Iso[A, B]): Iso[B, A] = Iso(Iso[A, B].back, Iso[A, B].extract)
    
  extension [A](a: A)
    def to[B](using Iso[A, B]): B = Iso[A, B].extract(a)
