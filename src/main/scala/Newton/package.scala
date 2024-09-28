package object Newton {
  trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

  // Mostrar convierte una expresión en una cadena simbólica
  def mostrar(e: Expr): String = e match {
    case Numero(d) => d.toString
    case Atomo(x) => x.toString
    case Suma(e1, e2) => s"(${mostrar(e1)} + ${mostrar(e2)})"
    case Prod(e1, e2) => s"(${mostrar(e1)} * ${mostrar(e2)})"
    case Resta(e1, e2) => s"(${mostrar(e1)} - ${mostrar(e2)})"
    case Div(e1, e2) => s"(${mostrar(e1)} / ${mostrar(e2)})"
    case Expo(e1, e2) => s"(${mostrar(e1)} ^ ${mostrar(e2)})"
    case Logaritmo(e1) => s"log(${mostrar(e1)})"
  }

  // Derivar calcula la derivada de una expresión respecto a una variable
  def derivar(f: Expr, a: Atomo): Expr = f match {
    case Numero(_) => Numero(0)
    case Atomo(x) if x == a.x => Numero(1)
    case Atomo(_) => Numero(0)
    case Suma(e1, e2) => Suma(derivar(e1, a), derivar(e2, a))
    case Resta(e1, e2) => Resta(derivar(e1, a), derivar(e2, a))
    case Prod(e1, e2) => Suma(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a)))
    case Div(e1, e2) => Div(Resta(Prod(derivar(e1, a), e2), Prod(e1, derivar(e2, a))), Expo(e2, Numero(2)))
    case Expo(Atomo(x), Numero(n)) if x == a.x => Prod(Numero(n), Expo(Atomo(x), Numero(n - 1)))
    case Logaritmo(e1) => Div(derivar(e1, a), e1)
    case _ => throw new UnsupportedOperationException("Derivación no implementada para esta expresión")
  }

  // Evaluar evalúa una expresión en un valor dado para la variable
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {
    case Numero(d) => d
    case Atomo(x) if x == a.x => v
    case Suma(e1, e2) => evaluar(e1, a, v) + evaluar(e2, a, v)
    case Resta(e1, e2) => evaluar(e1, a, v) - evaluar(e2, a, v)
    case Prod(e1, e2) => evaluar(e1, a, v) * evaluar(e2, a, v)
    case Div(e1, e2) => evaluar(e1, a, v) / evaluar(e2, a, v)
    case Expo(e1, e2) => Math.pow(evaluar(e1, a, v), evaluar(e2, a, v))
    case Logaritmo(e1) => Math.log(evaluar(e1, a, v))
  }

  // Limpiar simplifica la expresión eliminando ceros y unos innecesarios
  def limpiar(f: Expr): Expr = f match {
    case Suma(Numero(0), e) => limpiar(e)
    case Suma(e, Numero(0)) => limpiar(e)
    case Prod(Numero(1), e) => limpiar(e)
    case Prod(e, Numero(1)) => limpiar(e)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Suma(e1, e2) => Suma(limpiar(e1), limpiar(e2))
    case Prod(e1, e2) => Prod(limpiar(e1), limpiar(e2))
    case Resta(e1, e2) => Resta(limpiar(e1), limpiar(e2))
    case Div(e1, e2) => Div(limpiar(e1), limpiar(e2))
    case Expo(e1, e2) => Expo(limpiar(e1), limpiar(e2))
    case Logaritmo(e1) => Logaritmo(limpiar(e1))
    case _ => f
  }

  // Raíz de Newton
  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: (Expr, Atomo, Double) => Boolean): Double = {
    def newtonRecursivo(x: Double): Double = {
      val fx = evaluar(f, a, x)
      val fdx = evaluar(derivar(f, a), a, x)
      val nuevoX = x - (fx / fdx)
      if (ba(f, a, nuevoX)) nuevoX
      else newtonRecursivo(nuevoX)
    }
    newtonRecursivo(x0)
  }
}
