/*
  Taller 1 de Scala
  Descripcion: En este taller se pone en practica los temas vistos de escala mediante la resolucion
               de diferentes ejercicios.
  Autor: J. Alexander Velasquez E.
 */
package example

import scala.math.Pi

object FuncionesNumericas {
  /*
    En la siguiente seccion se trabaja el tema de Funciones literales/Funciones anónimas
  */
  /*
  Ejercicio 1. Defina una función literal llamada areaTrianguloRectangulo
               que se encargue de calcular el área de un triángulo rectángulo. Recibe los dos
               lados rectos.
  */
  val areaTrianguloRectangulo = (lado1:Double, lado2:Double)  => { (lado1 * lado2) / 2 }

  /*
  Ejercicio 2. Defina una función literal con un tipo basado funciones valoresa
               llamada areaDeUnCirculo.
   */
  val areaDeUnCirculo = new Function1[Double,Double] {
                              def apply(radio:Double) = { radio * radio * Pi }
                            }

  /*
  En la siguiente seccion se trabaja el tema de Funciones de primera clase y funciones de alto orden
   */
  /*
  Ejercicio 3. Defina una función literal llamada calSalario que reciba dos
               parámetros de tipo Double (devengado y deducciones) y que retorna el valor
               clásico del salario de una persona:
                             devengado − deducciones
   */
  val calSalario = (devengado:Double, deducciones:Double) => devengado - deducciones
  /*
  Ejercicio 4. Defina una función literal llamada calSalarioBono que reciba
               dos parámetros de tipo Double (devengado y deducciones) y calcule el salario
               con el siguiente valor.
                             devengado ∗ 1,10 − deducciones
   */
  val calSalarioBono = (devengado:Double, deducciones:Double) => {devengado * 1.10 - deducciones}
  /*
  Ejercicio 5. Defina una función llamada compSalario que recibe tres parámetros:
               el primero una función de tipo (Double,Double)=>Double y las otras dos
               son: devengado y deducciones. Prueba esta función pasado las dos funciones
               anteriores calSalario y calSalarioBono.
   */
  def compSalario(f:(Double,Double) => Double, devengado:Double, deducciones:Double) = f(devengado, deducciones)
  //Se comprueba los resultados como solicita el enunciado del ejercicio, para eso se tienen los siguientes print
  //de igual manera en FuncionesNumericasSpec.scala se encuentran las pruebas unitarias de todas las funciones
//  println("Salario 300, 40 : " + calSalario(300,40) + "  SalarioBono 300, 40: " + calSalarioBono(300,40) )
//  println("Mediante compSalario\nSalario 300, 40 : " + compSalario(calSalario,300,40) + "  SalarioBono 300, 40: " + compSalario(calSalarioBono,300,40) )

  /*
  Ejercicio 6. Defina una función llamada genCalSalarioBono esta función se
               encarga de generar funciones que computan diferente bonos. La función
               def genCalSalarioBono( bono : Double ) : ( Double , Double ) => Double = ???

   */
  //Esta funcion recibe el bono como tipo de dato Double donde 0.05=5%  0.2=20%
  def genCalSalarioBono( bono:Double ):(Double,Double) => Double = bono match {
    case 0.05 => (devengado:Double,deducciones:Double) => { devengado*1.05 - deducciones }
    case 0.2 => (devengado:Double,deducciones:Double) => { devengado*1.2 - deducciones }
    case _ => (devengado:Double,deducciones:Double) => { devengado*(1 + bono ) - deducciones }
  }
  /*
  Ejercicio 7. Utilizando la función generadora de funciones genCalSalarioBono
               cree la función literal calSalario5 que da un bono del 5 %.
   */
  //De esta manera tenemos calSalario5 que recibe dos parametros y calcula el salario con un bono del 5%
  val calSalario5 = genCalSalarioBono(0.05)
  /*
  Ejercicio 8. Utilizando la función generadora de funciones genCalSalarioBono
               cree la función literal calCalario20 que da un bono del 20 %.
   */
  val calSalario20 = genCalSalarioBono(0.2)
  /*
  En la siguiente seccion se trabaja el tema de Clausuras
   */
  /*
  Ejercicio 9. Declare una función calSalarioBonoClausura que reciba dos
               parámetros (devengados y deducciones) y que aplique la siguiente fórmula:
                          devengado × bono − deducciones
               Donde bono es una valor declarado externamente (Una clausura).
   */
  val bono = 1.05
  def calSalarioBonoClausura(devengado:Double,deducciones:Double) = { devengado * bono - deducciones}

  /*
  Ejercicio 10. Utilizando la función compSalario aplique la función utilizando
                como primer parámetro calSalarioBonoClausura y calculado varios salarios
                diferentes.
   */
//  println("compSalarioClausura 300, 40 : " + compSalario(calSalarioBonoClausura,300,40)  )
//  println("compSalarioClausura 300, 40 : " + compSalario(calSalarioBonoClausura,300,70)  )

  /*
  En la siguiente seccion se trabaja el tema de Funciones aplicadas parcialmente
   */
  /*
  Ejercicio 11. Utilizando la función genCalSalarioBono cree una función literal calCalario15
                que se obtiene a través de la aplicación parcial de genCalSalarioBono con un valor 0,15
   */
  val calCalario15 = genCalSalarioBono(0.15)
  /*
  Ejercicio 12. Utilizando la función genCalSalarioBono cree una función literal calCalario100
                que se obtiene a través de la aplicación parcial de genCalSalarioBono con un valor 2,00.
   */
  val calCalario100 = genCalSalarioBono(2.00)

  /*
  En la siguiente seccion se trabaja el tema de Funciones currificadas
   */
  /*
  Ejercicio 13. Utilizando currificación defina una función genCalSalarioBono2
                donde el último parámetro (el currificado) es el valor del bono y los dos primeros
                parámetros son: el devengado y la deducción.
   */
  def genCalSalarioBono2(devengado:Double,deduccion:Double):Double => Double =
    (bono:Double) => { devengado*(1 + bono ) - deduccion }

  /*
    En la siguiente seccion se trabaja el tema de Recursividad
   */

  /*
  Ejercicio 15. Implementar la función de factorial utilizando recursividad en Scala.
   */
  def factorial(n:Int):Int = {
    if (n==0) 1
    else n * factorial(n-1)
  }

  /*
  Ejercicio 16. Implementar la siguiente función de forma recursiva en Scala
  */
  def fibonacci(n:Int):Int = {
    if (n==0) 0
    else if (n==1) 1
    else fibonacci(n-1) + fibonacci(n-2)
  }

  /*
  Ejercicio 17. Reescriba la función de factorial para que se ejecute bajo recursividad de cola.
   */
  def tailFactorial(n:Int):Int = {
    @scala.annotation.tailrec
    def _tailFactorial(num:Int,result:Int):Int = {
      if (num==0) result
      else _tailFactorial(num-1,num*result)
    }
    _tailFactorial(n,1)
  }

}
