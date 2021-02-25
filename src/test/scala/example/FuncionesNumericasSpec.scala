package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuncionesNumericasSpec extends AnyFlatSpec with Matchers {
  "1 El area de un triangulo de lados 5 y 6 " should " 15 " in {
    FuncionesNumericas.areaTrianguloRectangulo(5.0,6.0) shouldEqual 15
  }
  "1.2 El area de un triangulo de lados 3 y 7 " should " 10.5 " in {
    FuncionesNumericas.areaTrianguloRectangulo(3.0,7.0) shouldEqual 10.5
  }
  "2.1 El area de un circulo de radio 1 " should " 3.14159265359 " in {
    FuncionesNumericas.areaDeUnCirculo(1) shouldBe 3.14159265359 +- 0.0001
  }
  "2.1 El area de un circulo de radio 4 " should " 50.2654824 " in {
    FuncionesNumericas.areaDeUnCirculo(4) shouldBe 50.2654824 +- 0.0001
  }
  "3.1 La funcion calSalario con 100 y 0 " should " 100.0 " in {
    FuncionesNumericas.calSalario(100.0,0.0) shouldBe 100.0 +- 0.0001
  }
  "3.2 La funcion calSalario con 1000 y 770 " should " 230.0 " in {
    FuncionesNumericas.calSalario(1000.0,770.0) shouldBe 230.0 +- 0.0001
  }
  "4.1 La funcion calSalarioBono con 100 y 0 " should " 110.0 " in {
    FuncionesNumericas.calSalarioBono(100.0,0.0) shouldBe 110.0 +- 0.0001
  }
  "4.2 La funcion calSalarioBono con 100 y 50 " should " 60.0 " in {
    FuncionesNumericas.calSalarioBono(100.0,50.0) shouldBe 60.0 +- 0.0001
  }
  "5.1 La funcion compSalario con la funcion calSalario 100 y 0 " should " 100.0 " in {
    FuncionesNumericas.compSalario(FuncionesNumericas.calSalario,100.0,0.0) shouldBe 100.0 +- 0.0001
  }
  "5.2 La funcion compSalario con la funcion calSalario 100 y 40 " should " 70.0 " in {
    FuncionesNumericas.compSalario(FuncionesNumericas.calSalarioBono,100.0,40.0) shouldBe 70.0 +- 0.0001
  }
  "6.1 La funcion genCalSalarioBono con un bono de 0.05 y 100,0" should " 100.0 " in {
    FuncionesNumericas.genCalSalarioBono(0.05)(100,0) shouldBe 105.0 +- 0.0001
  }
  "6.2 La funcion genCalSalarioBono con un bono de 0.5 y 100,30" should " 120.0 " in {
    FuncionesNumericas.genCalSalarioBono(0.5)(100,30) shouldBe 120.0 +- 0.0001
  }
  "7.1 La funcion calSalario5 con 100, 30 " should " 75.0 " in {
    FuncionesNumericas.calSalario5(100,30) shouldBe 75.0 +- 0.0001
  }
  "8.1 La funcion calSalario20 con 100, 30 " should " 90.0 " in {
    FuncionesNumericas.calSalario20(100,30) shouldBe 90.0 +- 0.0001
  }
  "9.1 La funcion calSalarioBonoClausura con 100, 30 cuando la variable bono esta definida" +
    "como bono = 1.05 " should " 75.0 " in {
    FuncionesNumericas.calSalarioBonoClausura(100,30) shouldBe 75.0 +- 0.0001
  }
  "11.1 La funcion calCalario15 con 100, 0 " should " 115.0 " in {
    FuncionesNumericas.calCalario15(100,0) shouldBe 115.0 +- 0.0001
  }
  "12.1 La funcion calCalario100 con 100, 100 " should " 200.0 " in {
    FuncionesNumericas.calCalario100(100,100) shouldBe 200.0 +- 0.0001
  }
  "15.1 El factorial de 3" should " 6 " in {
    FuncionesNumericas.factorial(3) shouldEqual 6
  }
  "15.2 El fibonacci  7" should " 5040 " in {
    FuncionesNumericas.factorial(7) shouldEqual 5040
  }
  "16.1 El fibonacci 2 " should " 1 " in {
    FuncionesNumericas.fibonacci(2) shouldEqual 1
  }
  "16.2 El fibonacci 2 " should " 13 " in {
    FuncionesNumericas.fibonacci(7) shouldEqual 13
  }
  "17.1 El factorial de 7" should " 5040 " in {
    FuncionesNumericas.tailFactorial(7) shouldEqual 5040
  }
  "17.2 El factorial de 5" should " 120 " in {
    FuncionesNumericas.tailFactorial(5) shouldEqual 120
  }
}
