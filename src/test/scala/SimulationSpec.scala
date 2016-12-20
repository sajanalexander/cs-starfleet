package org.sajanalexander.csstarfleet

import org.scalatest._

class SimulationSpec extends FlatSpec with Matchers {
  "Simulation" should "evaluate the first simulation correctly" in {
    val evaluation = Simulation.performSimulation("src/test/resources/examples/1/field.txt",
																 									"src/test/resources/examples/1/script.txt")
	
    evaluation shouldBe (Pass(5))
  }
  it should "evaluate the second simulation correctly" in {
    val evaluation = Simulation.performSimulation("src/test/resources/examples/2/field.txt",
																 									"src/test/resources/examples/2/script.txt")
	
    evaluation shouldBe (Pass(8))
  }
  it should "evaluate the third simulation correctly" in {
    val evaluation = Simulation.performSimulation("src/test/resources/examples/3/field.txt",
																 									"src/test/resources/examples/3/script.txt")
	
    evaluation shouldBe (Pass(1))
  }
  it should "evaluate the fourth simulation correctly" in {
    val evaluation = Simulation.performSimulation("src/test/resources/examples/4/field.txt",
																 									"src/test/resources/examples/4/script.txt")
	
    evaluation shouldBe (Fail())
  }
  it should "evaluate the fifth simulation correctly" in {
    val evaluation = Simulation.performSimulation("src/test/resources/examples/5/field.txt",
																 									"src/test/resources/examples/5/script.txt")
	
    evaluation shouldBe (Fail())
  }
}

