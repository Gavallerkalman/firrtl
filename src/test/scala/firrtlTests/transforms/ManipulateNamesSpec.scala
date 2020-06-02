// See LICENSE for license details.

package firrtlTests.transforms

import firrtl.{ir, CircuitState, FirrtlUserException, Namespace, Parser}
import firrtl.annotations.CircuitTarget
import firrtl.options.Dependency
import firrtl.testutils.FirrtlCheckers._
import firrtl.transforms.{
  ManipulateNames,
  ManipulateNamesBlacklistAnnotation,
  ManipulateNamesWhitelistAnnotation
}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ManipulateNamesSpec {

  class AddPrefix extends ManipulateNames {
    override def manipulate = (a: String, b: Namespace) => Some(b.newName("prefix_" + a))
  }

}

class ManipulateNamesSpec extends AnyFlatSpec with Matchers {

  import ManipulateNamesSpec._

  class CircuitFixture {
    protected val input =
      """|circuit Foo:
         |  module Bar:
         |    node a = UInt<1>(0)
         |  module Foo:
         |    inst bar of Bar
         |    inst bar2 of Bar
         |""".stripMargin
    val `~Foo` = CircuitTarget("Foo")
    val `~Foo|Foo` = `~Foo`.module("Foo")
    val `~Foo|Foo/bar:Bar` = `~Foo|Foo`.instOf("bar", "Bar")
    val `~Foo|Foo/bar2:Bar` = `~Foo|Foo`.instOf("bar2", "Bar")
    val `~Foo|Bar` = `~Foo`.module("Bar")
    val `~Foo|Bar>a` = `~Foo|Bar`.ref("a")
    val tm = new firrtl.stage.transforms.Compiler(Seq(Dependency[AddPrefix]))
  }

  behavior of "ManipulateNames"

  it should "rename everything by default" in new CircuitFixture {
    val state = CircuitState(Parser.parse(input), Seq.empty)
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "prefix_Foo") => true },
      { case ir.Module(_, "prefix_Foo", _, _) => true},
      { case ir.Module(_, "prefix_Bar", _, _) => true}
    )
    expected.foreach(statex should containTree (_))
  }

  it should "do nothing if the circuit is blacklisted" in new CircuitFixture {
    val annotations = Seq(ManipulateNamesBlacklistAnnotation(Seq(Seq(`~Foo`)), Dependency[AddPrefix]))
    val state = CircuitState(Parser.parse(input), annotations)
    val statex = tm.execute(state)
    state.circuit.serialize should be (statex.circuit.serialize)
  }

  it should "not rename the circuit if the top module is blacklisted" in new CircuitFixture {
    val annotations = Seq(ManipulateNamesBlacklistAnnotation(Seq(Seq(`~Foo|Foo`)), Dependency[AddPrefix]))
    val state = CircuitState(Parser.parse(input), annotations)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "Foo") => true },
      { case ir.Module(_, "Foo", _, _) => true},
      { case ir.Module(_, "prefix_Bar", _, _) => true}
    )
    val statex = tm.execute(state)
    expected.foreach(statex should containTree (_))
  }

  it should "not rename instances if blacklisted" in new CircuitFixture {
    val annotations = Seq(ManipulateNamesBlacklistAnnotation(Seq(Seq(`~Foo|Foo/bar:Bar`)), Dependency[AddPrefix]))
    val state = CircuitState(Parser.parse(input), annotations)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.DefInstance(_, "bar", "prefix_Bar", _) => true},
      { case ir.Module(_, "prefix_Bar", _, _) => true}
    )
    val statex = tm.execute(state)
    expected.foreach(statex should containTree (_))
  }

  it  should "do nothing if the circuit is not whitelisted" in new CircuitFixture {
    val annotations = Seq(
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo|Foo`)), Dependency[AddPrefix])
    )
    val state = CircuitState(Parser.parse(input), annotations)
    val statex = tm.execute(state)
    state.circuit.serialize should be (statex.circuit.serialize)
  }

  it should "rename only the circuit if whitelisted" in new CircuitFixture {
    val annotations = Seq(
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo`)), Dependency[AddPrefix]),
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo|Foo`)), Dependency[AddPrefix])
    )
    val state = CircuitState(Parser.parse(input), annotations)
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "prefix_Foo") => true },
      { case ir.Module(_, "prefix_Foo", _, _) => true},
      { case ir.DefInstance(_, "bar", "Bar", _) => true},
      { case ir.DefInstance(_, "bar2", "Bar", _) => true},
      { case ir.Module(_, "Bar", _, _) => true},
      { case ir.DefNode(_, "a", _) => true}
    )
    expected.foreach(statex should containTree (_))
  }

  it should "rename an instance via whitelisting" in new CircuitFixture {
    val annotations = Seq(
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo`)), Dependency[AddPrefix]),
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo|Foo/bar:Bar`)), Dependency[AddPrefix])
    )
    val state = CircuitState(Parser.parse(input), annotations)
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "Foo") => true },
      { case ir.Module(_, "Foo", _, _) => true},
      { case ir.DefInstance(_, "prefix_bar", "Bar", _) => true},
      { case ir.DefInstance(_, "bar2", "Bar", _) => true},
      { case ir.Module(_, "Bar", _, _) => true},
      { case ir.DefNode(_, "a", _) => true}
    )
    expected.foreach(statex should containTree (_))
  }

  it should "rename a node via whitelisting" in new CircuitFixture {
    val annotations = Seq(
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo`)), Dependency[AddPrefix]),
      ManipulateNamesWhitelistAnnotation(Seq(Seq(`~Foo|Bar>a`)), Dependency[AddPrefix])
    )
    val state = CircuitState(Parser.parse(input), annotations)
    val statex = tm.execute(state)
    val expected: Seq[PartialFunction[Any, Boolean]] = Seq(
      { case ir.Circuit(_, _, "Foo") => true },
      { case ir.Module(_, "Foo", _, _) => true},
      { case ir.DefInstance(_, "bar", "Bar", _) => true},
      { case ir.DefInstance(_, "bar2", "Bar", _) => true},
      { case ir.Module(_, "Bar", _, _) => true},
      { case ir.DefNode(_, "prefix_a", _) => true}
    )
    expected.foreach(statex should containTree (_))
  }

  it should "throw user errors on circuits that haven't been run through LowerTypes" in {
    val input =
      """|circuit Foo:
         |  module Foo:
         |    wire bar: {a: UInt<1>, b: UInt<1>}
         |    node baz = bar.a
         |""".stripMargin
    val state = CircuitState(Parser.parse(input), Seq.empty)
    intercept [FirrtlUserException] {
      (new AddPrefix).transform(state)
    }.getMessage should include ("LowerTypes")
  }

  behavior of "ManipulateNamesBlacklistAnnotation"

  it should "throw an exception if a non-local target is skipped" in new CircuitFixture {
    val barA = CircuitTarget("Foo").module("Foo").instOf("bar", "Bar").ref("a")
    assertThrows[java.lang.IllegalArgumentException]{
      Seq(ManipulateNamesBlacklistAnnotation(Seq(Seq(barA)), Dependency[AddPrefix]))
    }
  }

}
