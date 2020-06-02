// See LICENSE for license details.

package firrtl.transforms

import firrtl._

import firrtl.Utils.v_keywords
import firrtl.options.{Dependency, PreservesAll}
import firrtl.passes.Uniquify

/** Transform that removes collisions with reserved keywords
  * @param keywords a set of reserved words
  * @define implicitRename @param renames the [[RenameMap]] to query when renaming
  * @define implicitNamespace @param ns an encolosing [[Namespace]] with which new names must not conflict
  * @define implicitScope @param scope the enclosing scope of this name. If [[None]], then this is a [[Circuit]] name
  */
class RemoveKeywordCollisions(keywords: Set[String]) extends ManipulateNames {

  private val inlineDelim = "_"

  /** Generate a new name, by appending underscores, that will not conflict with the existing namespace
    * @param n a name
    * @param ns a [[Namespace]]
    * @return Some name if a rename occurred, None otherwise
    * @note prefix uniqueness is not respected
    */
  override def manipulate = (n: String, ns: Namespace) => keywords.contains(n) match {
    case true  => Some(Uniquify.findValidPrefix(n + inlineDelim, Seq(""), ns.cloneUnderlying ++ keywords))
    case false => None
  }

}

/** Transform that removes collisions with Verilog keywords */
class VerilogRename extends RemoveKeywordCollisions(v_keywords) with PreservesAll[Transform] {

  override def prerequisites = firrtl.stage.Forms.LowFormMinimumOptimized ++
    Seq( Dependency[BlackBoxSourceHelper],
         Dependency[FixAddingNegativeLiterals],
         Dependency[ReplaceTruncatingArithmetic],
         Dependency[InlineBitExtractionsTransform],
         Dependency[InlineCastsTransform],
         Dependency[LegalizeClocksTransform],
         Dependency[FlattenRegUpdate],
         Dependency(passes.VerilogModulusCleanup) )

  override def optionalPrerequisites = firrtl.stage.Forms.LowFormOptimized

  override def optionalPrerequisiteOf = Seq.empty

}
