# TODO

This is a list of future tasks, in no particular order. If these are taken up by
anyone, they should become issues or PRs, and be removed from this list.

* `SelfTypingSchema` should state what schema is causing a typing cycle.
* `ValidationError`s that describe a parent schema should be more systematic
  about ordering (sometimes this is first, sometimes this is second).
* Ways to inspect `ValidatedJSON` should be provided (optics, perhaps?).
* `AdditionalPropButBanned` and `RequiredPropertyIsMissing` should carry a
  `Value` (that is, the JSON chunk that failed).
* `WrongType` appears unused. This is either a mistake (and thus, it should be
  used), or if it's no longer necessary, it should be removed wholesale.
* `ConflictingSpecRequirements` currently gets thrown on two arguably quite
  different conditions. This should be split into two different data
  constructors, indicating each of them separately.
* `Natural` should be a newtype, and should probably be named something a bit
  less confusing vis a vis `base` (`MedeaNatural` would be appropriate).
* `tupleSpec` member should be a `Vector`, not a list.
* `Object.Specification` should be a sum type; if additional properties aren't
  allowed, we'll _never_ have a type specification for it, and therefore, we
  should avoid having invalid states be representable.
* The current SPEC mashes syntax and semantics together, and needs to be
  redrafted, perhaps separating them into two specifications (one for each).
* There are elements of a formal model for Medea, but it's not clearly
  elucidated or specified - this should happen.
* Figure out why our CI settings break on the following combinations:
  * Cabal latest, Windows latest, GHC 8.8.3
  * Stack latest, macOS latest, GHC 8.6.5
* Checking custom schemata currently relies on an unsafe construction. This
  should be replaced by a safer one.
