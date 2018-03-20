## 0.2.1

* Allow newer versions of conduit, conduit-extra, lzma-conduit, and resourcet.
* Relax minimum requirements on exceptions, HUnit, and text to fit better
  with what is shipped in Fedora.
* Remove the unused requirement on conduit-combinators.
* payloadContentsC now wants an instance of MonadThrow.

## 0.2.0

* Move parseC into a new Conduit module.
* Add payloadC and payloadContentsC.
* parseRPMC should return a ParseError on error.

## 0.1.3

* Derive Ord for DepRequirement.

## 0.1.2

* Relax overly strict conduit-extra and text requirements.

## 0.1.1

* Relax overly strict attoparsec requirement.

## 0.1.0

* Initial release.
