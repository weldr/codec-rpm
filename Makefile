sandbox:
	[ -d .cabal-sandbox ] || cabal sandbox init && cabal update

hlint: sandbox
	[ -x .cabal-sandbox/bin/hlint ] && cabal exec hlint .

tests: sandbox
	cabal install --dependencies-only --enable-tests --force-reinstalls
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test --show-details=always

ci: hlint tests

ci_after_success:
	[ -x .cabal-sandbox/bin/hpc-coveralls ] && cabal exec -- hpc-coveralls --display-report tests
