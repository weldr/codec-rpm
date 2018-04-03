sandbox:
	[ -d .cabal-sandbox ] || cabal sandbox init && cabal update

hlint: sandbox
	~/.cabal/bin/hlint .

tests: sandbox
	cabal install --dependencies-only --enable-tests --force-reinstalls
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test --show-details=always

ci: tests hlint

ci_after_success:
	~/.cabal/bin/hpc-coveralls --display-report tests
