all:
	dune build bin/discure.exe
	ls -s _build/default/bin/discure.exe Discure
	mkdir -p experiments/data/tmp
