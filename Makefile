default:
	opam update
	opam install . --deps-only
	dune build

build:
	dune build
	cd app && npm run build

install:
	opam update
	opam install . --deps-only
	cd app && npm install

run:
	cd app && npm run preview
