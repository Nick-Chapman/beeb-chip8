
top: run
all: build-all

default = self-PONG2

run: run-$(default)
build: build-$(default)
has: has-$(default)
dis: dis-$(default)

roms = $(patsubst roms/%.ch8, %, $(wildcard roms/*.ch8))
ssds = $(patsubst %, _build/%.ssd, $(roms))

release: build-all
	@ echo Regenerating docs release directory
	@ rm -rf docs
	@ mkdir docs
	@ cp $(ssds) docs
	@ ./make-docs-index.sh

build-all: _build $(ssds)
	@ echo -n

run-%: _build _build/%.ssd
	b-em _build/$*.ssd

build-%: _build _build/%.ssd
	@ echo -n

.PRECIOUS:_build/%.ssd
_build/%.ssd: chip8.asm roms/%.ch8 roms/%.info Makefile
	@ echo Building .ssd for chip8 rom: $*
	@ beebasm -S INFO=roms/$*.info -S ROM=roms/$*.ch8 -w -i $< -do $@ -boot Code || rm $@

.PRECIOUS:roms/%.info
roms/%.info:
	echo $* > $@

_build: ; @mkdir -p $@

# run using my haskell interpreter
has-%:
	cat roms/$*.ch8 | (cd ../../code/chip8; stack run /dev/stdin)

# dissasemble using my haskell interpreter
dis-%:
	cat roms/$*.ch8 | (cd ../../code/chip8; stack run /dev/stdin -- --dump)

roms/scroll.ch8: ../chip8/app/*.hs message.text Makefile
	(cd ../chip8; stack run -- --assemble scroll)
	cat ../chip8/gen/scroll.ch8 message.text > roms/scroll.ch8

roms/scroll-what.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble scroll-what)
	cp ../chip8/gen/scroll-what.ch8 roms

roms/pi.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble pi)
	cp ../chip8/gen/pi.ch8 roms

roms/three.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble three)
	cp ../chip8/gen/three.ch8 roms

roms/evens.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble evens)
	cp ../chip8/gen/evens.ch8 roms

roms/bf-reverse.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble bf-reverse)
	cp ../chip8/gen/bf-reverse.ch8 roms

roms/bf-fibs.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble bf-fibs)
	cp ../chip8/gen/bf-fibs.ch8 roms

roms/bf-collatz.ch8: ../chip8/app/*.hs
	(cd ../chip8; stack run -- --assemble bf-collatz)
	cp ../chip8/gen/bf-collatz.ch8 roms

.PRECIOUS:roms/%.ch8
roms/%.ch8: ../chip8/gen/%.ch8
	cp $^ $@
