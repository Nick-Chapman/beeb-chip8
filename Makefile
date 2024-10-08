
top: all
all: build-all

default = self-pong2

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
