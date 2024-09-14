
default = ibm
#default = 3-corax+

top: build

run: run-$(default)
build: build-$(default)

run-%: _build _build/%.ssd
	b-em _build/$*.ssd

build-%: _build _build/%.ssd
	@ echo -n

.PRECIOUS:_build/%.ssd
_build/%.ssd: chip8.asm Makefile
	@ echo Building .ssd for chip8 rom: $*
	@ beebasm -S ROM=roms/$*.ch8 -w -i $< -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@
