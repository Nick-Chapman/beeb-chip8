
#default = ibm
#default = maze
default = brix
#default = 3-corax+
#default = 6-keypad

top: run

run: run-$(default)
build: build-$(default)
has: has-$(default)
dis: dis-$(default)

run-%: _build _build/%.ssd
	b-em _build/$*.ssd

build-%: _build _build/%.ssd
	@ echo -n

.PRECIOUS:_build/%.ssd
_build/%.ssd: chip8.asm Makefile
	@ echo Building .ssd for chip8 rom: $*
	@ beebasm -S ROM=roms/$*.ch8 -w -i $< -do $@ -boot Code || rm $@

_build: ; @mkdir -p $@

# run using my haskell interpreter
has-%:
	cat roms/$*.ch8 | (cd ../../code/chip8; stack run /dev/stdin)

# dissasemble using my haskell interpreter
dis-%:
	cat roms/$*.ch8 | (cd ../../code/chip8; stack run /dev/stdin -- --dump)
