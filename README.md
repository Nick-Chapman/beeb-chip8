# beeb-chip8

Implementation of Chip8 for the BBC Micro.

Try various roms [online with jsbeeb](https://nick-chapman.github.io/beeb-chip8)
including
[maze](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/maze.ssd&autoboot),
[pong2](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/pong2.ssd&autoboot),
[brix](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/brix.ssd&autoboot),
[invaders](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/invaders.ssd&autoboot),
[tetris](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/tetris.ssd&autoboot).

Also see my Chip8 [Brainfuck interpreter](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/bf-fibs.ssd&autoboot) computing the Fibonacci Sequence.

# self.ch8

[self.ch8](https://github.com/Nick-Chapman/chip8/tree/master/roms/self.ch8), is a chip8 interpreter, written in chip8.
Here is its [disassembly](https://github.com/Nick-Chapman/chip8/tree/master/dis/self.dis).
Here is the [Haskell DSL](https://github.com/Nick-Chapman/chip8/tree/master/app/Self.hs) from which is was constructed.
To run the interpreter on an object program, simply concatenate the roms:
```
cat self.ch8 PONG2.ch8 > self-PONG2.ch88
```
To prove the interpreter is really an interpreter (and not simply a trivial relocate and execute cheat)
it offers a pause function on `z`.  When paused the program counter and current opcode are displayed.

Here are some examples in jsbeeb:
[self-maze](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/self-maze.ssd&autoboot),
[self-pong2](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/self-pong2.ssd&autoboot),
[self-brix](https://bbc.godbolt.org/?&disc1=https://nick-chapman.github.io/beeb-chip8/self-brix.ssd&autoboot).


Chip8 keypad mapping
```
     press             chip8
    -------           -------
    1 2 3 4      =>   1 2 3 C
     q w e r     =>   4 5 6 D
      a s d f    =>   7 8 9 E
       z x c v   =>   A 0 B F
```

Some resources:
- [Cheat sheet for CHIP-8 instructions](https://johnearnest.github.io/Octo/docs/chip8ref.pdf)
- [Tobias's emulator guide](https://tobiasvl.github.io/blog/write-a-chip-8-emulator)
- [Timendus's test suite](https://github.com/Timendus/chip8-test-suite)
- [Chip-8 on the COSMAC VIP](https://www.laurencescotford.net/2020/07/25/chip-8-on-the-cosmac-vip-index/)
- [My Haskell emulator](https://github.com/Nick-Chapman/chip8)
