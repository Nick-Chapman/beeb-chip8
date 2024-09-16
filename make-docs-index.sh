#!/bin/bash

(
echo '## Run chip8 in JsBeeb'
prefix='https://bbc.godbolt.org/?\&disc1=https://nick-chapman.github.io/beeb-chip8'
ls docs/*.ssd | grep -v Meteors.ssd | sed 's|docs/\(.*\).ssd|- [\1]('$prefix'/\1.ssd\&autoboot)|'
) > docs/README.md
