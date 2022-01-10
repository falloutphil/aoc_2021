#!/usr/bin/env sh
# -*- wisp -*-
# precompile the wisp spec
guile -L "$(dirname "$0")" \
      -c '(import (language wisp spec))'
# run your file via wisp
exec guile -L "$(dirname "$0")" \
           -x '.w' --language wisp \
           -s "$0" "$@"
# the sh indirection header ends here !#

define hpos 0
define vpos 0

define : forward n
    set! hpos : + hpos n

define : up n
    set! vpos : - vpos n

define : down n
    set! vpos : + vpos n

load "input.txt"
format #t "~%Part One Result: ~a~%" : * hpos vpos

