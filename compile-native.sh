#!/bin/bash
./compile.sh
(cd haxebin && gcc -I . -lhl -I /Users/janis.kirsteins/Projects/hashlink/src -o apphx_native app.c)
(cd haxebin && ./apphx_native)
