#!/bin/bash
cd "$(dirname "$0")/.."
BEAM_PATH=`pwd`/ebin/

echo $BEAM_PATH

erl -pa $BEAM_PATH -noshell -s simple_input