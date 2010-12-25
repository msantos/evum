#!/bin/sh

erl -boot start_sasl -pa $PWD/ebin $PWD/deps/*/ebin
pkill -TERM linux > /dev/null 2>&1
rm priv/uml/evum.ctl > /dev/null 2>&1
