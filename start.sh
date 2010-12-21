#!/bin/sh

exec erl -boot start_sasl -pa $PWD/ebin $PWD/deps/*/ebin
