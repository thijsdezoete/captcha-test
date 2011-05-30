#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname mymochiapp_dev \
    -s mymochiapp \
    -s emysql \
    -s reloader
