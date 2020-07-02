#!/bin/sh

./rebar3 compile
erl \
-pa _build/default/lib/*/ebin \
-pa _build/test/lib/recordis/test/ \
-name "abc@127.0.0.1" \
-run load_all_module start \
-setcookie abc