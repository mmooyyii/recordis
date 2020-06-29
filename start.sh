./rebar3 compile
erl -pa _build/default/lib/*/ebin -name "abc@127.0.0.1" -run recordis_app start -setcookie abc