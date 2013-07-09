#!/bin/sh
erl -sname {{appid}} -pa ebin -pa deps/*/ebin -s {{appid}} -config app \
	-eval "io:format(\"Server start with port {{port}} Success!~n\")."