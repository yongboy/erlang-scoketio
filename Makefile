PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

edoc:
	@$(REBAR) doc
	
clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

app:
	@$(REBAR) create template=demoapp dest=$(DEST) appid=$(PROJECT)