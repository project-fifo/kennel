REBAR = $(shell pwd)/rebar3
ELVIS = $(shell pwd)/elvis
APP=kennel
include fifo.mk

.PHONY: rel stagedevrel package all tree

all: compile

clean:
	$(REBAR) clean
	$(MAKE) -C rel/pkg clean

long-test:
	$(REBAR) as eqc,long eunit
