.PHONY: rel package version all tree

all: version compile

include fifo.mk

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > kennel.version || true

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat kennel.version)\">>)." > apps/howl/include/howl_version.hrl

clean:
	$(REBAR) clean
	make -C rel/pkg clean

long-test:
	$(REBAR) as eqc,long eunit

rel: update
	$(REBAR) as prod compile
	$(REBAR) as prod release

package: rel
	make -C rel/pkg package

typer:
	typer --plt ./_build/default/rebar3_*_plt _build/default/lib/*/ebin
