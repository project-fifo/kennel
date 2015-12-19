VERSION=0.7.2
COMPONENT_INTERNAL=kennel
COMPONENT=fifo-kennel
DEPS="erlang>=16.1.2" "coreutils>=8.22nb1" "sudo>=1.7.10p9"
DEPS="erlang" "coreutils" "sudo" "grep"

include ../../_build/prod/lib/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../../_build/prod/rel/$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)


package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz

