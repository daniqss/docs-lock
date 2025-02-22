ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))

$(eval $(ARGS):;@:)

.PHONY: install run

install:
	bash ./install.sh

run:
	bash ./run.sh $(ARGS)