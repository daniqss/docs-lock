###-----------------------------------------------------------------------------
### TARGETS
###-----------------------------------------------------------------------------
.PHONY: all compile clean check

all: compile

compile:
	rebar3 compile

clean:
	rebar3 clean

check:
	rebar3 check

shell:
	rebar3 shell

###-----------------------------------------------------------------------------
### TEST TARGETS
###-----------------------------------------------------------------------------
.PHONY: test

test:
	@$(MAKE) mongo_up
	rebar3 test
	@$(MAKE) mongo_down

###-----------------------------------------------------------------------------
### LOCAL DEPLOYMENT TARGETS
###-----------------------------------------------------------------------------
.PHONY: mongo_up mongo_down

mongo_up:
	docker-compose up -d 

mongo_down:
	docker-compose stop mongo
