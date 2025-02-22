.PHONY: all clean check test run

all:
	$(MAKE) -C $(OBJ) $(TARGET)
clean:
	$(MAKE) -C $(OBJ) clean

check:
	$(MAKE) -C $(OBJ) rebar3 check

test:
	$(MAKE) -C $(OBJ) test

run:
	$(MAKE) -C $(OBJ) $(TARGET)
