.PHONY: all clean check test run

all:
	$(MAKE) -C $(OBJ) $(TARGET)

clean:
	$(MAKE) -C $(OBJ) clean

check:
	$(MAKE) -C $(OBJ) check

test:
	$(MAKE) -C $(OBJ) test

run:
	$(MAKE) -C $(OBJ) $(TARGET)
