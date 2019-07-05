CC=clang
CFLAGS=-g -lm -ffast-math -I. -I./include `llvm-config --cflags`
CFLAGS=-g -lm -ffast-math -I. -I./include
OBJ_DIR=./obj

LD=clang
LDFLAGS=`llvm-config --cxxflags --ldflags --libs core executionengine mcjit interpreter analysis native bitwriter  --system-libs`
LDFLAGS=
SRC_DIR=./src
SRC_FILES=$(wildcard $(SRC_DIR)/*.c)
OBJ_FILES=$(patsubst $(SRC_DIR)/*.c,$(OBJ_DIR)/%.o,$(SRC_FILES))
BIN_DIR=bin

$(BIN_DIR)/build: $(OBJ_FILES)
	$(LD) $(CFLAGS) $(LDFLAGS) -o $@ $^

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(OBJ_DIR}/*.o $(BIN_DIR)/build
