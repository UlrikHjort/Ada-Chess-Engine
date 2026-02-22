# Makefile for Ada Chess Engine

# Directories
SRC_DIR = src
BIN_DIR = bin
OBJ_DIR = obj

# Compiler
GNATMAKE = gnatmake
GNATCLEAN = gnatclean

# Flags
GNATFLAGS = -gnata -gnatwa -gnatVa -gnat2012 -I$(SRC_DIR) -D $(OBJ_DIR)

# Main programs
MAIN     = chess
MAIN_UCI = chess_uci

# Target executables
TARGET     = $(BIN_DIR)/$(MAIN)
TARGET_UCI = $(BIN_DIR)/$(MAIN_UCI)

.PHONY: all uci clean run run-uci

all: $(TARGET)

uci: $(TARGET_UCI)

$(TARGET): $(SRC_DIR)/*.ad[sb]
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	$(GNATMAKE) $(GNATFLAGS) $(SRC_DIR)/main.adb -o $(TARGET)

$(TARGET_UCI): $(SRC_DIR)/*.ad[sb]
	@mkdir -p $(BIN_DIR) $(OBJ_DIR)
	$(GNATMAKE) $(GNATFLAGS) $(SRC_DIR)/main_uci.adb -o $(TARGET_UCI)

clean:
	$(GNATCLEAN) -r -c -D $(OBJ_DIR)
	rm -f $(TARGET) $(TARGET_UCI)
	rm -rf $(OBJ_DIR)/*

run: $(TARGET)
	$(TARGET)

run-uci: $(TARGET_UCI)
	$(TARGET_UCI)
