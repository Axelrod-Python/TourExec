# Generate an executable with this name
TARGET = tourexec

# Define the compiler and flags to pass to that compiler
CC = gfortran
CFLAGS = -fno-automatic

# Define the directories for source code and output binary executable
SRC_DIR = src
BIN_DIR = bin

$(BIN_DIR)/$(TARGET): $(SRC_DIR)/*.f
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -o $@ $<
