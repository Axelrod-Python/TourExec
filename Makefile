TARGET = tourexec

CC = gfortran
CFLAGS = -fno-automatic

LINKER = gfortran

SRC_DIR = src
BIN_DIR = bin
OBJ_DIR = obj

SOURCES  := $(wildcard $(SRC_DIR)/*.f)
OBJECTS  := $(SOURCES:$(SRC_DIR)/%.f=$(OBJ_DIR)/%.o)
rm       = rm -rf

$(BIN_DIR)/$(TARGET): $(OBJECTS)
	@mkdir -p $(@D)
	@$(LINKER) $(OBJECTS) -o $@
	@echo "Linking complete"

$(OBJECTS): $(OBJ_DIR)/%.o : $(SRC_DIR)/%.f
	@mkdir -p $(@D)
	@$(CC) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" successfully"

.PHONY: clean
clean:
	@$(rm) $(OBJ_DIR)
	@echo "Cleanup complete"

.PHONY: remove
remove: clean
	@$(rm) $(BIN_DIR)
	@echo "Executable removed"