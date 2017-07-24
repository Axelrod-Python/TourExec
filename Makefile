EXECUTABLE = tourexec
LIBRARY = libstrategies.so

COMPILER = gfortran
CFLAGS = -fno-automatic -fPIC

LINKER = gfortran

SRC_DIR = src
STRATEGY_SRC_DIR = $(SRC_DIR)/strategies
TOURNAMENT_SRC_DIR = $(SRC_DIR)/tournament

OBJ_DIR = obj
STRATEGY_OBJ_DIR = $(OBJ_DIR)/strategies
TOURNAMENT_OBJ_DIR = $(OBJ_DIR)/tournament

BIN_DIR = bin

# Use make's wildcard function to generate lists of source code files
STRATEGY_SOURCES := $(wildcard $(STRATEGY_SRC_DIR)/*.f)
TOURNAMENT_SOURCES := $(wildcard $(TOURNAMENT_SRC_DIR)/*.f)
# Use make's substitution references to define corresponding lists of object files
STRATEGY_OBJECTS := $(STRATEGY_SOURCES:$(STRATEGY_SRC_DIR)/%.f=$(STRATEGY_OBJ_DIR)/%.o)
TOURNAMENT_OBJECTS := $(TOURNAMENT_SOURCES:$(TOURNAMENT_SRC_DIR)/%.f=$(TOURNAMENT_OBJ_DIR)/%.o)

rm       = rm -rf

.PHONY: all clean remove
all: $(BIN_DIR)/$(EXECUTABLE) 

# Link the tournament object files and the strategies shared object file to create the executable
$(BIN_DIR)/$(EXECUTABLE): $(TOURNAMENT_OBJECTS) $(BIN_DIR)/$(LIBRARY)
	@$(LINKER) -L $(BIN_DIR) $(TOURNAMENT_OBJECTS) -l strategies -o $@
	@echo "Created executable file "$@

# Link the strategy object files to a create a single shared object file
$(BIN_DIR)/$(LIBRARY): $(STRATEGY_OBJECTS)
	@mkdir -p $(@D)
	@$(LINKER) -shared $(STRATEGY_OBJECTS) -o $@
	@echo "Built shared object file "$@

# Compile all strategy source files to object files
$(STRATEGY_OBJECTS): $(STRATEGY_OBJ_DIR)/%.o : $(STRATEGY_SRC_DIR)/%.f
	@mkdir -p $(@D)
	@$(COMPILER) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" to "$@

# Compile all tournament source files to object files
$(TOURNAMENT_OBJECTS): $(TOURNAMENT_OBJ_DIR)/%.o : $(TOURNAMENT_SRC_DIR)/%.f
	@mkdir -p $(@D)
	@$(COMPILER) $(CFLAGS) -c $< -o $@
	@echo "Compiled "$<" to "$@

clean:
	@$(rm) $(OBJ_DIR)
	@echo "Removed obj directory"

remove: clean
	@$(rm) $(BIN_DIR)
	@echo "Removed bin directory"