# Final executable name
TARGET = clickceler

# Header and library directories
# -I flags tell the compiler where to look for header files
# -L flags tell the linker where to look for library files
INCLUDE_DIR = -I./include -I/usr/lib/swipl/include
LIBS_DIR = -L/usr/lib/swipl/lib/x86_64-linux

# Primary header files that all .c files might depend on.
# List all necessary headers here to ensure correct dependency tracking.
HEADERS = include/clickceller.h include/global.h

# Source files
SRCS = main.c rule_loader.c collector.c thread_handler.c 

# Object files generated from sources
OBJS = $(SRCS:.c=.o)

# Compiler and flags
CC = gcc -w
# -Wall, -Wextra: Enable a high level of warnings for good practice
# -g: Include debugging information
# -O0: Disable optimization (good for debugging)
# $(INCLUDE_DIR): Provides the paths for header files
CFLAGS = -Wall -Wextra -g -O0 $(INCLUDE_DIR)
LDFLAGS = $(LIBS_DIR) -lswipl -lpthread -lm -ldl

# Default target
.PHONY: all clean

all: $(TARGET)

# Link object files into the final executable
$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o ../build/$@ $(LDFLAGS)

# Compile each .c file into a .o object file.
# This rule now explicitly depends on the HEADERS variable.
# If a header file is modified, make will recompile all dependent .o files.
%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

# Remove generated files
clean:
	rm -f $(OBJS) ../build/$(TARGET)

