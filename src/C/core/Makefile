# Final executable name
TARGET = jollyclick

# Directory where header files are located
INCLUDE_DIR = -I include -I /usr/lib/swipl/include/
LIBS_DIR= -L /usr/lib/swipl/lib/x86_64-linux/
# Source files
SRCS = main.c rule_loader.c collector.c refine.c kb_shared.c

# Object files generated from sources
OBJS = $(SRCS:.c=.o)

# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -g -O0 $(INCLUDE_DIR) -w
LDFLAGS = $(LIBS_DIR)  -lswipl -lpthread -lm -ldl
# Default target
.PHONY: all clean

all: $(TARGET)

# Link object files into the final executable
$(TARGET): $(OBJS)
	$(CC) $(OBJS) -o ../build/$@ $(LDFLAGS)

# Compile each .c file into a .o object file
%.o: %.c $(INCLUDE_DIR)/jollyclick.h
	$(CC) $(CFLAGS) -c $< -o $@

# Remove generated files
clean:
	rm -f *.o $(TARGET)
