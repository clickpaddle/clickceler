CC = gcc
CFLAGS = -Wall -g -I/usr/lib/swi-prolog/include
LDFLAGS = -L/usr/lib/swi-prolog/lib -lswipl -lpthread

TARGET = build/jollyclick
SRC = core/jollyclick.c

all: $(TARGET)

$(TARGET): $(SRC)
	mkdir -p build
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f build/jollyclick

