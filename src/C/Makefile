CC = gcc
#CFLAGS = -Wall -Wextra -g -O0 -I /usr/local/lib/swipl/include/ -w
CFLAGS = -Wall -Wextra -g -O0 -I /usr/lib/swi-prolog/include/ -w
#LDFLAGS = -L/usr/local/lib/swipl/lib/x86_64-linux/ -lswipl -lpthread
LDFLAGS =  -lswipl -lpthread

TARGET = build/jollyclick
SRC = core/jollyclick.c

all: $(TARGET)

$(TARGET): $(SRC)
	mkdir -p build
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(TARGET)

