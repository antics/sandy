# sandy version
VERSION = 0.5

# Customize below to fit your system

# paths
PREFIX = /usr/local
MANPREFIX = ${PREFIX}/share/man

# includes and libs (ncurses)
INCS = -I. -I/usr/include
LIBS = -L/usr/lib -lc -lncurses

# flags
CPPFLAGS = -DVERSION=\"${VERSION}\" -D_BSD_SOURCE
#CFLAGS = -std=c99 -pedantic -Wall -Os ${INCS} ${CPPFLAGS}
#LDFLAGS = -s ${LIBS}
CFLAGS = -ggdb -std=c99 -pedantic -Wall -O0 ${INCS} ${CPPFLAGS}
LDFLAGS = ${LIBS}

# Solaris
#CFLAGS = -fast ${INCS} -DVERSION=\"${VERSION}\"
#LDFLAGS = ${LIBS}

# compiler and linker
CC = cc

