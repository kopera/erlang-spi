CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

ifeq ($(ERTS_INCLUDE_DIR),)
	ERTS_INCLUDE_DIR := $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
endif

UNAME_SYS := $(shell uname -s)

# System specific C compiler/flags.
ifeq ($(UNAME_SYS), Darwin)
	CC ?= cc
	CFLAGS ?= -O2 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS ?= -O2 -finline-functions -Wall -Wmissing-prototypes
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS ?= -O2 -finline-functions -Wall -Wmissing-prototypes
endif

CFLAGS += -std=c99 -fPIC -I $(ERTS_INCLUDE_DIR)

LDLIBS +=
LDFLAGS += -shared


# Project specific C compiler/flags.
# ifeq ($(UNAME_SYS), Darwin)
# 	LDFLAGS +=
# else ifeq ($(UNAME_SYS), Linux)
# 	LDFLAGS +=
# endif

# Verbosity.
V ?= 0

c_verbose_0 = @echo " C     " $(@F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

compile_c = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c

SOURCES := \
	spidev_nif.c

HEADERS := $(wildcard *.h)
OBJECTS = $(addprefix _build/, $(addsuffix .o, $(basename $(SOURCES))))

all: $(BASEDIR)/priv/spidev_nif.so

$(BASEDIR)/priv/spidev_nif.so: $(OBJECTS) Makefile | $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) $(LDLIBS) -o "$@"

$(BASEDIR)/priv/:
	@mkdir -p "$@"

_build/%.o: %.c $(HEADERS) Makefile | _build/
	$(compile_c) "$<" -o "$@"

_build/:
	@mkdir -p "$@"

clean:
	@rm -f "$(BASEDIR)/priv/spidev_nif.so"
	@rm -Rf "_build/"
