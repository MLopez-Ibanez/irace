#-----------------------------------------------------------------------

#  Makefile for mootools

VERSION = 1.0svn$(SVN_REV)

#-----------------------------------------------------------------------

#                        Copyright (c) 2007, 2008, 2009, 2010
#           Manuel Lopez-Ibanez  <manuel.lopez-ibanez@ulb.ac.be>

#  This program is free software (software libre); you can redistribute
#  it and/or modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2 of the
#  License, or (at your option) any later version.

#  This program is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.

#  You should have received a copy of the GNU General Public License
#  along with this program; if not, you can obtain a copy of the GNU
#  General Public License at:
#                  http://www.gnu.org/copyleft/gpl.html
#  or by writing to:
#            Free Software Foundation, Inc., 59 Temple Place,
#                  Suite 330, Boston, MA 02111-1307 USA
#
#-----------------------------------------------------------------------
# IMPORTANT NOTE: Please be aware that the fact that this program is
# released as Free Software does not excuse you from scientific
# propriety, which obligates you to give appropriate credit! If you
# write a scientific paper describing research that made substantive
# use of this program, it is your obligation as a scientist to
# acknowledge its use.  Moreover, as a personal note, I would
# appreciate it if you would email manuel.lopez-ibanez@ulb.ac.be with
# citations of papers referencing this work so I can mention them to
# my funding agent and tenure committee.
#-----------------------------------------------------------------------
DEBUG ?=1

BINDIR := ./

## Define source files
SRCS  = epsilon.c dominatedsets.c nondominated.c io.c ndsort.c hv_contrib.c hv.c
HEADERS = io.h common.h gcc_attribs.h epsilon.h nondominated.h hv_contrib.h hv.h
OBJS  = $(SRCS:.c=.o)
DIST_OTHER_FILES = Makefile README LICENSE svn_version Makefile.libhv

## Do we have svnversion?
ifeq ($(shell sh -c 'which svnversion 1> /dev/null 2>&1 && echo y'),y)
  ## Is this a working copy?
  ifeq ($(shell sh -c 'LC_ALL=C svnversion -n . | grep -q ^[0-9] && echo y'),y)
    $(shell sh -c 'svnversion -n . > svn_version')
  endif
endif
## Set version information:
SVN_REV = $(shell sh -c 'cat svn_version 2> /dev/null')

CC=gcc
# FIXME: Check Makefile of hv code!
#pentium# athlon-xp (see man gcc)
march=native
OPTIMISE := -O3

ifneq ($(DEBUG),0)
CDEBUG = -g3 -DMALLOC_CHECK_=3
OPTIMISE := -O0
endif

WARNFLAGS=-Wall -Wextra
override CFLAGS += $(OPTIMISE) -std=gnu99 $(WARNFLAGS) \
		 -D_GNU_SOURCE -D DEBUG=$(DEBUG) $(CDEBUG)
# Order is important !
LINKERFLAGS = -lm

DELETE = @rm -f
ECHO = @echo "$(1)"

guess-march:=$(strip $(shell $(CC) $(CFLAGS) -march=$(march) -x c -S -\#\#\# - < /dev/null 2>&1  | \
                grep -m 1 -o -e "march=[^'\"]*" | \
                sed 's,march=,,'))

ifndef march
  $(error please define an architecture, e.g., 'make march=native')
endif

override CFLAGS += -march=$(march) -DMARCH='"$(guess-march)"' -DVERSION=\"$(VERSION)\"

epsilon = $(BINDIR)/epsilon
dominatedsets = $(BINDIR)/dominatedsets
nondominated = $(BINDIR)/nondominated
ndsort = $(BINDIR)/ndsort

.PHONY : all clean distclean dist default 


default : $(epsilon) $(dominatedsets) $(nondominated) $(ndsort)

$(epsilon) : OBJS=epsilon.o io.o
$(epsilon) : epsilon.o io.o $(HEADERS)

$(dominatedsets) : OBJS=dominatedsets.o io.o
$(dominatedsets) : dominatedsets.o io.o $(HEADERS)

$(nondominated) : OBJS=nondominated.o io.o
$(nondominated) : nondominated.o io.o $(HEADERS)

include Makefile.libhv

$(ndsort) : OBJS=ndsort.o io.o hv_contrib.c $(HV_LIB)
$(ndsort) : $(OBJS) $(HEADERS) $(HV_LIB)

$(epsilon) $(nondominated) $(dominatedsets) $(ndsort):
	@mkdir -p $(BINDIR)
	$(call ECHO,--> Building $@ version $(VERSION) <---)
	$(CC) -o $@  $(OBJS) \
		 $(CFLAGS) \
		 $(LINKERFLAGS)

$(OBJS) : $(HEADERS)


clean :
	$(call ECHO,---> Removing $(epsilon) <---)
	$(DELETE) $(epsilon)

	$(call ECHO,---> Removing $(dominatedsets) <---)
	$(DELETE) $(dominatedsets)

	$(call ECHO,---> Removing $(nondominated) <---)
	$(DELETE) $(nondominated)

	$(call ECHO,---> Removing $(ndsort) <---)
	$(DELETE) $(ndsort)

	$(call ECHO,---> Removing object files: $(OBJS) <---)
	$(DELETE) $(OBJS)

all :  clean default

DIST_SRC_FILES =  $(DIST_OTHER_FILES) $(OBJS:.o=.c) $(HEADERS) 
DIST_SRC := mo-tools-$(VERSION)-src
dist : DEBUG=0
dist : CDEBUG=
dist : all
	@(mkdir -p ../$(DIST_SRC) \
	&& rsync -rlpC --exclude=.svn $(DIST_SRC_FILES) ../$(DIST_SRC)/ \
	&& cd .. \
	&& tar cf - $(DIST_SRC) | gzip -f9 > $(DIST_SRC).tar.gz \
	&& rm -rf ./$(DIST_SRC)/* && rmdir ./$(DIST_SRC)/ \
	&& echo "$(DIST_SRC).tar.gz created." && cd $(PWD) )

TAGS: *.c *.h
	etags *.c *.h
