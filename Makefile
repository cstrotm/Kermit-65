# $Id: Makefile,v 1.1 2000/12/29 00:13:07 chris Exp $
#
# makefile for cc65 kermit65 version
#

AS = ca65
LD = ld65
RM = rm -f

all: k65.com k65nr.com
	$(.NULL)

clean:
	$(RM) k65.com kermit65.o kermit65.map loadrs.com loadrs.o loadrs.map k65nr.com

k65nr.com: kermit65.lnk kermit65.o
	$(LD) -m kermit65.map -o $@ -C $^

k65.com: loadrs.com k65nr.com
	cat $^ > $@

kermit65.o: kermit65.s
	$(AS) -t atari -o $@ $^

loadrs.com: loadrs.lnk loadrs.o
	$(LD) -m loadrs.map -o $@ -C $^

loadrs.o: loadrs.s
	$(AS) -t atari -o $@ $^
