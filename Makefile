HC  := ghc
HCFLAGS := -O -odirobj -package text -fglasgow-exts
PROG := adelie

all : $(PROG)

.PHONY: adelie
adelie : Main.hs
	$(HC) $(HCFLAGS) --make -o $@ $<

.PHONY : strip
strip :
	strip $(PROG)
