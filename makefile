OUT := tsuru
MAIN_HS := Main.hs

all: release

release:$(MAIN_HS)
	ghc -O --make $(MAIN_HS) -o $(OUT)
clean:
	rm -f *.*~ *~ *.o *.hi $(OUT)