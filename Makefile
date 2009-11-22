export prefix = /usr
export bindir = $(prefix)/games
export datarootdir = $(prefix)/share
export datadir = $(datarootdir)/games/xiangqiboard

VERSION = $(shell grep '^Version' ChangeLog | head -n 1 | awk '{ print $$2; }')

xiangqiboard: ./src/xiangqiboard.hs
	ghc --make ./src/xiangqiboard.hs

install: xiangqiboard 
	mkdir -p $(datadir)
	mkdir -p $(prefix)/share/doc/xiangqiboard
	cp ./src/xiangqiboard $(prefix)/games
	cp ./data/*.svg $(datadir)
	cp ./docs/help/xiangqiboard.xml $(prefix)/share/doc/xiangqiboard
	cp ./docs/help/xiangqiboard.jpg $(prefix)/share/doc/xiangqiboard

uninstall: 
	rm -r $(datadir)
	rm $(prefix)/games/xiangqiboard
	rm -r $(prefix)/share/doc/xiangqiboard

deb: xiangqiboard
	mkdir -p ./debs
	mkdir -p ./debs/xiangqiboard-deb/DEBIAN
	mkdir -p ./debs/xiangqiboard-deb/usr/games
	mkdir -p ./debs/xiangqiboard-deb/usr/share/games/xiangqiboard
	mkdir -p ./debs/xiangqiboard-deb/usr/share/doc/xiangqiboard
	cp AUTHORS ChangeLog COPYING NEWS README TODO ./debs/xiangqiboard-deb/DEBIAN
	cp ./src/xiangqiboard ./debs/xiangqiboard-deb/usr/games
	cp ./data/*.svg ./debs/xiangqiboard-deb/usr/share/games/xiangqiboard
	cp ./docs/help/xiangqiboard.xml ./debs/xiangqiboard-deb/usr/share/doc/xiangqiboard
	cp ./docs/help/xiangqiboard.jpg ./debs/xiangqiboard-deb/usr/share/doc/xiangqiboard
	cp control ./debs/xiangqiboard-deb/DEBIAN
	dpkg-deb -b ./debs/xiangqiboard-deb xiangqiboard_0.1.3_i386.deb

clean:
	rm -f -r ./debs
	rm -f xiangqiboard*.deb
	rm -f ./src/xiangqiboard 
	rm -f ./src/xiangqiboard.hi 
	rm -f ./src/xiangqiboard.o


