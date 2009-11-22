TARGETS = readme.html 
INPUTFILE = readme.xml

XSLTPROC = /usr/bin/xsltproc
XSL = /usr/share/xml/docbook/stylesheet/nwalsh/xhtml/docbook.xsl
EPIPHANY = /usr/bin/epiphany

%.html: %.xml $(XSL)
	@$(XSLTPROC) -o $@ $(XSL) $<

readme.html: readme.xml $(XSL)
	@$(XSLTPROC) -o $@ $(XSL) $<

all: 
	$(TARGETS)
	
view:
	@epiphany readme.html

clean:
	@rm -f *.html

install:
	@cp readme.html ../readme.html
	@cp Shogi.jpg ../Shogi.jpg

