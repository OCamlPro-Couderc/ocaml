FILE=proposal

all: soa.tex prop.tex $(FILE).pdf

$(FILE).pdf: $(FILE).tex biblio.bib
	rubber -d $<

clean:
	rm -f *~ 
	rm -f $(FILE).log $(FILE).aux $(FILE).pdf
