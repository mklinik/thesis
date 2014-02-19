PDFLATEXFLAGS=-halt-on-error

klinik2014-thesis.pdf: klinik2014-thesis.tex computer_science.bib
	pdflatex ${PDFLATEXFLAGS} klinik2014-thesis

remake:
	pdflatex ${PDFLATEXFLAGS} klinik2014-thesis
	bibtex klinik2014-thesis
	pdflatex ${PDFLATEXFLAGS} klinik2014-thesis
	pdflatex ${PDFLATEXFLAGS} klinik2014-thesis
