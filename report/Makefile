report.pdf:
	mkdir build
	pdflatex --output-directory build report.tex
	bibtex build/report
	pdflatex --output-directory build report.tex
	pdflatex --output-directory build report.tex
	mv build/report.pdf .

.PHONY : clean
clean :
	rm -rf build/ report.pdf
