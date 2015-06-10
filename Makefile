
FIG=figure/elbow-correct-data.pdf \
 	figure/elbow-correct-withinss.pdf \
	figure/elbow-incorrect-data.pdf \
 	figure/elbow-incorrect-withinss.pdf \
	figure/null-2d-corr-equal.pdf \
	figure/null-2d-corr-proportions.pdf

all: $(FIG)

sim/null-2d-corr.csv:
	cd sim && Rscript --vanilla null-2d-corr.R

figure/elbow-correct-data.pdf figure/elbow-correct-withinss.pdf figure/elbow-incorrect-data.pdf figure/elbow-incorrect-withinss.pdf:
	cd figure && Rscript --vanilla elbow.R

figure/null-2d-corr-equal.pdf figure/null-2d-corr-proportions: sim/null-2d-corr.csv
	cd figure && Rscript --vanilla null-2d-corr.R

