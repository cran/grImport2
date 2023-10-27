library(grid)
library(grImport2)
library(gridSVG)

pic <- readPicture("test-defs-input.svg")

gridsvg("test-defs-output.svg", width = 6, height = 6, annotate=FALSE)
grid.picture(pic, expansion = 0)
dev.off()
