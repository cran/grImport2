library(grid)
library(grImport2)
library(gridSVG)

pic <- readPicture("test-raster-input.svg")

gridsvg("test-raster-output.svg", width = 6, height = 6, annotate = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

## This test is not run be default because it is sensitive to changes
## in the temporary PNG file that is created when exporting raster
## to SVG via 'gridSVG'
notrun <- function() {
    model <- readLines("test-raster-output.svg.save")[-1]
    test <- readLines("test-raster-output.svg")[-1]
    same <- model == test
    if (! all(same)) {
        stop(paste0("raster output not equal to expected output",
                    "------------------",
                    model[!same],
                    "------------------",
                    test[!same],
                    collapse="\n"))
    }
}

