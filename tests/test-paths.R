library(grid)
library(grImport2)
library(gridSVG)

pic <- readPicture("test-path-simple-input.svg")

gridsvg("test-path-simple-output.svg", width = 6, height = 6, annotate = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

pic <- readPicture("test-path-complex-input.svg")

gridsvg("test-path-complex-output.svg", width = 6, height = 6, annotate = FALSE)
grid.picture(pic, expansion = 0)
dev.off()

## Encoding in SVG file (on line 1) differs by platform
model <- readLines("test-path-simple-output.svg.save")[-1]
test <- readLines("test-path-simple-output.svg")[-1]
same <- model == test
if (! all(same)) {
    stop(paste0("path-simple output not equal to expected output",
                "------------------",
                model[!same],
                "------------------",
                test[!same],
                collapse="\n"))
}

model <- readLines("test-path-complex-output.svg.save")[-1]
test <- readLines("test-path-complex-output.svg")[-1]
same <- model == test
if (! all(same)) {
    ## Protect against tiny rounding differences in locations
    x <- suppressWarnings(lapply(strsplit(model[!same], " "), as.numeric))
    y <- suppressWarnings(lapply(strsplit(test[!same], " "), as.numeric))
    rx <- unlist(lapply(x, round, 1))
    ry <- unlist(lapply(y, round, 1))
    if (!all.equal(rx, ry)) {
        stop(paste0("path-complex output not equal to expected output",
                    "------------------",
                    model[!same],
                    "------------------",
                    test[!same],
                    collapse="\n"))
    }
}


