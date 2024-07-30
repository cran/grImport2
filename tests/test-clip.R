library(grid)
library(grImport2)
library(gridSVG)

pic <- readPicture("test-clip-input.svg")

gridsvg("test-noclip-output.svg", width = 6, height = 6, annotate=FALSE)
grid.picture(pic, expansion = 0)
dev.off()

gridsvg("test-bboxclip-output.svg", width = 6, height = 6, annotate=FALSE)
grid.picture(pic, expansion = 0, ext="clipbbox")
dev.off()

## Encoding in SVG file (on line 1) differs by platform
model <- readLines("test-noclip-output.svg.save")[-1]
test <- readLines("test-noclip-output.svg")[-1]
same <- model == test
if (! all(same)) {
    stop(paste0("noclip output not equal to expected output",
                "------------------",
                model[!same],
                "------------------",
                test[!same],
                collapse="\n"))
}

model <- readLines("test-bboxclip-output.svg.save")[-1]
test <- readLines("test-bboxclip-output.svg")[-1]
same <- model == test
if (! all(same)) {
    stop(paste0("bboxclip output not equal to expected output",
                "------------------",
                model[!same],
                "------------------",
                test[!same],
                collapse="\n"))
}


