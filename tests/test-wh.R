library(grid)
library(grImport2)
library(gridSVG)

test <- readPicture("test-wh-input.svg")

dev <- function(name, width, height) {
    gridsvg(name, width = width, height = height, annotate = FALSE)
}

## Check that 'width' and 'height' default to sensible unit(1, "npc")
dev("test-wh-default.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0)
dev.off()

dev("test-wh-default-wider.svg", width=8, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0)
dev.off()

dev("test-wh-default-taller.svg", width=4, height=8)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0)
dev.off()

## Check justification
dev("test-wh-default-wider-left.svg", width=8, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0, x=0, hjust=0)
dev.off()

dev("test-wh-default-taller-bottom.svg", width=4, height=8)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0, y=0, vjust=0)
dev.off()

## Check that aspect ratio is retained if only one of width/height is given
dev("test-wh-width-null.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             height=.5,
             x=.25, y=.25, hjust=0, vjust=0)
dev.off()

dev("test-wh-height-null.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             width=.5,
             x=.25, y=.25, hjust=0, vjust=0)
dev.off()

## For sanity, check that setting width and height is respected
dev("test-wh-set.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             width=.5, height=.5)
dev.off()

dev("test-wh-set-wider.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             width=1, height=.5)
dev.off()

## Various repetitions allowing distortion
dev("test-wh-set-distort.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             width=.5, height=.5,
             distort=TRUE)
dev.off()

dev("test-wh-set-wider-distort.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             width=1, height=.5,
             distort=TRUE)
dev.off()

dev("test-wh-default-wider-distort.svg", width=8, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             distort=TRUE)
dev.off()

dev("test-wh-default-taller-distort.svg", width=4, height=8)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             distort=TRUE)
dev.off()

dev("test-wh-width-null-distort.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             height=.5,
             x=.25, y=.25, hjust=0, vjust=0,
             distort=TRUE)
dev.off()

dev("test-wh-height-null-distort.svg", width=4, height=4)
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.picture(test, expansion=0,
             width=.5,
             x=.25, y=.25, hjust=0, vjust=0,
             distort=TRUE)
dev.off()

###############
modelFiles <- list.files(pattern="^test-wh-.+[.]save")
testFiles <- gsub("[.]save", "", modelFiles)
for (i in seq_along(modelFiles)) {
    model <- readLines(modelFiles[i])[-1]
    test <- readLines(testFiles[i])[-1]
    same <- model == test
    if (! all(same)) {
        stop(paste0("wh output not equal to expected output",
                    "------------------",
                    model[!same],
                    "------------------",
                    test[!same],
                    collapse="\n"))
    }
}
