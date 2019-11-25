
################################################################################

## SVG content is parsed in TWO sweeps:
## 1: definitions (within <defs>)
## 2: other content

## Parsing generates Picture objects

## Sweep 1 creates "definitions" and stores them (by name) in a
## list of definitions.  Some elements ONLY do this.

## Sweep 2 creates a big tree of Picture
## objects that represent the image.

## Transformations are applied to Picture objects.

## Some elements NEST (e.g., groups, masks, clipPaths, use),
## in which case, the sweep recursively descends.
## Transformations are applied recursively.

## Some attributes work BY REFERENCE (e.g., mask= and clip-path=),
## in which case, we resolve a LABEL to a Picture definition (in both sweeps).

## <use> elements work by REPLACEMENT, in which case, we retrieve
## a Picture definition.
## Transformations are applied (recursively) to a retrieved definition.
## Transformations generate a new definition, with a new LABEL.

################################################################################

getPictureDims <- function(image) {
    svgDims <- c(xmlGetAttr(image, "width"),
                 xmlGetAttr(image, "height"))
    # Remove "pt"
    as.numeric(substring(svgDims, 1, nchar(svgDims) - 2))
}

nullFn <- function(...) { NULL }

parseSVGClipPath <- function(x, createDefs) {
    # Can assume just one child (if necessary)
    # because Cairo SVG does not include more than
    # one definition. It's either a rectangle or a path.
    clipID <- xmlGetAttr(x, "id")
    # children don't set definitions, set createDefs to FALSE
    clipRegion <- parseImage(xmlChildren(x, addNames = FALSE),
                             createDefs = FALSE)
    cp <- new("PictureClipPath",
              content = clipRegion,
              label = clipID)
    if (createDefs)
        setDef(clipID, cp) # should always be this
    else
        cp
}

parseSVGFeColorMatrix <- function(x, createDefs) {
    # type will be a matrix
    # input will be SourceGraphic
    # primary concern is parsing values, which will likely be the following
    # sequence of integers:
    # "0 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0"
    nums <- as.integer(strsplit(xmlGetAttr(x, "values"), " ")[[1]])
    colmat <- matrix(nums, nrow = 4, ncol = 5, byrow=TRUE)
    new("PictureFeColorMatrix",
        type = xmlGetAttr(x, "type"),
        input = xmlGetAttr(x, "in"),
        values = colmat)
}

parseSVGFilter <- function(x, createDefs) {
    # Can assume just one child (if necessary)
    # because Cairo SVG does not include more than
    # a single feColorMatrix element
    filterID <- xmlGetAttr(x, "id")
    # children don't set definitions, set createDefs to FALSE
    fecolmat <- parseImage(xmlChildren(x, addNames = FALSE),
                           createDefs = FALSE)[[1]]
    f <- new("PictureFilter", filterUnits = "bbox",
              x = 0, y = 0, width = 1, height = 1,
              content = fecolmat)
    if (createDefs)
        setDef(filterID, f) # should always be this
    else
        f
}

parseSVGGroup <- function(x, createDefs) {
    # Getting style information to pass on later
    styleList <- svgStyleToList(xmlGetAttr(x, "style"))
    if (length(styleList)) {
        tm <- parseTransform(xmlGetAttr(x, "transform"))
        gp <- svgStyleListToGpar(styleList)
        if (! is.null(tm))
            gp <- applyTransform(gp, tm)
    } else {
        gp <- gpar()
    }
    # Just setting defs from children because we do not have
    # the group as a reference by itself (most likely)
    if (createDefs && is.null(xmlGetAttr(x, "id")))
        return(parseImage(xmlChildren(x, addNames = FALSE),
                          createDefs))

    ## children don't set definitions, set createDefs to FALSE
    children <- do.call("list", parseImage(xmlChildren(x, addNames = FALSE),
                                           FALSE))
    clippath <- xmlGetAttr(x, "clip-path")
    clip <- ! is.null(clippath) 
    if (clip) {
        clipid <- urlToID(clippath)
        clipRegion <- getDef(clipid)
    } else {
        clipRegion <- NULL
    }
    pg <- new("PictureGroup",
              content = children,
              clip = clipRegion,
              maskRef = urlToID(xmlGetAttr(x, "mask")),
              filterRef = urlToID(xmlGetAttr(x, "filter")),
              gp = gp)
    if (createDefs && ! is.null(xmlGetAttr(x, "id")))
        setDef(xmlGetAttr(x, "id"), pg)
    else
        pg
}

parseSVGImage <- function(x, createDefs) {
    imageAttrs <- as.list(xmlAttrs(x))
    bbox <- c(0, 0,
              as.numeric(imageAttrs$width),
              as.numeric(imageAttrs$height))
    pimage <- new("PictureImage",
                  x = 0, y = 0, # Always true, as per cairo source
                  width = bbox[3],
                  height = bbox[4],
                  angle = 0, # just initialising
                  image = readBase64Image(imageAttrs$href),
                  maskRef = urlToID(xmlGetAttr(x, "mask")),
                  bbox = bbox)
    if (createDefs)
        setDef(imageAttrs$id, pimage) # always the case?
    else
        pimage
}

parseSVGLinearGradient <- function(x, createDefs) {
    gradAttrs <- as.list(xmlAttrs(x))
    spreadMethod <-
        if (! is.null(gradAttrs$spreadMethod))
            gradAttrs$spreadMethod
        else
            "pad"
    stops <- lapply(xmlChildren(x, addNames = FALSE),
                    parseSVGStop, createDefs = FALSE)
    tm <- parseTransform(gradAttrs$gradientTransform)
    lg <- new("PictureLinearGradient",
              x0 = as.numeric(gradAttrs$x1),
              y0 = as.numeric(gradAttrs$y1),
              x1 = as.numeric(gradAttrs$x2),
              y1 = as.numeric(gradAttrs$y2),
              spreadMethod = spreadMethod,
              stops = stops)
    if (! is.null(tm))
        lg <- applyTransform(lg, tm)
    setDef(gradAttrs$id, lg) # can only be setting a def
}

parseSVGMask <- function(x, createDefs) {
    maskID <- xmlGetAttr(x, "id")
    # children don't set definitions, set createDefs to FALSE
    maskRegion <- parseImage(xmlChildren(x, addNames = FALSE),
                             createDefs = FALSE)
    m <- new("PictureMask",
             content = maskRegion)
    if (createDefs)
        setDef(maskID, m) # should always be this
    else
        m
}

parseSVGPattern <- function(x, createDefs) {
    patAttrs <- as.list(xmlAttrs(x))
    if (is.null(patAttrs$x))
        patAttrs$x <- 0
    if (is.null(patAttrs$y))
        patAttrs$y <- 0
    tm <- parseTransform(patAttrs$patternTransform)
    children <- parseImage(xmlChildren(x, addNames = FALSE),
                           FALSE)
    pat <- new("PicturePattern",
               x = as.numeric(patAttrs$x), y = as.numeric(patAttrs$y),
               width = as.numeric(patAttrs$width),
               height = as.numeric(patAttrs$height),
               angle = 0, # just initialising
               definition = children)
    if (! is.null(tm)) {
        pat <- applyTransform(pat, tm)
    }
    setDef(patAttrs$id, pat) # can only be setting a def
}

parseSVGPath <- function(x, createDefs) {
    styleList <- svgStyleToList(xmlGetAttr(x, "style"))
    tm <- parseTransform(xmlGetAttr(x, "transform"))
    # Sometimes a path is present where d=""
    d <- xmlGetAttr(x, "d")
    if (! nzchar(d))
        return(NULL)
    rule <-
        if (! is.null(styleList$`fill-rule`))
            styleList$`fill-rule`
        else
            "winding"
    d <- parsePathData(d)
    # If path data is (effectively) empty, return NULL
    if (is.null(d))
        return(NULL)
    gp <- svgStyleListToGpar(styleList)
    points <- getPoints(d, line = TRUE)
    p <- new("PicturePath", d = d, rule = rule,
             gp = gp, bbox = c(min(points$x), max(points$x),
                               min(points$y), max(points$y)))
    # Scaling path and line data in addition to styling
    if (! is.null(tm)) {
        p <- applyTransform(p, tm)
    }
    # won't be used by id, so just return
    p
}

parseSVGRadialGradient <- function(x, createDefs) {
    gradAttrs <- as.list(xmlAttrs(x))
    spreadMethod <-
        if (! is.null(gradAttrs$spreadMethod))
            gradAttrs$spreadMethod
        else
            "pad"
    stops <- lapply(xmlChildren(x, addNames = FALSE),
                    parseSVGStop, createDefs = FALSE)
    tm <- parseTransform(gradAttrs$gradientTransform)
    radgrad <- new("PictureRadialGradient",
                   x = as.numeric(gradAttrs$cx),
                   y = as.numeric(gradAttrs$cy),
                   r = as.numeric(gradAttrs$r),
                   fx = as.numeric(gradAttrs$fx),
                   fy = as.numeric(gradAttrs$fy),
                   spreadMethod = spreadMethod,
                   stops = stops)
    if (! is.null(tm))
        radgrad <- applyTransform(radgrad, tm)
    setDef(gradAttrs$id, radgrad) # can only be setting a def
}

parseSVGRect <- function(x, createDefs) {
    rectAttrs <- as.list(xmlAttrs(x))
    styleList <- svgStyleToList(rectAttrs$style)
    tm <- parseTransform(rectAttrs$transform)
    gp <- svgStyleListToGpar(styleList)
    # If 'x' or 'y' are unspecified,
    # the SVG spec says the value is taken to be zero
    if (is.null(rectAttrs$x)) rectAttrs$x <- 0
    if (is.null(rectAttrs$y)) rectAttrs$y <- 0
    xy <- as.numeric(c(rectAttrs$x, rectAttrs$y))
    wh <- as.numeric(c(rectAttrs$width, rectAttrs$height))
    r <- new("PictureRect",
             x = xy[1], y = xy[2],
             width = wh[1], height = wh[2],
             angle = 0, # just initialising
             gp = gp, bbox = c(xy, xy + wh))
    # Scaling path and line data and gp
    if (! is.null(tm))
        r <- applyTransform(r, tm)
    # won't be used by id so just return
    r
}

parseSVGStop <- function(x, createDefs) {
    # ignore defs and createalways content for another definition
    stopAttrs <- as.list(xmlAttrs(x))
    offset <- as.numeric(stopAttrs$offset)
    styleList <- svgStyleToList(stopAttrs$style)
    col <- stopsToCol(styleList)
    new("PictureGradientStop", offset = offset, col = col)
}

parseSVGSymbol <- function(x, createDefs) {
    symbolID <- xmlGetAttr(x, "id")
    symbolChildren <- parseImage(xmlChildren(x, addNames = FALSE),
                                 createDefs = FALSE)
    if (is.null(symbolChildren) || ! length(symbolChildren) ||
        is.null(unlist(symbolChildren)))
        symbolChildren <-
            list(new("PictureRect",
                     x = 0, y = 0,
                     width = 0, height = 0,
                     angle = 0,
                     gp = gpar(col = "#FFFFFF00", fill = "#FFFFFF00"),
                     bbox = rep(0, 4)))
    symbol <- new("PictureSymbol",
                  definition = symbolChildren)
    setDef(symbolID, symbol)
}

parseSVGUse <- function(x, createDefs) {
    # Will not be used to set definitions (because a <use> will only
    # refer to something that already exists, and *not* define anything
    # new).
    useAttrs <- as.list(xmlAttrs(x))
    def <- getDef(hrefToID(useAttrs$href))
    if (is.null(def))
        return(NULL) # Assume that the source is not drawable
    # <use> could spec 'x' and 'y' OR 'transform' (latter for image)
    # OR possibly neither (in which case provide identity transform)
    if (is.null(useAttrs$transform)) {
        if (is.null(useAttrs$x))
            tx <- 0
        else
            tx <- as.numeric(useAttrs$x)
        if (is.null(useAttrs$y))
            ty <- 0
        else
            ty <- as.numeric(useAttrs$y)
        ## No scaling, just translation
        ## Still need to build up a transformation matrix (for consistency)
        tm <- diag(3)
        tm[1, 3] <- tx
        tm[2, 3] <- ty
    } else {
        tm <- parseTransform(useAttrs$transform)
    }
    ## Now need to act according to the type of content we have pulled out
    ## For example, for a <symbol>, we're only interested in the definition 
    ## and not the <symbol> itself.
    ## We'll only be seeing this for symbols and patterns in practice (as
    ## per the cairo source)
    if (is(def, "PictureSymbol")) {
        ## symbol, assume only one child!
        ## safe because they're only used for font glyphs, which are known
        ## to be single <path> elements
        applyTransform(def@definition[[1]], tm)
    } else if ("maskRef" %in% slotNames(def) &&
               ! is.null(xmlGetAttr(x, "mask"))) {
        ## Shift mask on <use> to mask on def
        def@maskRef <- urlToID(xmlGetAttr(x, "mask"))
        applyTransform(def, tm) 
    } else if (is(def, "PictureImage")) {
        applyTransform(def, tm) # image 
    } else {
        applyTransform(def, tm) 
    }
}

parseImage <- function(x, createDefs = FALSE) {
    # Ensure there is a list of content to iterate over
    if (inherits(x, "XMLInternalNode"))
        x <- list(x)

    result <- vector("list", length(x))

    for (i in seq_len(length(x))) {
        node <- x[[i]]
        el <- xmlName(node)
        ID <- xmlGetAttr(node, "id")
        svgFn <- switch(el,
                        clipPath = parseSVGClipPath,
                        feColorMatrix = parseSVGFeColorMatrix,
                        filter = parseSVGFilter,
                        g = parseSVGGroup,
                        image = parseSVGImage,
                        linearGradient = parseSVGLinearGradient,
                        mask = parseSVGMask,
                        pattern = parseSVGPattern,
                        path = parseSVGPath,
                        radialGradient = parseSVGRadialGradient,
                        rect = parseSVGRect,
                        symbol = parseSVGSymbol,
                        use = parseSVGUse,
                        nullFn)
        result[[i]] <- svgFn(node, createDefs)
    }

    if (length(result) > 1)
        result <- result[! sapply(result, is.null)]
    else
        result
}

parsePictureDefinitions <- function(svgImage) {
    parseImage(getNodeSet(svgImage, "//svg:defs/*",
                          c(svg = "http://www.w3.org/2000/svg")),
               createDefs = TRUE)
}
