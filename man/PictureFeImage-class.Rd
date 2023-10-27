\name{PictureFeImage-class}
\docType{class}
\alias{PictureFeImage-class}
\alias{grobify,PictureFeImage-method}

\title{Class \code{"PictureFeImage"}}
\description{
  A description of a colour transformation matrix filter effect
  primitive. To be used in conjunction with the gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{href}:}{Object of class \code{"character"} ~~ }
    \item{\code{result}:}{Object of class \code{"character"} ~~ }
    \item{\code{x}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{y}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{width}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{height}:}{Object of class \code{"characternumeric"} ~~ }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{grobify}{
      \code{signature(object = "PictureFeImage")}: creates an
      \code{feImage} object to include in a filter effect object
      for use with the gridSVG package.
    }
  }
}
\author{
  Paul Murrell
}
\seealso{
  \code{\linkS4class{PictureFilter}}
}
