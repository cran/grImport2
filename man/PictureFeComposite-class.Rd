\name{PictureFeComposite-class}
\docType{class}
\alias{PictureFeComposite-class}
\alias{grobify,PictureFeComposite-method}

\title{Class \code{"PictureFeComposite"}}
\description{
  A description of a colour transformation matrix filter effect
  primitive. To be used in conjunction with the gridSVG package.
}
\section{Slots}{
  \describe{
    \item{\code{input1}:}{Object of class \code{"character"} ~~ }
    \item{\code{input2}:}{Object of class \code{"character"} ~~ }
    \item{\code{operator}:}{Object of class \code{"character"} ~~ }
    \item{\code{k1}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{k2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{k3}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{k4}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{colorspace}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
  Class \code{\linkS4class{PictureContent}}, directly.
}
\section{Methods}{
  \describe{
    \item{grobify}{
      \code{signature(object = "PictureFeComposite")}: creates an
      \code{feComposite} object to include in a filter effect object
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
