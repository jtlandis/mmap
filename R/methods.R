Ops.mmap <- function(e1,e2) {

  if(!is.element(.Generic, c("==","!=",">=","<=",">","<")))
    stop(paste(.Generic,"unsupported for 'mmap' objects"))
  if(is.mmap(e1)) {
    OPS <- switch(.Generic,"=="=1L,
                  "!="=2L,
                  ">="=3L,
                  "<="=4L,
                  ">"= 5L,
                  "<"= 6L,
                  -1L)
    if(storage.mode(e1$storage.mode) == "character")
      e2 <- charToRaw(e2)
    .Call("mmap_compare", e2, OPS, e1)
  } else if(is.mmap(e2)) {
    OPS <- switch(.Generic,"=="=1L,
                  "!="=2L,
                  ">="=4L,
                  "<="=3L,
                  ">"= 6L,
                  "<"= 5L,
                  -1L)
    if(storage.mode(e2$storage.mode) == "character")
      e1 <- charToRaw(e1)
    .Call("mmap_compare", e1, OPS, e2)
  }
}

do_struct_ops <- function(mmap, field, comp, ops) {
  if(!is.element(ops, c("==","!=",">=","<=",">","<")))
    stop(paste(ops,"unsupported for 'mmap' objects"))

  if(!is.mmap(mmap))
    stop("arg `mmap` must be a mmap object")

  if (!is.struct(mmap$storage.mode))
    stop("arg `mmap` must have a 'struct' storage mode")

  OPS <- switch(ops,"=="=1L,
                "!="=2L,
                ">="=3L,
                "<="=4L,
                ">"= 5L,
                "<"= 6L,
                -1L)
  if (OPS==-1L)
    stop(ops, " unsupported operation")

  if (is.character(field)) {
    f <- match(field, names(mmap$storage.mode), nomatch = NA_character_)
    if (is.na(f))
      stop(field, " does not match any names: ", names(mmap$storage.mode))
  } else if(!is.integer(field)) {
    f <- as.integer(field)
    if (f>length(mmap$storage.mode))
      stop("coerced ", field, " to integer and index is out of bounds")
  } else {
    f <- field
  }

  if (storage.mode(mmap$storage.mode[[f]])=="character")
    comp <- charToRaw(comp)

  .Call("mmap_compare_struct", comp, OPS, mmap, as.integer(field))

}

dim.mmap <- function(x) {
  if( is.struct(x$storage.mode))
    return( c(length(x), length(x$storage.mode)) )
  if(is.null(x$dim))
    c(length(x),1)
  else
    x$dim
}

`dim<-.mmap` <- function(x, value) {
  if( is.struct(x$storage.mode))
    stop("dimensions are fixed for struct objects")
  if( is.null(value)) {
    x$dim <- value
  } else
  if( length(value) != 2) {
    stop("only dimension of length two supported")
  } else x$dim <- as.integer(value)
  x
}

dimnames.mmap <- function(x) {
  if( is.struct(x$storage.mode))
    list(NULL, names(x$storage.mode))
  else x$dimnames
}

`dimnames<-.mmap` <- function(x, value) {
  if( is.null(dim(x)))
    stop("'dimnames' applied to non-array")
  if( is.struct(x$storage.mode)) {
    names(x$storage.mode) <- value[[2]]
    x$dimnames <- value
  } else x$dimnames <- value
  x
}

is.array.mmap <- function(x) TRUE  # used for NROW/NCOL
