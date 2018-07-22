#' mappend
#'
#' The monoidal append operation. Currently
#' implemented for:
#' 1. `list`s under concatenation,
#' 1. `data.frame`s under row binding
#' 1. `character` under `paste0`
#' 1. `numeric` and `integer` under addition
#'
#' appends two elements of the correct type.
#' @param x The left element
#' @param y The right element
#' @return The result of appending
#' @export
mappend <- function(x,y) UseMethod("mappend")

#' @rdname mappend
#' @export
"%<>%" <- mappend

#' @export
mappend.list <- function(x,y){
  if(!is.list(y))
    stop("Second argument to `mappend` was of the wrong type, expected `list`, received "
       , class(y))
  c(x,y)
}
#' @export
mappend.data.frame <- function(x,y){
  if(!is.data.frame(y))
    stop("Second argument to `mappend` was of the wrong type, expected `data.frame`"
       , ", received ", class(y))

  bind_rows(x,y)
}
#' @export
mappend.character <- function(x,y){
  if(!is.character(y))
    stop("Second argument to `mappend` was of the wrong type, expected `character`"
       , ", recieved ", class(y))

  paste0(x,y)
}
#' @export
mappend.numeric <- function(x,y){
  if(!is.numeric(y))
    stop("Second argument to `mappend` was of the wrong type, expected `numeric`"
       , ", recieved ", class(y))

  x + y
}
#' @export
mappend.integer <- function(x,y){
  if(!is.numeric(y))
    stop("Second argument to `mappend` was of the wrong type, expected `integer`"
       , ", recieved ", class(y))

  x + y
}

#' mempty
#'
#' The identity element of the monoid currently:
#'
#' 1. `list()` for `list`s
#' 1. `data.frame()` for `data.frame`s
#' 1. `""` for `character`
#' 1. `0` for `numeric` and `integer`
#' @param x An unused element indicating the desired type
#' of the identity element.
#' @return An identity element of the correct type
#' @export
mempty <- function(x) UseMethod("mempty")
#' @export
mempty.list <- function(x) list()
#' @export
mempty.data.frame <- function(x) data.frame()
#' @export
mempty.character <- function(x) ""
#' @export
mempty.numeric <- function(x) 0
#' @export
mempty.integer <- function(x) 0L

#' mconcat
#'
#' Append a list (or occaisonally vector) of elements of
#' a monoid. Default implementation is to use [purrr::reduce]
#' with `mappend` and `mempty` as the initial element. Fast
#' implementations are provided for each currently implemented
#' type.
#' @param xs A `list` or ocassionally `vector` of elements of
#' the type of the monoid.
#' @return An element of the type of the monoid
#' @export
mconcat <- function(xs) UseMethod("mconcat", xs[[1]])

#' @export
mconcat.default <- function(xs){
  if(!is_monoid(xs[[1]]))
    stop("Elements do not appear to be from a monoidal type")
     
  mappend_typed <- getS3method("mappend", class(xs[[1]]))
  reduce(xs, mappend_typed, .init = xs[[1]])
}

#' @export
mconcat.numeric <- function(xs){
  if(!all(purrr::map_lgl(xs, is.numeric)))
    stop("All arguments to mconcat must be numeric")
  sum(unlist(xs))
}

#' @export
mconcat.integer <- function(xs){
  if(!all(purrr::map_lgl(xs, is.integer)))
    stop("All arguments to mconcat must be integer")
  sum(unlist(xs))
}

#' @export
mconcat.data.frame <- function(xs){
  if(!all(purrr::map_lgl(xs, is.data.frame)))
    stop("All arguments to mconcat must be `data.frame`'s")

  dplyr::bind_rows(rlang::UQS(xs))
}

#' @export
mconcat.character <- function(xs){
  if(!all(purrr::map_lgl(xs, is.character)))
    stop("All arguments to mconcat must be `character`")

  rlang::eval_tidy(
           rlang::quo(
                    paste0(rlang::UQS(xs))))
}

#' is_monoid
#'
#' Check if a type is a monoid, checks for implementations
#' of `mappend` and `mempty`
#' @param x The object whose type to test
#' @return boolean whether or not the type implements monoid
#' functionality
#' @export
is_monoid <- function(x){
  tryCatch(getS3method("mappend", class = class(x))
         , error = function(e) return(FALSE))

  tryCatch(getS3method("mempty", class = class(x))
         , error = function(e) return(FALSE))

  TRUE
}
