memSafeFun <- function(FUN, ..., filename = memSafeFile()) {
    r = FUN(...)
    r = writeRaster(r, filename)
    return(r)
}

memSafe.FUN.apply <- function(applyFUN, r, FUN, ...) {
    memSafeFunc <- function (i, FUN, ...) memSafeFun(FUN, i, ...)
    r = applyFUN(r, memSafeFunc, FUN,...)
    return(r)
}

memSafe.layer.apply <- function(r, FUN, ...)
    memSafe.FUN.apply(layer.apply, r, FUN, ...)

memSafe.lapply <- function(r, FUN, ...)
    memSafe.FUN.apply(lapply, r, FUN, ...)
