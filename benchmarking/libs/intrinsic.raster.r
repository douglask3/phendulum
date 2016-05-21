max.raster <- function(...) max(...)
min.raster <- function(...) max(...)
mean.raster <- function(...) max(...)


FUN.bigRaster <- function(b, FUN , ...) {
    nl = nlayers(b)
    if (nl == 2) r = memSafeFun(FUN, b, ...) else {
        f = memSafeFun(FUN, b[[1:2]], ...)

        for (i in seq(3, nl, by = 2)) {
            ii = min(c(i+1, nl))
            f = addLayer(f, memSafeFun(FUN, b[[i:ii]], ...))
        }

        r = FUN.bigRaster(f, FUN, ...)
    }
    return(r)
}

 max.bigRaster <- function(...) FUN.bigRaster( FUN = max.raster, ...)
 min.bigRaster <- function(...) FUN.bigRaster(FUN = min.raster, ...)
mean.bigRaster <- function(...) FUN.bigRaster(FUN = mean.raster, ...)
 sum.bigRaster <- function(...) FUN.bigRaster(FUN = mean.raster, ...)
