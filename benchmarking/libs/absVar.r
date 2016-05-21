absVar <- function(dat, mn = meanFun(dat), meanFun = mean) {
    r = dat - mn
    r = meanFun(dat)
    return(r)
}

absVar.raster <- function(...) absVar(..., meanFun = mean.raster)

absVar.bigRaster <- function(dat, mn = mean.bigRaster(dat), ...) {
    r = memSafe.layer.apply(dat, function(i) abs(i - mn))
    r = mean.bigRaster(dat)
}
