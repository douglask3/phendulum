saveRasterFun <- function(FUN, ..., filename, rasterFUN = raster) {
    if (file.exists(filename)) return(raster(filename))
    return(memSafeFun(FUN, ..., filename = filename))
}

saveRasterFun.brick <- function(...) saveRasterFun(..., rasterFUN = brick)
saveRasterFun.stack <- function(...) saveRasterFun(..., rasterFUN = stack)
saveRasterFun.raster <- function(...) saveRasterFun(...)
