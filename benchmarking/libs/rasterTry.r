rasterTry <- function(file, rasterTemplate, rasterFun = raster) {
    test = try(rasterFun(file))
    if (is.raster(test)) return(test)

    if (is.character(rasterTemplate)) rasterTemplate = rasterFun(rasterTemplate)
    rasterTemplate[] = NaN
    return(rasterTemplate)
}

raster.modis <- function(file) {
    r = raster(file, crs = "+proj=longlat +datum=WGS84")
    extent(r) = extent(c(110.00, 155, -45, -10.00))
    return(r)
}
