rasterTry <- function(file, rasterTemplate) {
    test = try(raster(file))
    if (is.raster(test)) return(test)

    if (is.character(rasterTemplate)) rasterTemplate = raster(rasterTemplate)
    rasterTemplate[] = NaN
    return(rasterTemplate)
}
