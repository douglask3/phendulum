library(raster)
library(gitBasedProjects)
library(rasterExtras)
library(rasterPlot)
setupProjectStructure()

sourceAllLibs()
try(memSafeFile.remove(), silent = TRUE)
memSafeFile.initialise('temp/')

dir = 'data/MODIS/hdf_zipped/'
filenameOut = 'outputs/MODIS'

years = list.files(dir, include.dirs = TRUE, full.names = TRUE)

maskMissing <- function(i) {i[i>1] = NaN; i}

interpolate <- function(dats, mask) {
    blockSize = 10
    interpolateCell <- function(d, m) {
        if (m) d[] = NaN
        else {
            index = is.na(d)
            if (any(index)) {
                index1 = which(index); index2 = which(!index); index = 1:length(d)
                spl = spline(index[index2], d[index2], xout = index[index1])[[2]]
                spl[spl<0] = 0; spl[spl>1] = 1
                d[index1] = spl
            }
        }
        return(d)
    }

    dvals = values(dats)
    mvals = values(mask)

    for (i in 1:nrow(dvals)) dvals[i,] = interpolateCell(dvals[i,], mvals[i])
    for (i in 1:ncol(dvals)) dats[[i]] = memSafeFun(function(i) {mask[] = i; mask}, dvals[,i])
    return(dats)
}

openYear <- function(year) {
    filename = paste(filenameOut, tail(strsplit(year, '/')[[1]], 1), 'interp.nc', sep = '-')
    if (file.exists(filename)) return(stack(filename))

    cat('Opening and processing files from: \n\t', filename, '\n\n')

    cat("\t find files \n")
    files = list.files(year, full.names = TRUE)
    files = files[substr(files, nchar(files) - 3, nchar(files)) == ".hdf"]

    cat("\t Opening data \n")
    dats  = layer.apply(files, rasterTry, files[1])
    dats  = memSafe.layer.apply(dats, maskMissing)

    cat("\t Calculating Mask \n")
    Missing  = memSafe.layer.apply(dats, function(i) is.na(i))
    nMissing = Missing[[1]]
    for (i in 2:nlayers(Missing)) nMissing = nMissing + Missing[[i]]
    mask = nMissing>(nlayers(dats))*0.9

    cat("\t Filling in missing values \n")
    dats = interpolate(dats, mask)

    cat("\t Outputting \n")
    dats = writeRaster(dats, filename)

    return(dats)
}


## Open All (1 year per list item)
dats = lapply(years, openYear)

fname = paste(filenameOut, 'climateology.nc',sep = '-')

if (file.exists(fname)) clim = stack(filename) else {
    addLayers <- function(ci, i, j) r = ci[[j]] + i[[j]]
    clim = dats[[1]]
    for (i in dats[-1]) for (j in 1:nlayers(i))
        clim[[j]] = memSafeFun(addLayers, clim, i, j)

    clim = writeRaster(clim, fname)
}
