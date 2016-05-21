library(raster)
library(gitBasedProjects)
library(rasterExtras)
library(rasterPlot)
library(benchmarkMetrics)
setupProjectStructure()

sourceAllLibs()
try(memSafeFile.remove(), silent = TRUE)
memSafeFile.initialise('temp/')

dir = 'data/MODIS/hdf_zipped/'
filenameOut = 'outputs/MODIS'

years = list.files(dir, include.dirs = TRUE, full.names = TRUE)

maskMissing <- function(i) {i[i>1] = NaN; i}

interpolate <- function(dats, mask) {
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

fname = paste(filenameOut, 'climateology-.nc',sep = '-')

if (file.exists(fname)) clim = brick(fname) else {
    addLayers <- function(ci, i, j) r = ci[[j]] + i[[j]]
    clim = dats[[1]]
    for (i in dats[-1]) for (j in 1:nlayers(i))
        clim[[j]] = memSafeFun(addLayers, clim, i, j)

    clim = writeRaster(clim, fname)
}



calculateBenchmarks <- function(dat) {
    year = filename(dat[[1]])
    year = strsplit(year, '-')[[1]][2]

    cat("Calculating Benchmarks for:", year)

    filenames = paste(filenameOut, year,
                     c('Max', 'Min', 'Mean', 'AbsVar', 'Phase', 'Conc'), '.nc',
                     sep = '-')

           saveRasterFun.raster(   max.bigRaster, dat,       filename = filenames[1])
           saveRasterFun.raster(   min.bigRaster, dat,       filename = filenames[2])
    mean = saveRasterFun.raster(  mean.bigRaster, dat,       filename = filenames[3])
           saveRasterFun.raster(absVar.bigRaster, dat, mean, filename = filenames[4])

    if (any(!file.exists(filenames[5:6]))) {
        c(phase, conc) := PolarConcentrationAndPhase(dat, 'months', n = nlayers(dat))

        nullFun <- function(r) r
        saveRasterFun.raster(nullFun, phase, filename = filenames[5])
        saveRasterFun.raster(nullFun, conc, filename = filenames[6])
    }
}

lapply(dats, calculateBenchmarks)
calculateBenchmarks(clim)
