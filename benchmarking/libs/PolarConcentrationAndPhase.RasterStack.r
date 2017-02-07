PolarConcentrationAndPhase <-
function (dat, phase_units = "radians", n =nlayers(dat),
    disagFact = NaN, justPhase = FALSE)
{
    if (nlayers(dat) < n) {
        warning(paste("number of layers in dat is less than n. n set to",
            nlayers(dat)))
    }
    if (nlayers(dat) > n) {
        dat0 = dat
        dat = dat0[[1:n]]
        for (i in 1:n) {
            index = seq(i, nlayers(dat0), by = n)
            dat[[i]] = mean(dat0[[index]])
        }
    }
    if (!is.na(disagFact))
        dat = layer.apply(dat, disaggregate, disagFact, method = "bilinear")
    out = dat[[1:2]]
    out[,] = NaN
    names(out) = c("Phase", "Concentration")
    nc = ncol(dat); nr = nrow(dat)
    for (i in 1:nr) {#for (j in 1:ncol(dat)){
        cs = getValuesBlock(dat, i, 1)
        if (any(!is.na(cs))) {
            cat("\n %", 100*i/nr)
            x = y = rep(0, n = nc)
            for (k in 1:n) {
                angle = 2 * pi * (n - k + 1) / n
                x = x + cs[, k] * cos(angle)
                y = y + cs[, k] * sin(angle)
            }
            a = apply(cs, 1, sum)

            index =  which(!is.na(x) & a!=0)
            x = x[index]; y = y[index]; a = a[index]
            out[[1]][i, index] = atans(x, y, phase_units)

            if (!justPhase) out[[2]][i, index] = sqrt(x^2 + y^2) / a
        }
    }
    cat("\n")
    if (justPhase)
        return(out[[1]])
    return(out)
}
