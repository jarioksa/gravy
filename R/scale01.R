"scale01" <-
    function (x, xrange) 
{
    if (missing(xrange)) {
        minx <- min(x)
        ranx <- max(x) - minx
    }
    else {
        minx <- xrange[1]
        ranx <- diff(xrange)
    }
    (x - minx)/ranx
}
