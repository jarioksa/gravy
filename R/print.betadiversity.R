"print.betadiversity" <-
    function(x, ...)
{
    out <- rbind("Gradient" = x$x, "Betadiversity" = x$beta)
    print(out)
    invisible(x)
}
