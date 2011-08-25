.onAttach <- function(lib, pkg) {
    packageStartupMessage("This is gravy ",
                          utils::packageDescription("gravy",
                                                    field="Version"),
                          appendLF = TRUE)

}

