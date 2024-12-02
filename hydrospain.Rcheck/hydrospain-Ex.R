pkgname <- "hydrospain"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('hydrospain')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("hydrospain")
### * hydrospain

flush(stderr()); flush(stdout())

### Name: hydrospain
### Title: Read data from gauging stations of most largest Spanish rivers
### Aliases: hydrospain

### ** Examples

## Not run: 
##D # Read afliq.csv data from basin 'cantabrico'.
##D x <- hydrospain(file_name = "afliq", basin_nam = "cantabrico", verbose = FALSE)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
