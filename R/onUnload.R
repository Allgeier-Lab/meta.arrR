# http://r-pkgs.had.co.nz/src.html#c-best-practices
# https://stackoverflow.com/questions/27076732/dynamic-library-not-loading-in-r-binary-package-build

# nocov start

.onUnload <- function(libpath) {library.dynam.unload("meta.arrR", libpath)}

# nocov end
