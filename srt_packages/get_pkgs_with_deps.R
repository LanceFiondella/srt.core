library(tools)
chooseCRANmirror(graphics=FALSE, ind=48)
required_packages <- c("shiny", "knitr", "ggplot2", "rootSolve", "DT", "gdata")
pwd <- getwd()
pkg_dir <- paste(pwd,"/packages", sep="")
contrib_dir <- paste("file://",pwd,"/packages", sep="")

getPkgsDep <- function(packs){
  packages <- unlist(
    tools::package_dependencies(packs, available.packages(),
                         which=c("Depends", "Imports"), recursive=TRUE)
  )
  packages <- union(packs, packages)
  packages
}

download.packages(getPkgsDep(required_packages), destdir="./packages", type="source")
write_PACKAGES("./packages")
install.packages(c("shiny", "ggplot2", "rootSolve", "DT", "gdata", "knitr"), lib="./lib", contriburl=contrib_dir)


# Renviron
# .libpaths 
# 