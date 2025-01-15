shinylive::export(
  appdir="C:/R/Cycle4Allocation",
  destdir="docs"
)

#remotes::install_github("rstudio/httpuv")

httpuv::runStaticServer(
  dir= "docs",
  port=8888
)

