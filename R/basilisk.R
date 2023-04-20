#' @importFrom basilisk BasiliskEnvironment basiliskStart basiliskStop
#'     basiliskRun

bsklenv <- basilisk::BasiliskEnvironment(
    envname = "bsklenv",
    pkgname = "BiocTNU",
    packages = "pandas==1.3.5", # obligatory packages value
    pip = "terra-notebook-utils==0.12.0"
)
