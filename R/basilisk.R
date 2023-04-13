#' @importFrom basilisk BasiliskEnvironment basiliskStart basiliskStop
#'     basiliskRun

bsklenv <- BasiliskEnvironment(
    envname = "bsklenv",
    pkgname = "BiocTNU",
    packages = "pandas==1.3.5", # obligatory packages value
    pip = "terra-notebook-utils==0.11.0"
)
