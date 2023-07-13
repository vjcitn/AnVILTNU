#' @importFrom basilisk BasiliskEnvironment basiliskStart basiliskStop
#'     basiliskRun

bsklenv <- basilisk::BasiliskEnvironment(
    envname = "bsklenv",
    pkgname = "AnVILTNU",
    packages = "pandas==1.3.5", # obligatory packages value
    pip = c("urllib3==1.26.15","terra-notebook-utils==0.12.0")
)
