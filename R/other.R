
#' demonstrate use of basilisk to present features of terra-notebook-utils
#' @examples
#' tnu_top()
#' @export
tnu_top = function() {
  proc = basilisk::basiliskStart(bsklenv)
  on.exit(basilisk::basiliskStop(proc))
  basilisk::basiliskRun(proc, function() {
    tnu = reticulate::import("terra_notebook_utils")
    names(tnu)
    })
}

#' demonstrate use of basilisk to help on terra-notebook-utils
#' @examples
#' tnu_help()
#' @export
tnu_help = function() {
  proc = basilisk::basiliskStart(bsklenv)
  on.exit(basilisk::basiliskStop(proc))
  basilisk::basiliskRun(proc, function() {
    tnu = reticulate::import("terra_notebook_utils")
    reticulate::py_help(tnu)
    })
}

#' demonstrate use of basilisk to help on terra-notebook-utils DRS utilities
#' @examples
#' tnu_drs_help()
#' @export
tnu_drs_help = function() {
  proc = basilisk::basiliskStart(bsklenv)
  on.exit(basilisk::basiliskStop(proc))
  basilisk::basiliskRun(proc, function() {
    tnu_drs = reticulate::import("terra_notebook_utils.drs")
    reticulate::py_help(tnu_drs)
    })
}
