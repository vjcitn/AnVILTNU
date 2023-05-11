#' @importFrom reticulate py_capture_output import_builtins import

my_help <-
    function(...)
{
    output <- py_capture_output(import_builtins()$help(...), type = "stdout")

    cat(output, sep = "\n")
    output
}

#' @rdname tnu_utilities
#'
#' @title Helpers to Discover terra-notebook-utils Functionality
#'
#' @description `tnu_top()` demonstrates use of basilisk to present
#'     features of terra-notebook-utils
#'
#' @examples
#' tnu_top()
#'
#' @export
tnu_top <-
    function()
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, function() {
        tnu <- import("terra_notebook_utils")
        names(tnu)
    })
}

#' @rdname tnu_utilities
#'
#' @description `tnu_help()` provides top-level help on
#'     terra-notebook-utils python functions
#'
#' @examples
#' tnu_help()
#'
#' @export
tnu_help <-
    function()
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    result <- basiliskRun(proc, function() {
        tnu <- import("terra_notebook_utils")
        ## reticulate::py_help(tnu)
        my_help(tnu)
    })

    invisible(result)
}

#' @rdname tnu_utilities
#'
#' @description `tnu_drs_help()` provides help on
#'     terra-notebook-utils 'drs' module.
#'
#' @examples
#' tnu_drs_help()
#'
#' @export
tnu_drs_help <-
    function()
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    result <- basiliskRun(proc, function() {
        tnu_drs <- import("terra_notebook_utils.drs")
        ## reticulate::py_help(tnu_drs)
        my_help(tnu_drs)
    })
    invisible(result)
}

#' @rdname tnu_utilities
#'
#' @description `tnu_table_help()` provides help on
#'     terra-notebook-utils 'table' module.
#'
#' @examples
#' tnu_table_help()
#'
#' @export
tnu_table_help <-
    function()
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    result <- basiliskRun(proc, function() {
        tnu_drs <- import("terra_notebook_utils.table")
        ## reticulate::py_help(tnu_drs)
        my_help(tnu_drs)
    })
    invisible(result)
}

