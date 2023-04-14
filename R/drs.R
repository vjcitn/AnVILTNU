#' @rdname drs_access
#'
#' @title Access DRS through terra-notebook-utilities
#'
#' @description `drs_access()` acquires a signed URL from a DRS
#'     address.
#'
#' @param drs_uri character(1) DRS URI to be resolved
#'
#' @return character(1) signed URL
#'
#' @note The URI used in the example is a CCDG CRAM file reference;
#'     the associated CRAI file has DRS URI
#'     `drs://dg.4503:dg.4503/1447260e-654b-4f9a-9161-c511cbdd0f95`
#'
#' @importFrom reticulate import
#'
#' @examples
#' uri <- "drs://dg.4503:dg.4503/17141a26-90e5-4160-9c20-89202b369431"
#' if (tnu_workspace_ok()) {
#'     drs_access(uri)
#' }
#'
#' @export
drs_access <-
    function(drs_uri)
{
    stopifnot(
        is_drs_uri(drs_uri),
        tnu_workspace_ok()
    )
    namespace <- tnu_workspace_namespace()
    name <- tnu_workspace_name()

    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, function(drs_uri) {
        drs_module <- import("terra_notebook_utils.drs")
        drs_module$access(
            drs_uri,
            workspace_namespace = namespace,
            workspace_name = name
        )
    }, drs_uri = drs_uri)
}
