# demonstrate acquisition of signed URL
#' @param drs_uri character(1) DRS URI to be resolved
#' @return character(1) signed URL
#' @note the default value for drs_uri is a CCDG CRAM file reference; the associated
#' CRAI file has DRS URI `drs://dg.4503:dg.4503/1447260e-654b-4f9a-9161-c511cbdd0f95`
#' @export
drs_access = function(drs_uri='drs://dg.4503:dg.4503/17141a26-90e5-4160-9c20-89202b369431') {
  proc = basilisk::basiliskStart(bsklenv)
  on.exit(basilisk::basiliskStop(proc))
  basilisk::basiliskRun(proc, function(drs_uri) {
    drs_module = reticulate::import("terra_notebook_utils.drs")
    drs$access(drs_uri)
    }, drs_uri=drs_uri)
}
