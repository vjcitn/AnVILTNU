drs_validate <-
    function(drs_uri, namespace = NULL, name = NULL, ...)
{
    stopifnot(
        is_drs_uri(drs_uri),
        is.null(namespace) || is_scalar_character(namespace),
        is.null(name) || is_scalar_character(name),
        ...
    )
}

drs_do <-
    function(module_function, ...)
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, function(...) {
        drs_module <- import("terra_notebook_utils.drs")
        drs_module[[module_function]](...)
    }, ...)
}

#' @rdname drs
#'
#' @title Access DRS Through terra-notebook-utilities
#'
#' @description `drs_access()` acquires a signed URL from a DRS
#'     address.
#'
#' @param drs_uri character(1) DRS URI to be resolved.
#'
#' @param namespace character(1) AnVIL workspace namespace; see
#'     `?tnu_workspace` for details and default value.
#'
#' @param name character(1) AnVIL workspace name; see `?tnu_workspace`
#'     for detaials and default value.
#'
#' @param billing_project NULL or character(1) AnVIL billing project.
#'
#' @return `drs_access()` returns a character() vector of signed URLs.
#'
#' @importFrom reticulate import
#'
#' @examples
#' uri <- c(
#'     "drs://dg.4503/15fdd543-9875-4edf-8bc2-22985473dab6",
#'     "drs://dg.4503/3c861ec6-d810-4058-b851-c0b19dd5933e",
#'     "drs://dg.4503/374a0ad9-b3a2-47f3-8860-5083b302e478"
#' )
#' if (tnu_workspace_ok()) {
#'     print(drs_access(uri))
#'     print(drs_copy(uri, tempfile())) # local copy
#'     print(drs_info(uri))
#'     print(drs_head(uri, 100))        # first 100 bytes of each file
#' }
#'
#' @export
drs_access <-
    function(
        drs_uri,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name(),
        billing_project = NULL)
{
    drs_validate(
        drs_uri = drs_uri,
        namespace = namespace, name = name,
        is.null(billing_project) || is_scalar_character(billing_project)
    )

    vapply(drs_uri, function(uri, namespace, name, billing_project) {
        drs_do(
            "access", uri,
            workspace_name = name, workspace_namespace = namespace
            ## billing_project = billing_project
        )
    }, character(1), namespace, name, billing_project)
}

#' @rdname drs
#'
#' @description `drs_copy()` copies DRS URLS to the local file system
#'     or google bucket.
#'
#' @param destination character(1) directory or google bucket
#'     (starting with `gs://`) to which the DRS file will be copied.
#'
#' @return `drs_copy()` returns the file or bucket path(s) to the copied
#'     file(s).
#'
#' @export
drs_copy <-
    function(
     drs_uri, destination,
     namespace = tnu_workspace_namespace(),
     name = tnu_workspace_name())
{
    drs_validate(
        drs_uri = drs_uri,
        namespace = namespace, name = name,
        is_scalar_character(destination)
    )

    if (startsWith(destination, "gs://")) {
        .drs_copy_gs(drs_uri, destination, namespace, name)
    } else {
        ## FIXME: copy to local files result in an error
        .drs_copy_local(drs_uri, destination, namespace, name)
    }
}

.drs_copy_gs <-
    function(drs_uri, destination, namespace, name)
{
    drs_do(
        "copy_batch", drs_urls = drs_uri, dst_pfx = destination,
        workspace_namespace = namespace,
        workspace_name = name
    )
    names <- drs_info(drs_uri)$name
    file.path(destination, names)
}

#' @importFrom utils download.file
.drs_copy_local <-
    function(drs_uri, destination, namespace, name)
{
    if (!dir.exists(destination))
        dir.create(destination, recursive = TRUE)

    access_uri <- drs_access(drs_uri, namespace, name)
    info <- drs_info(drs_uri)
    result <- Map(function(access_uri, info, destination) {
        to_file <- file.path(destination, info$name)
        download.file(access_uri, to_file, quiet = interactive())
        md5sum <- tools::md5sum(to_file)
        if (!identical(as.vector(md5sum), info$checksums$md5)) {
            stop(
                "md5sum of '", access_uri, "' differs from expected:\n",
                "  observed: ", md5sum, "\n",
                "  expected: ", info$checksums$md5sum
            )
        }
        to_file
    }, access_uri, info, MoreArgs = list(destination = destination))
    unlist(result)
}

#' @rdname drs
#'
#' @description `drs_head()` queries the DRS object for the first few
#'     bytes of the object.
#'
#' @param num_bytes integer(1) the number of bytes to return from the
#'     'head' of th DRS object.
#'
#' @return `drs_head()` returns a list, with one element for each
#'     `drs_uri`, of Python 'memoryview' of the DRS object.
#'
#' @export
drs_head <-
    function(
        drs_uri,
        num_bytes = 1L,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name())
{
    num_bytes <- as.integer(num_bytes)
    drs_validate(
        drs_uri = drs_uri,
        namespace = namespace, name = name,
        is_scalar_integer(num_bytes),
        as.integer(num_bytes) > 0L
    )

    lapply(drs_uri, function(uri, num_bytes, namespace, name) {
        drs_do(
            "head", uri, num_bytes = num_bytes,
            workspace_name = name, workspace_namespace = namespace
        )
    }, num_bytes, namespace, name)
}

#' @rdname drs
#'
#' @description `drs_info()` returns a information about a DRS uri.
#'
#' @return `drs_info()` returns a list of lists (one element for each
#'     `drs_uri`) with elements
#'
#' - `name`: the name of the DRS file, e.g., `"HG00536.final.cram.crai"`
#' - `size`: file size in bytes; unfortunately, the size of large
#'   files (>3 Gb) is returned as a negative number (integer overflow).
#' - `updated`: date of last DRS resource modification.
#' - `checksums`: a vector of checksums, e.g, `md5`.
#'
#' @export
drs_info <-
    function(drs_uri)
{
    drs_validate(drs_uri = drs_uri)

    lapply(drs_uri, function(uri) {
        drs_do("info", uri)
    })
}
