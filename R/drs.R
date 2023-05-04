drs_validate <-
    function(drs_uri, namespace, name, ...)
{
    stopifnot(
        is_drs_uri(drs_uri),
        is_scalar_character(namespace),
        is_scalar_character(name),
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
#' @title Access DRS through terra-notebook-utilities
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
#' @return `drs_access()` returns a character(1) signed URL
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

    drs_do(
        "access", drs_uri,
        workspace_name = name, workspace_namespace = namespace
        #billing_project = billing_project
    )
}

#' @rdname drs
#'
#' @description `drs_copy()` copies a DRS url or to the local file
#'     system. This can be a single url or multiple.
#'
#' @param destination character(1) directory to which the DRS file
#'     will be copied.
#'
#' @return `drs_copy()` returns the local file path(s) to the copied
#'     file(s).
#'
#' @examples
#' if (tnu_workspace_ok()) {
#'    destination <- tempfile()
#'    drs_copy(uri, destination)
#' }
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
    if (!dir.exists(destination))
        dir.create(destination, recursive = TRUE)
    ## FIXME what if the file already exists in `destination`?

    if (length(drs_uri) == 1) {
        drs_do(
            "copy", drs_uri, dst = destination,
            workspace_namespace = namespace,
            workspace_name = name
        )
    } else {
        drs_do(
            "copy_batch", drs_urls = drs_uri, dst_pfx = destination,
            workspace_namespace = namespace,
            workspace_name = name
        )
    }
}

#' @rdname drs
#'
#' @description `drs_head()` heads a DRS object by byte.
#'
#' @return `drs_head()` returns
#'
#' @examples
#' if (tnu_workspace_ok()) {
#'    drs_head(uri)
#' }
#'
#' @export 
drs_head <-
    function(
        drs_uri,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name())
{
    drs_validate(
        drs_uri = drs_uri,
        namespace = namespace, name = name,
    )

    drs_do(
        "head", drs_uri,
        workspace_names = name, workspace_namespace = namespace
    )
}

#' @rdname drs
#'
#' @description `drs_info()` returns a curated subset of data from a DRS url.
#'
#' @return `drs_info()` returns a curated subset.
#'
#' @examples
#' if (tnu_workspace_ok()) {
#'    drs_info(uri)
#' }
#'
#' @export 
drs_info <-
    function(drs_uri)
{
    drs_validate(drs_uri = drs_uri)

    drs_do("info", drs_uri)
}

## possible functions to implement: copy, copy_batch,
##    copy_batch_manifest, copy_batch_urls(?), copy_to_bucket,
##    get_drs, get_drs_info

## tnu_drs_help()
##
##     access(drs_url: str, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None, billing_project: Union[str, NoneType] = None) -> str

##         Return a signed url for a drs:// URI, if available.


##     blob_for_url(url: str, billing_project: Union[str, NoneType] = None) -> terra_notebook_utils.blobstore.Blob


##     copy(drs_uri: str, dst: str, indicator_type: terra_notebook_utils.blobstore.progress.Indicator = <Indicator.bar: <class 'getm.progress.ProgressBar'>>, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None)
##         Copy a DRS object to either the local filesystem, or to a
##         Google Storage location if `dst` starts with "gs://".


##     copy_batch(drs_urls: Union[Iterable[str], NoneType] = None, dst_pfx: Union[str, NoneType] = None, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None, indicator_type: terra_notebook_utils.blobstore.progress.Indicator = <Indicator.log: <class 'getm.progress.ProgressLogger'>>, manifest: Union[List[Dict[str, str]], NoneType] = None)

##     copy_batch_manifest(manifest: List[Dict[str, str]], indicator_type: terra_notebook_utils.blobstore.progress.Indicator = <Indicator.log: <class 'getm.progress.ProgressLogger'>>, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None)

##     copy_batch_urls(drs_urls: Iterable[str], dst_pfx: str, indicator_type: terra_notebook_utils.blobstore.progress.Indicator = <Indicator.log: <class 'getm.progress.ProgressLogger'>>, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None)

##     copy_to_bucket(drs_uri: str, dst_key: str = '', dst_bucket_name: Union[str, NoneType] = None, indicator_type: terra_notebook_utils.blobstore.progress.Indicator = <Indicator.bar: <class 'getm.progress.ProgressBar'>>, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None)
##         Resolve `drs_url` and copy into user-specified bucket
##         `dst_bucket`.  If `dst_bucket` is None, copy into workspace
##         bucket.

##     enable_requester_pays(workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None)

##     extract_tar_gz(drs_url: str, dst: Union[str, NoneType] = None, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None, billing_project: Union[str, NoneType] = None)
##
##         Extract a `.tar.gz` archive resolved by a DRS url. 'dst'
##         may be either a local filepath or a 'gs://' url.  Default
##         extraction is to the bucket for 'workspace'.

##     get_drs(drs_url: str, fields: List[str]) -> requests.models.Response
##         Request DRS information from martha.

##     get_drs_blob(drs_url_or_info: Union[str, terra_notebook_utils.drs.DRSInfo], billing_project: Union[str, NoneType] = None) -> Union[terra_notebook_utils.blobstore.gs.GSBlob, terra_notebook_utils.blobstore.url.URLBlob]

##     get_drs_info(drs_url: str, access_url: bool = False) -> terra_notebook_utils.drs.DRSInfo
##
##         Attempt to resolve gs:// url and credentials for a DRS
##         object.

##     head(drs_url: str, num_bytes: int = 1, workspace_name: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None, billing_project: Union[str, NoneType] = None)
##         Head a DRS object by byte.

##     info(drs_url: str) -> dict
## Return a curated subset of data from `get_drs`.
