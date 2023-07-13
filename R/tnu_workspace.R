BIOC_TNU <- new.env(parent = emptyenv())

#' @rdname tnu_workspace
#'
#' @title Manage workspace namespace and name needed by terra-notebook-utils
#'
#' @description `tnu_workspace()` gets (when `x = NULL`) or sets and
#'     returns the namespace and name in the form `NAMESPACE/NAME`.
#'
#' @param x NULL (get the existing value) or character(1) workspace
#'     namespace and name, formatted as `NAMESPACE/NAME`.
#'
#' @param is_google_cloud logical(1) is the workspace on the google
#'     cloud? If so, ensure that the environment variable
#'     `WORKSPACE_BUCKET` is set, as this is used by
#'     terra-notebook-utils during authentication.
#'
#' @details `tnu_workspace()` with no argument checks to see if the
#'     workspace namespace and name have been assigned through a
#'     previous call to `tnu_workspace()`, or are defined by the
#'     package options `AnVILTNU.WORKSPACE_NAMESPACE`,
#'     `AnVILTNU.WORKSPACE_NAME` (set via
#'     `options(AnVILTNU.WORKSPACE_NAME = ...)`), or by the system
#'     environment variables `WORKSPACE_NAMESPACE`,
#'     `WORKSPACE_NAME`. Calls to `tnu_workspace()` have precedence
#'     over package options, which have precedence over system
#'     environment variables.
#'
#' @examples
#' if (tnu_workspace_ok()) {
#'     tnu_workspace()
#' }
#'
#' @export
tnu_workspace <-
    function(x = NULL, is_google_cloud = TRUE)
{
    stopifnot(is.null(x) || is_tnu_workspace(x))
    if (is.null(x)) {
        workspace <- tnu_workspace_get()
    } else {
        workspace <- tnu_workspace_set(x, is_google_cloud)
    }
    workspace
}

tnu_workspace_namespace <-
    function()
{
    workspace <- tnu_workspace_get()
    sub("/.*", "", workspace)
}

tnu_workspace_name <-
    function()
{
    workspace <- tnu_workspace_get()
    sub(".*/", "", workspace)
}

tnu_workspace_get <-
    function()
{
    workspace <- BIOC_TNU[["tnu_workspace"]]
    if (!is.null(workspace))
        return(workspace)
    ## read workspace namespace and name from the environment, then
    ## options
    namespace <- Sys.getenv("WORKSPACE_NAMESPACE", NA_character_)
    namespace <- getOption("AnVILTNU.WORKSPACE_NAMESPACE", namespace)
    name <- Sys.getenv("WORKSPACE_NAME", NA_character_)
    name <- getOption("AnVILTNU.WORKSPACE_NAME", name)

    if (is.na(name) || is.na(namespace)) {
        txt <- paste0(
            "could not determine AnVIL workspace namespace and name, ",
            "see ?tnu_workspace"
        )
        stop(paste0(strwrap(txt, exdent = 2), collapse = "\n"))
    }

    BIOC_TNU[["tnu_workspace"]] <- paste0(namespace, "/", name)

    ## return the workspace namespace/name
    BIOC_TNU[["tnu_workspace"]]
}

tnu_workspace_set <-
    function(x, is_google_cloud)
{
    BIOC_TNU[["tnu_workspace"]] <- x

    if (is_google_cloud)
        Sys.setenv("WORKSPACE_BUCKET" = AnVIL::avbucket(
            tnu_workspace_namespace(),
            tnu_workspace_name()
        ))

    ## return the workspace namespace/name
    BIOC_TNU[["tnu_workspace"]]
}

#' @rdname tnu_workspace
#'
#' @description `tnu_workspace_ok()` returns TRUE when the workspace
#'     namespace and name have been set by `tnu_workspace()`, package
#'     options, or system environment variables.
#'
#' @examples
#' tnu_workspace_ok()
#'
#' @export
tnu_workspace_ok <-
    function()
{
    !is.null(BIOC_TNU[["tnu_workspace"]])
}
