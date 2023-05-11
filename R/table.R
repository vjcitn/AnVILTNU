table_validate <-
    function(namespace, name, ...)
{
    stopifnot(
        is_scalar_character(namespace),
        is_scalar_character(name),
        ...
    )
}

table_do <-
    function(module_function, ...)
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, function(...) {
        drs_module <- import("terra_notebook_utils.table")
        drs_module[[module_function]](...)
    }, ...)
}

#' @rdname table_list
#'
#' @title Data table commands through terra-notebook-utils
#'
#' @description `table_list()` lists all available tables from a provided
#'     workspace.
#'
#' @param namespace character(1) AnVIL workspace namespace; see `?tnu_workspace`
#'     for details and default value.
#'
#' @param name character(1) AnVIL workspace name; see `?tnu_workspace` for
#'     details and default value.
#' @return `table_list()` returns a character(1) list.
#' 
#' @examples
#' tnu_workspace("bioconductor-rpci-anvil/Bioconductor-Workflow-DESeq2")
#' table_list()
#'
#' @export
table_list <-
    function(
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name()
    )
{
    table_validate(
        namespace = namespace, name = name
    )

    ## but access this like drs_do
    tbl_generator <- table_do(
        "list_tables",
        workspace = name,
        workspace_namespace = namespace
    )

    tbl_names <- reticulate::iterate(tbl_generator)
    tbl_names
}


#' @rdname table_list
#'
#' @description `table_list_rows()` lists all rows from a table.
#'
#' @param table charater(1) AnVIL workspace table name.
#'
#' @return `table_list_rows()` returns a list of row information.
#'
#' @examples
#' tnu_workspace("bioconductor-rpci-anvil/Bioconductor-Workflow-DESeq2")
#' table_list_rows("participant")
#'
#' @export
table_list_rows <-
    function(
        table,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name()
    )
{
    table_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% table_list()
    )

    row_generator <- table_do(
        "list_rows",
        table = table,
        workspace = name,
        workspace_namespace = namespace
    )

    rows <- reticulate::iterate(row_generator)
    rows
}

#' @rdname table_list
#'
#' @description `table_delete()` deletes a table from the AnVIL workspace.
#'
#' @return `table_delete()` returns updated list of tables after deletion.
#'
#' @export
table_delete <-
    function(
        table,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name()
    )
{
    table_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% table_list()
    )

    table_do(
        "delete",
        table = table,
        workspace = name,
        workspace_namespace = namespace
    )

    tbls <- able_list()
    tbls
}

#' @rdname table_list
#'
#' @description `table_delete_row()` deletes a single (or multiple) rows from
#'     the AnVIL workspace table.
#'
#' @return `table_delete_row()` returns the list of rows after the deletion.
#'
#' @export
table_delete_row <-
    function(
        table,
        row_num,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name()
    )
{
    table_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% table_list()
    )

    if (length(row_num) > 1) {
        table_do(
            "del_row",
            table = table,
            item = row_num,
            workspace = name,
            workspace_namespace = namespace
        )
    } else {
        table_do(
            "del_rows",
            table = table,
            item = row_num,
            workspace = name,
            workspace_namespace = namespace
        )
    }

    tbl <- table_list_rows(table)
    tbl
}
## put_row(table: str, item: Union[Tuple[str, Dict[str, Union[str, int, float, bool, Iterable[Union[str, int, float, bool]]]]], Dict[str, Union[str, int, float, bool, Iterable[Union[str, int, float, bool]]]]], workspace: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None) -> str

## put_row("mytable", list(x = 1, y = "b", z = 2)) ???

## put_rows(table: str, items: Iterable[Union[Tuple[str, Dict[str, Union[str, int, float, bool, Iterable[Union[str, int, float, bool]]]]], Dict[str, Union[str, int, float, bool, Iterable[Union[str, int, float, bool]]]]]], workspace: Union[str, NoneType] = None, workspace_namespace: Union[str, NoneType] = None) -> List[str]
