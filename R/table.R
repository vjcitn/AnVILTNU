table_validate <-
    function(namespace, name, ...)
{
    stopifnot(
        is_scalar_character(namespace),
        is_scalar_character(name),
        ...
    )
}

#tnu_do(module, fun) #not exported, called within drs_*, table_* (add to tnu_utilities?)

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
    tbl_generator <- tnu_do(
        "table",
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

    row_generator <- tnu_do(
        "table",
        "list_rows",
        table = table,
        workspace = name,
        workspace_namespace = namespace
    )

    rows <- reticulate::iterate(row_generator)
   

    ## could this function go in above iterate as f? 
    row_dat <- lapply(rows, function(row) {
        row_id <- list(row$name)
        names(row_id) <- paste0(table,"_id")       
        #cbind(as.data.frame(row_id), row$attributes)
        c(row_id, row$attributes)
    })

    row_dat
    #dplyr::bind_rows(row_dat)
}

#' @rdname table_list
#'
#' @description `table_delete()` deletes a table from the AnVIL workspace.
#'
#' @param dry.run A boolean to indicate if the deletion should happen. The 
#'     default is TRUE, meaning the deletion will not be executed.
#'
#' @return None
#'
#' @export
table_delete <-
    function(
        table,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name(),
        dry.run = TRUE
    )
{
    table_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% table_list()
    )

    if (dry.run) {
        m <- paste0("The ",
            table, 
            " table will be deleted. If correct, rerun with dry.run = FALSE.")
        message(m)
    } else {
        tnu_do(
            "table",
            "delete",
            table = table,
            workspace = name,
            workspace_namespace = namespace
        )
    }
}

#' @rdname table_list
#'
#' @description `table_delete_row()` deletes a single (or multiple) rows from
#'     the AnVIL workspace table.
#'
#' @param row_id character(1) The row id(s) associated with the row(s) that 
#'     are to be deleted.
#'
#' @return `table_delete_row()` returns the name of the table that had the 
#'     row(s) deleted from.
#'
#' @export
table_delete_row <-
    function(
        table,
        row_id,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name(),
        dry.run = TRUE
    )
{
    table_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% table_list()
    )

    if (dry.run) {
        m <- paste0("Row(s) will be deleted from the ",
            table,
            " table. If correct, rerun with dry.run = FALSE.")
        message(m)
    } else {
        if (length(row_id) > 1) {
            tnu_do(
                "table",
                "del_rows",
                table = table,
                items = row_id,
                workspace = name,
                workspace_namespace = namespace
            )
        } else {
            tnu_do(
                "table",
                "del_row",
                table = table,
                item = row_id,
                workspace = name,
                workspace_namespace = namespace
            )
        }
    }
    
    table    
}

#' @rdname table_list
#'
#' @description `table_put_row()` adds a single (or multiple) rows to the AnVIL
#'     workspace table.
#'
#' @return `table_put_row()` returns the id(s) of the row(s) added.
#'
#' @examples
#'
#' @export
table_put_row <-
    function(
        table,
        item,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name()
    )
{
    table_validate(
        namespace = namespace, name = name,
        is_scalar_character(table)
    )

    if (any(lengths(item) > 1)) {
        tnu_do(
            "table",
            "put_rows",
            table = table,
            items = item,
            workspace = name,
            workspace_namespace = namespace
        )
    } else {
        tnu_do(
            "table",
            "put_row",
            table = table,
            item = item,
            workspace = name,
            workspace_namespace = namespace
        )
    }
}
