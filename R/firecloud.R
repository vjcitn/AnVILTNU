## will change when tableFunctionality branch is merged over
## this function is now generalized until tnu_utilities
firecloud_do <-
    function(module_function, ...)
{
    proc <- basiliskStart(bsklenv)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, function(...) {
        module <- import("firecloud.api")
        module[[module_function]](...)
    }, ...)
}

#' @importFrom jsonlite fromJSON
#'
#' @importFrom rjsoncons jmespath
jmesquery <-
    function(response, query)
{
    response$text |>
        jmespath(query) |>
        fromJSON()
}

firecloud_validate <-
    function(namespace, name, ...)
{
    stopifnot(
        is_scalar_character(namespace),
        is_scalar_character(name),
        ...
    )
}

#' @rdname firecloud
#'
#' @title Data table commands through firecloud
#'
#' @description `tables()` lists the available tables in a workspace with the
#'     corresponding row count and column names.
#'
#' @param namespace character(1) AnVIL workspace namespace; see `?tnu_workspace`
#'     for details and default value.
#'
#' @param name character(1) AnVIL workspace name; see `?tnu_workspace` for
#'     details and default value.
#'
#' @return `tables()` returns a tibble.
#'
#' @examples
#' if (tnu_workspace_ok()) {
#'     tables()
#'     table("test_table")
#'     mycars <- head(mtcars) |>
#'         dplyr::as_tibble(rownames = "model_id")
#'     table_upload(mycars)
#'     table_delete_values("model", "Mazda_RX4")
#'     tables_gadget()
#' }
#'
#' @importFrom dplyr tibble as_tibble rename_with starts_with
#'
#' @export
tables <-
    function(
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name())
{
    firecloud_validate(
        namespace = namespace, name = name
    )

    response <- firecloud_do(
        "list_entity_types",
        namespace = namespace,
        workspace = name
    )
    response$raise_for_status()

    clmns <- Map(c,
        jmesquery(response, "*.idName"),
        jmesquery(response, "*.attributeNames")
    )

    tibble(
        table = jmesquery(response, "keys(@)"),
        n_row = jmesquery(response, "*.count"),
        n_col = lengths(clmns, use.names = FALSE),
        colnames = vapply(unname(clmns), toString, character(1))
    )
}

#' @rdname firecloud
#'
#' @description `table()` returns a table.
#'
#' @param table character(1) table name.
#'
#' @return `table()` a tibble of data from the table.
#'
#' @importFrom utils read.delim write.table
#'
#' @export
table <-
    function(
        table,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name())
{
    firecloud_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% tables()$table
    )

    response <- firecloud_do(
        "get_entities_tsv",
        namespace = namespace,
        workspace = name,
        etype = table,
        model = "flexible"
    )
    response$raise_for_status()

    read.delim(text = response$text, sep = "\t", header = TRUE) |>
        as_tibble() |>
        rename_with(~ gsub("entity.", "", .x), starts_with("entity"))
}

#' @rdname firecloud
#'
#' @description `table_delete_values()` deletes row from the table.
#'
#' @param values character(1) The '*_id' of the row that should be deleted from
#'     the table.
#'
#' @param dry.run logical(1) When TRUE (default) report the action to
#'     be performed, without actually doing the action.
#'
#' @return `table_delete_values()` returns the name of the table that had the
#'     row deleted from.
#'
#' @export
table_delete_values <-
    function(
        table,
        values,
        dry.run = TRUE,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name())
{
    firecloud_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% tables()[[1]],
        is_scalar_character(values), values %in% table(table)[[1]],
        is_scalar_logical(dry.run)
    )

    if (dry.run) {
        message(
            "'", values, "' will be deleted from '", table, "'. ",
            "Re-run with dry.run = FALSE"
        )
    } else {
        response <- firecloud_do(
            "delete_entity_type",
            namespace = namespace,
            workspace = name,
            etype = table,
            ename = values
        )
        response$raise_for_status()
        table
    }
}

.find_entity <-
    function(tbl)
{
    ## Test for entity: in any of the columns
    ## Test for _id in any of the columns
    ## If neither, assume first column is id column
    if (any(grepl("entity:", colnames(tbl)))) {
        entity_col <- grep("entity:", colnames(tbl))
    } else if (any(grepl("_id", colnames(tbl)))) {
        id_col <- grep("_id", colnames(tbl))
        colnames(tbl)[id_col] <- paste0('entity:', colnames(tbl)[id_col])
        entity_col <- id_col
    } else {
        colnames(tbl)[1] <- paste0('entity:', colnames(tbl)[1])
        entity_col <- 1
    }

    tbl[[entity_col]] <- gsub(" ", "_", tbl[[entity_col]])
    tbl
}

#' @rdname firecloud
#'
#' @description `table_upload()` uploads a table to the AnVIL workspace.
#'
#' @param tbl A tibble to uploaded as a data table to the workspace.
#'
#' @return NULL
#'
#' @export
table_upload <-
    function(
        tbl,
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name())
{
    firecloud_validate(
        namespace = namespace, name = name
    )

    entity_tbl <- .find_entity(tbl)

    write.table(entity_tbl, where <- tempfile(), sep = "\t", row.names = FALSE)

    response <- firecloud_do(
        "upload_entities_tsv",
        namespace = namespace,
        workspace = name,
        entities_tsv = where,
        model = "flexible"
    )
    response$raise_for_status()
}

#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar
#'
#' @importFrom DT DTOutput
.gadget_ui <-
    function(title)
{
    force(title)
    function() {
        miniPage(
            gadgetTitleBar(title),
            miniContentPanel(
                p(
                    "Select the table of interest. Click 'Done' to import ",
                    "into R."
                ),
                DTOutput("gadget_tibble", height = "100%"),
                p("Workspace:", textOutput("workspace", inline = TRUE))
            )
        )
    }
}

#' @importFrom DT renderDT formatStyle datatable
#'
#' @importFrom htmlwidgets JS
.gadget_renderDT <-
    function(tbl)
{
    force(tbl)
    renderDT(formatStyle(
        datatable(
            tbl, fillContainer = TRUE,
            selection = list(mode = "single", target = "row"),
            options = list(
                dom = "ftp",
                columnDefs = list(list(
                    ## truncate column 4 (column names) to 70 characters...
                    targets = 4,
                    render = JS(
                        "function(data, type, row, meta) {",
                        "return type === 'display' && data.length > 70 ?",
                        "'<span title=\"' + data + '\">' + data.substr(0, 67) + '...</span>' : data;",
                        "}")
                ))
            )
        ),
        seq_len(NROW(tbl) + 1L) - 1L, `vertical-align` = "top"
    ))
}

#' @importFrom shiny renderText observeEvent stopApp p strong textOutput
.gadget_server <-
    function(tibble, DONE_FUN)
{
    force(tibble)
    force(DONE_FUN)
    function(input, output, session) {
        output$workspace <- renderText({
            if (nzchar(tnu_workspace_name())) {
                tnu_workspace()
            }
        })
        output$gadget_tibble <- .gadget_renderDT(tibble)
        observeEvent(input$done, {
            row_selected <- input$gadget_tibble_rows_selected
            if (is.integer(row_selected)) {
                returnValue <- DONE_FUN(tibble, row_selected)
            }
            else {
                returnValue <- character()
            }
            stopApp(returnValue)
        })
        observeEvent(input$cancel, {
            stopApp(NULL)
        })
    }
}

#' @importFrom shiny runGadget
.gadget_run <-
    function(title, tibble, DONE_FUN)
{
    stopifnot(is_scalar_character(title), is.data.frame(tibble))
    suppressMessages({
        runGadget(
            .gadget_ui(title),
            .gadget_server(tibble, DONE_FUN),
            stopOnCancel = FALSE
        )
    })
}

#' @rdname firecloud
#'
#' @description `tables_gadget()` provides a simple graphical interface
#'     to allow selection of a table from the workspace.
#'
#' @return `tables_gadget()` returns a tibble representing the selected table.
#'
#' @importFrom dplyr mutate
#'
#' @export
tables_gadget <-
    function(namespace = tnu_workspace_namespace(), name = tnu_workspace_name())
{
    firecloud_validate(namespace, name)
    DONE_FUN <- function(tibble, row_selected)
        tibble$table[row_selected]
    title <- paste0("Tables in ", tnu_workspace())
    table <- .gadget_run(title, tables(namespace, name), DONE_FUN)
    if (length(table)) {
        table(table, namespace, name)
    } else {
        invisible()
    }
}
