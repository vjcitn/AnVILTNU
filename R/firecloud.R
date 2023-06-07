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

jmesquery <-
    function(response, query)
{
    response$text |>
        rjsoncons::jmespath(query) |>
        jsonlite::fromJSON()
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
#'         dplyr::as_tibble(rownames = "model_id") |>
#'         dplyr::mutate(`entity:model_id` = gsub(" ", "_", `entity:model_id`))
#'     table_upload(mycars)
#'     table_delete_values("model", "Mazda_RX4")
#'     table_gadget()
#' }
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
        workspace = name)
    #response$text |> listviewer::jsonedit()
    response$raise_for_status()

    dplyr::tibble(
        table = jmesquery(response, "keys(@)"),
        count = jmesquery(response, "*.count"),
        colnames = paste0(jmesquery(response, "*.idName"), ", ",
            sapply(jmesquery(response, "*.attributeNames"), toString))
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
        dplyr::as_tibble() |>
        dplyr::rename_with(~ gsub("entity.", "", .x), starts_with("entity"))
}

#' @rdname firecloud
#'
#' @description `table_delete_values()` deletes row from the table.
#'
#' @param values character(1) The '*_id' of the row that should be deleted from
#'     the table.
#'
#' @return `table_delete_values()` returns the name of the table that had the
#'     row deleted from.
#'
#' @export 
table_delete_values <-
    function(
        table,
        values, 
        namespace = tnu_workspace_namespace(),
        name = tnu_workspace_name(), 
        dry.run = TRUE)
{
    firecloud_validate(
        namespace = namespace, name = name,
        is_scalar_character(table), table %in% tables()[[1]],
        is_scalar_character(values), values %in% table(table)[[1]]
    )

    if (dry.run) {
        message(paste0(values, " will be deleted from ", table, ". Re-run with dry.run = FALSE"))
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

    if (!startsWith("entity:", colnames(tbl)[1]))
        colnames(tbl)[1] <- paste0('entity:', colnames(tbl)[1])

    if (length(grep(" ", tbl[[1]])) > 0L)
        tbl[[1]] <- gsub(" ", "_", tbl[[1]])

    write.table(tbl, where <- tempfile(), sep = "\t", row.names = FALSE)

    response <- firecloud_do(
        "upload_entities_tsv",
        namespace = namespace,
        workspace = name,
        entities_tsv = where,
        model = "flexible"
    )
    response$raise_for_status()
}


.gadget_ui <-
    function(title)
{
    force(title)
    function() {
        miniPage(p(strong("Current workspace:"), textOutput("workspace",
            inline = TRUE)), miniContentPanel(DTOutput("gadget_tibble",
            height = "100%")), gadgetTitleBar(title))
    }
}

.gadget_renderDT <-
    function(tbl)
{
    force(tbl)
    renderDT(formatStyle(datatable(tbl, fillContainer = TRUE,
        selection = list(mode = "single", target = "row"), options = list(dom = "ftp")),
        seq_len(NROW(tbl) + 1L) - 1L, `vertical-align` = "top"))
}

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

.gadget_run <-
    function(title, tibble, DONE_FUN)
{
    stopifnot(is_scalar_character(title), is.data.frame(tibble))
    suppressMessages({
        runGadget(.gadget_ui(title), .gadget_server(tibble, DONE_FUN),
            stopOnCancel = FALSE)
    })
}

#' @rdname firecloud
#'
#' @description `table_gadget()` displays tables in the workspace and can be 
#'     returned as a tibble.
#'
#' @return `table_gadget()` returns a tibble representing the selected table.
#'
#' @export
table_gadget <- 
    function(namespace = tnu_workspace_namespace(), name = tnu_workspace_name())
{
    DONE_FUN <- function(tibble, row_selected) tibble$table[row_selected]
    table <- .gadget_run("Tables", tables(namespace, name), DONE_FUN)
    if (length(table)) {
        table(table, namespace, name)
    }
    else {
        invisible()
    }
}
