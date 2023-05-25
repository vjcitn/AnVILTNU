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

## Document/export
tables <-
    function(namespace = tnu_workspace_namespace(), name = tnu_workspace_name())
{
    response <- firecloud_do(
        "list_entity_types",
        namespace = namespace,
        workspace = name)
    #response$text |> listviewer::jsonedit()
    response$raise_for_status()

    dplyr::tibble(
        table = jmesquery(response, "keys(@)"),
        count = jmesquery(response, "*.count"),
        colnames = paste(c(jmesquery(response, "*.idName"),
            jmesquery(response, "*.attributeNames")), collapse = ", ")
    )
}

## Document/export
table <-
    function(table, namespace, name)
{
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

## Document/export/test
table_delete_values <-
    function(table, values, namespace, name, dry.run = TRUE)
{
    if (dry.run) {
        message(paste0(values, "will be deleted from", table, "table. Re-run with dry.run = FALSE"))
    } else {
        response <- firecloud_do(
            "delete_entity_type",
            namespace = namespace,
            workspace = name,
            etype = table,
            ename = values
        )
        response$raise_for_status()
    }
}

## table_gadget <- function(
