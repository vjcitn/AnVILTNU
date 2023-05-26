is_scalar_character <-
    function(x)
{
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

is_scalar_integer <-
    function(x)
{
    is.numeric(x) && length(x) == 1L && !is.na(x)
}

is_character <-
    function(x)
{
    is.character(x) && !any(is.na(x)) && all(nzchar(x))
}

is_drs_uri <-
    function(x)
{
    is_character(x) && all(vapply(x, startsWith, logical(1), "drs://"))
}

is_tnu_workspace <-
    function(x)
{
    is_scalar_character(x) &&
        ## ?? not sure how to validate
        length(gregexpr("/", x)[[1]]) == 1L &&
        gregexpr("/", x)[[1]] != -1L

}
