test_that("'is_drs_uri()' works", {
    drs_uri <- "drs://dg.4503:dg.4503/17141a26-90e5-4160-9c20-89202b369431"
    expect_true(is_drs_uri(drs_uri))

    expect_false(is_drs_uri("https://foo.bar"))
    ## no NA, "", length != 1L
    expect_false(is_drs_uri(NA_character_))
    expect_false(is_drs_uri(""))
    expect_true(is_drs_uri(character()))
    expect_true(is_drs_uri(c(drs_uri, drs_uri)))
})

test_that("'is_tnu_workspace()' works", {
    expect_true(is_tnu_workspace("foo/bar"))

    expect_false(is_tnu_workspace("foo"))
    expect_false(is_tnu_workspace("foo/bar/baz"))
    ## no NA, "", length != 1L
    expect_false(is_tnu_workspace(NA_character_))
    expect_false(is_tnu_workspace(""))
    expect_false(is_tnu_workspace(character()))
    expect_false(is_tnu_workspace(c("foo/bar", "bar/baz")))
})
