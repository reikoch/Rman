test_that("correct base packages", {
    basepkgs$Version <- as.character(getRversion())
    rpkgs <- getRpkgs()
    expect_equal(basepkgs, rpkgs[!is.na(rpkgs$Priority) & rpkgs$Priority=='base',])
})
