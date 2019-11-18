test_that("correct base packages", {
    basepkgs$Version <- as.character(getRversion())
    rpkgs0 <- getRpkgs()
    rpkgs <- rpkgs0[!is.na(rpkgs0$Priority) & rpkgs0$Priority=='base',]
    expect_equal(basepkgs[order(basepkgs$Package), ], rpkgs[order(rpkgs$Package),])
})
