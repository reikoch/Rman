test_that("correct base packages", {
    basepkgs <- baserecpkgs[ baserecpkgs$Priority=='base',]
    rpkgs0 <- getRpkgs()
    rpkgs <- rpkgs0[!is.na(rpkgs0$Priority) & rpkgs0$Priority=='base', c('Package', 'Priority')]
    expect_equal(basepkgs[order(basepkgs$Package), ], rpkgs[order(rpkgs$Package),])
})
