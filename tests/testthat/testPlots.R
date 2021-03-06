context('test plots.R')

test_that("test plot.kwic scale argument default", {

    sda <- kwic(texts(data_corpus_inaugural)[[1]], 'american')
    sdp <- kwic(texts(data_corpus_inaugural)[[1]], 'people')
    mda <- kwic(data_corpus_inaugural, 'american')
    mdp <- kwic(data_corpus_inaugural, 'people')

    # Single document, should be absolute
    p <- plot(sda)
    expect_equal(p$labels$x, 'Token index')

    # Single document, multiple keywords, should be absolute
    p <- plot(sda, sdp)
    expect_equal(p$labels$x, 'Token index')

    # Multiple documents, should be relative
    p <- plot(mda)
    expect_equal(p$labels$x, 'Relative token index')

    # Multiple documents, multiple keywords, should be relative
    p <- plot(mda, mdp)
    expect_equal(p$labels$x, 'Relative token index')

    # Explicit overrides
    p <- plot(sda, scale='absolute')
    expect_equal(p$labels$x, 'Token index')
    p <- plot(sda, sdp, scale='absolute')
    expect_equal(p$labels$x, 'Token index')
    p <- plot(mda, scale='absolute')
    expect_equal(p$labels$x, 'Token index')
    p <- plot(mda, mdp, scale='absolute')
    expect_equal(p$labels$x, 'Token index')

    p <- plot(sda, scale='relative')
    expect_equal(p$labels$x, 'Relative token index')
    p <- plot(sda, sdp, scale='relative')
    expect_equal(p$labels$x, 'Relative token index')
    p <- plot(mda, scale='relative')
    expect_equal(p$labels$x, 'Relative token index')
    p <- plot(mda, mdp, scale='relative')
    expect_equal(p$labels$x, 'Relative token index')


})


test_that("test plot.kwic facet order parameter", {

    p <- plot(kwic(data_corpus_inaugural, 'american'), sort=TRUE)
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    expect_true(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )

    p <- plot(kwic(data_corpus_inaugural, 'american'), kwic(data_corpus_inaugural, 'people'), sort=TRUE)
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    expect_true(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )

    # Default should be false
    #TODO Fix this
    p <- plot(kwic(data_corpus_inaugural, 'american'), kwic(data_corpus_inaugural, 'people'))
    plot_docnames <- as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$docname))
    expect_false(
        all(
            plot_docnames[order(plot_docnames)] == plot_docnames
        )
    )


})

test_that("test plot.kwic keeps order of keywords passed", {
    p <- plot(kwic(data_corpus_inaugural, 'people'), kwic(data_corpus_inaugural, 'american'), sort=TRUE)
    expect_equal(
        as.character(unique(ggplot2::ggplot_build(p)$layout$panel_layout$keyword)),
        c('people', 'american')
    )
})
