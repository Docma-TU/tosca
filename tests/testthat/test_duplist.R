context("creating duplists")
test_that("duplist", {

    DL <- list(
        uniqueTexts = c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2", "d_IDRealDup1", "e_IDRealDup1"),
        notDuplicatedTexts = c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2"),
        idFakeDups = list(c = c("c_IDFakeDup1", "c_IDFakeDup2")),
        idRealDups = list(d = c("d_IDRealDup1", "d_IDRealDup2", "d_IDRealDup3"),
            e = c("e_IDRealDup1", "e_IDRealDup2")),
        allTextDups = list(c("d_IDRealDup1", "d_IDRealDup2", "d_IDRealDup3"), c("e_IDRealDup1", "e_IDRealDup2")),
        textMetaDups = list(c("e_IDRealDup1", "e_IDRealDup2"))
    )
    class(DL) <- "duplist"


########
    corpus <- textmeta(meta=data.frame(id=c("a", "a", "b", "c", "c", "d", "d", "d", "e1", "e2"),
                           title=c("a","a", "b", "c", "c", "d1", "d2", "d3", "e", "e"),
                           date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a="A",a="A", b="B", c="C1", c="C2", d="D", d="D", d="D", e="E", e="E"))
    corpus <- deleteAndRenameDuplicates(corpus)
    dl <- duplist(corpus, paragraph = FALSE)

    expect_equal(dl,DL)
    expect_true(is.duplist(dl))
    expect_false(is.duplist(pi))
########
    corpus <- textmeta(meta=data.frame(id=c("a", "a", "b", "c", "c", "d", "d", "d", "e1", "e2"),
                           title=c("a","a", "b", "c", "c", "d1", "d2", "d3", "e", "e"),
                           date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a=c("A","A2"),a=c("A","A2"), b="B", c="C1", c="C2", d="D", d="D", d="D", e="E", e="E"))
    corpus <- deleteAndRenameDuplicates(corpus, paragraph=TRUE)
    dl <- duplist(corpus, paragraph = TRUE)
    expect_equal(dl,DL)
########
    corpus <- textmeta(meta=data.frame(id=c("a", "b", "c"),
                           title=c("a", "b", "c"),
                           date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a="A", b="B", c="C"))
    corpus <- deleteAndRenameDuplicates(corpus, paragraph=TRUE)
    dl <- duplist(corpus, paragraph = TRUE)
    DL <- list(
        uniqueTexts = c("a", "b", "c"),
        notDuplicatedTexts = c("a", "b", "c"),
        idFakeDups = list(),
        idRealDups = list(),
        allTextDups = list(),
        textMetaDups = list()
    )
    class(DL) <- "duplist"
    expect_equal(dl,DL)
########
    corpus <- textmeta(meta=data.frame(id=c("a", "a", "c"),
                           title=c("a", "b", "c"),
                           date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a="A", a="B", c="C"))
    corpus <- deleteAndRenameDuplicates(corpus, paragraph=TRUE)
    dl <- duplist(corpus, paragraph = TRUE)
    DL <- list(
        uniqueTexts = c("a_IDFakeDup1", "a_IDFakeDup2", "c"),
        notDuplicatedTexts = c("a_IDFakeDup1", "a_IDFakeDup2", "c"),
        idFakeDups = list(a = c("a_IDFakeDup1", "a_IDFakeDup2")),
        idRealDups = list(),
        allTextDups = list(),
        textMetaDups = list()
    )
    class(DL) <- "duplist"
    expect_equal(dl,DL)

########
    a <- pi
    class(a) <- "duplist"
    expect_false(is.duplist(a))
    a <- list(a=pi, b=1)
    class(a) <- "duplist"
    expect_false(is.duplist(a))
    a <- list(uniqueTexts=NULL, notDuplicatedTexts=NULL, idFakeDups=NULL, idRealDups=NULL,
              allTextDups=NULL, textMetaDups=NULL)
    expect_false(is.duplist(a))

    print.duplist("a")
})
