context("creating duplists")
test_that("duplist", {
    
    DL <- list(
        uniqueTexts = c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2", "d_IDRealDup1", "e1"),
        notDuplicatedTexts = c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2"),
        idFakeDups = list(c = c("c_IDFakeDup1", "c_IDFakeDup2")),
        idRealDups = list(d = c("d_IDRealDup1", "d_IDRealDup2", "d_IDRealDup3")),
        allTextDups = list(c("d_IDRealDup1", "d_IDRealDup2", "d_IDRealDup3"), c("e1", "e2")),
        textMetaDups = list(c("e1", "e2"))
    )
    class(DL) <- "duplist"
    DL2 <- list(
        uniqueTexts = c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2", "d_IDRealDup1", "e_IDRealDup1"),
        notDuplicatedTexts = c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2"),
        idFakeDups = list(c = c("c_IDFakeDup1", "c_IDFakeDup2")),
        idRealDups = list(d = c("d_IDRealDup1", "d_IDRealDup2", "d_IDRealDup3"),
                          e = c("e_IDRealDup1", "e_IDRealDup2")),
        allTextDups = list(c("d_IDRealDup1", "d_IDRealDup2", "d_IDRealDup3"), c("e_IDRealDup1", "e_IDRealDup2")),
        textMetaDups = list(c("e1", "e2"))
    )
    class(DL2) <- "duplist"
    
    ########
    corpus <- textmeta(text=list(a="A",a="A", b="B", c="C1", c="C2", d="D", d="D", d="D", e1="E", e2="E"))
    corp <- deleteAndRenameDuplicates(corpus)
    expect_error(duplist(corp))
    expect_equivalent(names(corp$text), c("a", "b", "c_IDFakeDup1", "c_IDFakeDup2", "d", "e1", "e2"))
    
    ########
    corpus <- textmeta(meta=data.frame(id=c("a", "a", "b", "c", "c", "d", "d", "d", "e1", "e2"),
                                       title=c("a","a", "b", "c", "c", "d1", "d2", "d3", "e", "e"),
                                       date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a="A",a="A", b="B", c="C1", c="C2", d="D", d="D", d="D", e="E", e="E"))
    expect_error(corpus <- deleteAndRenameDuplicates(corpus))
    corpus <- textmeta(meta=data.frame(id=c("a", "a", "b", "c", "c", "d", "d", "d", "e1", "e2"),
                                       title=c("a","a", "b", "c", "c", "d1", "d2", "d3", "e", "e"),
                                       date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a="A",a="A", b="B", c="C1", c="C2", d="D", d="D", d="D", e1="E", e2="E"))
    corp <- deleteAndRenameDuplicates(corpus)
    dl <- duplist(corp, paragraph = FALSE)
    
    expect_equal(dl,DL)
    expect_equal(dl,duplist(deleteAndRenameDuplicates(corpus, renameRemaining = FALSE)))
    expect_true(is.duplist(dl))
    expect_false(is.duplist(pi))
    ########
    corpus <- textmeta(meta=data.frame(id=c("a", "a", "b", "c", "c", "d", "d", "d", "e1", "e2"),
                                       title=c("a","a", "b", "c", "c", "d1", "d2", "d3", "e", "e"),
                                       date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a=c("A","A2"),a=c("A","A2"), b="B", c="C1", c="C2", d="D", d="D", d="D", e1="E", e2="E"))
    corpus <- deleteAndRenameDuplicates(corpus)
    dl <- duplist(corpus, paragraph = TRUE)
    expect_equal(dl,DL)
    ########
    corpus <- textmeta(meta=data.frame(id=c("a", "b", "c"),
                                       title=c("a", "b", "c"),
                                       date="2018-01-01", stringsAsFactors=FALSE),
                       text=list(a="A", b="B", c="C"))
    corpus <- deleteAndRenameDuplicates(corpus)
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
    corpus <- deleteAndRenameDuplicates(corpus)
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
    corpus = textmeta(
        meta = data.frame(
            id = c("d", "a", "a", "d", "d", "d"),
            title = c("d3", "a","a", "d1", "d2", "d3"),
            date = "2018-01-01",
            stringsAsFactors = FALSE),
        text = list(d="", a="A", a="", d="D", d="D", d=""))
    corp1 = deleteAndRenameDuplicates(corpus)
    corpus = textmeta(
        meta = data.frame(
            id = c("d", "a", "a", "d", "d"),
            title = c("d3", "a","a", "d1", "d2"),
            date = "2018-01-01",
            stringsAsFactors = FALSE),
        text = list(d="", a="A", a="", d="D", d="D"))
    corp2 = deleteAndRenameDuplicates(corpus)
    expect_identical(corp1, corp2)
    expect_true(all(corp1$meta$id == c("d_IDFakeDup1", "a_IDFakeDup1", "a_IDFakeDup2",
                                       "d_IDRealDup1", "d_IDRealDup2")))
    
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
