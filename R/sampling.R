#' Sample Texts
#'
#' Sample texts from different subsets to minimize variance of the recall estimator
#' 
#' @param id Character: IDs of all texts in the corpus.
#' @param corporaID List of Character: Each list element is a character vector and
#' contains the IDs belonging to one subcorpus. Each ID has to be in \code{id}.
#' @param label Named Logical: Labeling result for already labeled texts. Could be empty, if no labeled data exists. The algorithm sets \code{p = 0.5} for all intersections.
#' Names have to be \code{id}.
#' @param m Integer: Number of new samples.
#' @param randomize Logical: If \code{TRUE} calculated split is used as parameter
#' to draw from a multinomial distribution.
#' @param exact Logical: If \code{TRUE} exact calculation is used. For the default
#' \code{FALSE} an approximation is used.
#' @return Character vector of IDs, which should be labeled next.
#' @examples
#' id <- paste0("ID", 1:1000)
#' corporaID <- list(sample(id, 300), sample(id, 100), sample(id, 700))
#' label <- sample(as.logical(0:1), 150, replace=TRUE)
#' names(label) <- c(sample(id, 100), sample(corporaID[[2]], 50))
#' m <- 100
#' sampling(id, corporaID, label, m)
#' @export sampling

sampling <- function(id, corporaID, label, m, randomize = FALSE, exact = FALSE){
  if(missing(label)){label <- logical()}
  stopifnot(is.character(id), is.list(corporaID), all(sapply(corporaID, is.character)),
    all(unique(unlist(corporaID)) %in% id),
    all(names(label) %in% id), is.logical(label), is.integer(as.integer(m)), length(m)==1,
    length(randomize)==1, is.logical(randomize), length(exact)==1, is.logical(exact)
  )  
  
  intersections <- sapply(corporaID, function(x) id %in% x)
  intlabelID <- apply(intersections, 1, function(x) paste0("I", paste(as.integer(x), collapse="")))
  names(intlabelID) <- id
  intlabel <- sort(unique(intlabelID))
  IDsplit <- split(id, intlabelID)
  n <- sapply(IDsplit, function(x) sum(x %in% names(label)))
  pos <- sapply(IDsplit, function(x) sum(label[names(label) %in% x]))
  p <- pos/n
  subset <- grepl("I1", intlabel)
  w <- lengths(IDsplit)/length(id)
  nc <- ncorrect(n)
  pc <- pcorrect(p, nc)
  
  IDunused <- lapply(IDsplit, function(x) x[!(x %in% names(label))])
  Nunused <- lengths(IDunused)
  if(sum(Nunused) < m){
    warning(paste("Not enough texts. m too large. Reducing to", sum(Nunused)))
    m <- sum(Nunused)
  }
  
  if(randomize){
    oldvar <- vrecall(w, p = pc, subset = subset, n = nc)
    ncandidates <- nc + diag(length(nc))
    tmp <- oldvar - apply(ncandidates, 2, function(x)vrecall(w, pc, subset, n=x))
    # variance improvement for all candidates
    intsample <- table(sample(factor(1:length(tmp)), m, prob = tmp/sum(tmp), replace = TRUE))
  }else{
    if(exact){
      intsample <- bestsample(w, p=pc, subset, n=nc, m)
      intsamplevar <- calculate.variances(x=intsample, w, p=pc, n=nc, subset)
      intsample <- roundN(intsample[which.min(intsamplevar),])
    }else{
      intsample <- roundN(quicksample(w, p=pc, subset, n=nc, m))
    }
  }
  names(intsample) <- intlabel
  
  if(any(Nunused < intsample)){
    message("At least one intersection includes too few texts. Rearranging the segmentation")
    empty <- logical(length(intsample))
    while(any(Nunused < intsample)){
      empty[Nunused <= intsample] <- TRUE
      intsample[empty] <- Nunused[empty]
      intsample[!empty] <- intsample[!empty] +
        ((intsample[!empty] + 1)/ sum(intsample[!empty]+1)) * (m-sum(intsample))
      intsample <- roundN(intsample)
    }
  }
  
  sampleID <- mapply(function(x,y)sample(x,y), IDunused, intsample)
  unname(sample(unlist(sampleID)))
}

pcorrect <- function(p,n){
  p[is.nan(p)] <- 0.5
  p[p==0] <- (1/(n+1))[p==0]
  p[p==1] <- (1- 1/(n+1))[p==1]
  p
}

ncorrect <- function(n){
  n[n==0] <- 1
  n
}

ci <- function(w,p,subset){
  (sum((w*p)[!subset]) / sum(w*p)^2)^2 * w^2*p*(1-p) *  subset +
    (sum((w*p)[ subset]) / sum(w*p)^2)^2 * w^2*p*(1-p) * !subset
}

quicksample <- function(w,p,subset,n, m){ # smallest negative value iteratively to 0
  CI <- sqrt(ci(w,p,subset))
  mi <- CI * (sum(n) + m) / sum(CI) -n
  tmp <- logical(length(mi))
  while(any(mi<0)){
    tmp <- tmp | mi==min(mi)
    mi[mi==min(mi)] <- 0
    CI <- sqrt(ci(w,p,subset)[!tmp])
    mi[!tmp] <- CI * (sum(n[!tmp]) + m) / sum(CI) -n[!tmp]
  }
  mi
}

bestsample <- function(w,p,subset,n, m){ # try all possible combinations
  eg <- list()
  for(i in 1:length(w)){eg <- c(eg,list(c(TRUE,FALSE)))}
  eg <- expand.grid(eg)
  eg <- eg[-nrow(eg),]
  mi <- unlist(apply(eg,1, function(x){
    CI <- sqrt(ci(w,p,subset)[x])
    CI * (sum(n[x]) + m) / sum(CI) -n[x]}))
  eg2 <- as.vector(t(eg))
  eg2[eg2==TRUE] <- unlist(mi)
  eg2 <- matrix(eg2, nrow=2^length(w)-1, ncol=length(w), byrow=TRUE)
  eg2
}

roundN <- function(n){
  nrest <- n-floor(n)
  n <- floor(n)
  if(sum(nrest)>0){
    n[order(nrest, decreasing=TRUE)[1:round(sum(nrest))]] <-
      n[order(nrest, decreasing=TRUE)[1:round(sum(nrest))]] + 1}
  return(n)
}

calculate.variances <- function(x, w, p, n, subset){
  zul <- apply(x,1,function(y)!any(y<0))
  zul[zul] <- apply(x[zul,],1,function(y)vrecall(w, p, subset, n=n+round(y)))
  zul[zul==0] <- NA
  #  cbind(x,zul)
  zul
}
