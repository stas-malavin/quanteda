#' select features from an object
#' 
#' This function selects or discards features from a dfm.variety of objects, 
#' such as tokenized texts, a dfm, or a list of collocations.  The most common 
#' usage for \code{removeFeatures} will be to eliminate stop words from a text 
#' or text-based object, or to select only features from a list of regular 
#' expression.
#' @param x object whose features will be selected
#' @param features one of: a character vector of features to be selected, a 
#'   \link{dfm} whose features will be used for selection, or a dictionary class
#'   object whose values (not keys) will provide the features to be selected. 
#'   For \link{dfm} objects, see details in the Value section below.
#' @param selection whether to keep or remove the features
#' @param valuetype how to interpret feature vector: \code{fixed} for words as 
#'   is; \code{"regex"} for regular expressions; or \code{"glob"} for 
#'   "glob"-style wildcard
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param verbose if \code{TRUE} print message about how many features were 
#'   removed
#' @param ... supplementary arguments passed to the underlying functions in 
#'   \code{\link[stringi]{stri_detect_regex}}.  (This is how 
#'   \code{case_insensitive} is passed, but you may wish to pass others.)
#' @note This function selects features based on their labels.  To select 
#'   features based on the values of a the document-feature matrix, use 
#'   \code{\link{trim}}.
#' @return A dfm after the feature selection has been applied.
#'   
#'   When \code{features} is a \link{dfm-class} object, then the returned object
#'   will be identical in its feature set to the dfm supplied as the
#'   \code{features} argument.  This means that any features in \code{x} not in
#'   \code{features} will be discarded, and that any features in found in the
#'   dfm supplied as \code{features} but not found in \code{x} will be added
#'   with all zero counts.  This is useful when you have trained a model on one dfm, and
#'   need to project this onto a test set whose features must be identical.
#'   
#' @export
#' @seealso \code{\link{removeFeatures}}, \code{\link{dfm_trim}}
selectFeatures <- function(x, features, ...) {
    UseMethod("selectFeatures")
}



#' @rdname selectFeatures
#' @param padding (only for \code{tokenizedTexts} objects) if \code{TRUE}, leave
#'   an empty string where the removed tokens previously existed.  This is
#'   useful if a positional match is needed between the pre- and post-selected
#'   features, for instance if a window of adjacency needs to be computed.
#' @param indexing use dfm-based index to efficiently process large tokenizedTexts object
#' @export
#' @examples 
#' \dontrun{## performance comparisons
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(SOTUCorpus, removePunct = TRUE)
#' # toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' # head to head, old v. new
#' system.time(selectFeaturesOLD(toks, stopwords("english"), "remove", verbose = FALSE))
#' system.time(selectFeatures(toks, stopwords("english"), "remove", verbose = FALSE))
#' system.time(selectFeaturesOLD(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"))
#' system.time(selectFeatures(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"))
#' microbenchmark::microbenchmark(
#'     old = selectFeaturesOLD(toks, stopwords("english"), "remove", verbose = FALSE),
#'     new = selectFeatures(toks, stopwords("english"), "remove", verbose = FALSE),
#'     times = 5, unit = "relative")
#' microbenchmark::microbenchmark(
#'     new = selectFeaturesOLD(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"),
#'     old = selectFeatures(toks, c("and", "of"), "remove", verbose = FALSE, valuetype = "regex"),
#'     times = 2, unit = "relative")
#'     
#' types <- unique(unlist(toks))
#' numbers <- types[stringi::stri_detect_regex(types, '[0-9]')]
#' microbenchmark::microbenchmark(
#'     new = selectFeaturesOLD(toks, numbers, "remove", verbose = FALSE, valuetype = "fixed"),
#'     old = selectFeatures(toks, numbers, "remove", verbose = FALSE, valuetype = "fixed"),
#'     times = 2, unit = "relative")  
#'     
#' # removing tokens before dfm, versus after
#' microbenchmark::microbenchmark(
#'     pre = dfm(selectFeaturesOLD(toks, stopwords("english"), "remove"), verbose = FALSE),
#'     post = dfm(toks, ignoredFeatures = stopwords("english"), verbose = FALSE),
#'     times = 5, unit = "relative")
#' }
#' 
#' ## with simple examples
#' toks <- tokenize(c("This is a sentence.", "This is a second sentence."), 
#'                  removePunct = TRUE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                 valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' 
#' # how case_insensitive works
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "fixed", padding = TRUE, case_insensitive = FALSE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "fixed", padding = TRUE, case_insensitive = TRUE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "glob", padding = TRUE, case_insensitive = TRUE)
#' selectFeatures(toks, c("is", "a", "this"), selection = "remove", 
#'                valuetype = "glob", padding = TRUE, case_insensitive = FALSE)
#' 
#' # with longer texts
#' txts <- data_char_inaugural[1:2]
#' toks <- tokenize(txts)
#' selectFeatures(toks, stopwords("english"), "remove")
#' selectFeatures(toks, stopwords("english"), "keep")
#' selectFeatures(toks, stopwords("english"), "remove", padding = TRUE)
#' selectFeatures(toks, stopwords("english"), "keep", padding = TRUE)
#' selectFeatures(tokenize(data_char_inaugural[2]), stopwords("english"), "remove", padding = TRUE)
selectFeatures.tokenizedTexts <- function(x, features, selection = c("keep", "remove"), 
                                           valuetype = c("glob", "regex", "fixed"),
                                           case_insensitive = TRUE, padding = FALSE, indexing = FALSE,
                                           verbose = FALSE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    originalvaluetype <- valuetype
    features <- unique(unlist(features, use.names=FALSE))  # to convert any dictionaries
    y <- qatd_cpp_deepcopy(x) # copy x to y to prevent changes in x
    n <- length(y)
    
    if(indexing){
        if(verbose) catm("Indexing tokens...\n")
        index <- dfm(y, verbose = FALSE)
        index_binary <- as(index, 'nMatrix')
        types <- colnames(index_binary)
    }else{
        types <- unique(unlist(y, use.names=FALSE))
        flag <- rep(TRUE, n)
    }
    
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "fixed") {
        
        if (case_insensitive) {
            #types <- unique(unlist(y, use.names=FALSE))
            types_match <- types[toLower(types) %in% toLower(features)]
        } else {
            types_match <- features
        }
        if (indexing) flag <- Matrix::rowSums(index_binary[,types_match]) > 0 # identify texts where types match appear
        if (verbose) catm(sprintf("Scanning %.2f%% of texts...\n", 100 * sum(flag) / n))
        if(selection == "remove"){
            select_tokens_cppl(y, flag, types_match, TRUE, padding)
        }else{ 
            select_tokens_cppl(y, flag, types_match, FALSE, padding)
        }
    } else if (valuetype == "regex") {
        if (verbose) catm("Converting regex to fixed...\n")
        types_match <- unlist(regex2fixed(features, types, valuetype, case_insensitive), use.names = FALSE) # get all the unique types that match regex
        if(indexing) flag <- Matrix::rowSums(index_binary[,types_match]) > 0 # identify texts where types match appear
        if(verbose) catm(sprintf("Scanning %.2f%% of texts...\n", 100 * sum(flag) / n))
        if (selection == "remove") {
            select_tokens_cppl(y, flag, types_match, TRUE, padding)  # search as fixed
        } else {
            select_tokens_cppl(y, flag, types_match, FALSE, padding) # search as fixed
        }
    }
    
    class(y) <- c("tokenizedTexts", class(x))
    attributes(y) <- attributes(x)
    return(y)
}

#' @export
#' @rdname selectFeatures
#' @examples
#' toksh <- tokens(c(doc1 = "This is a SAMPLE text", doc2 = "this sample text is better"))
#' feats <- c("this", "sample", "is")
#' # keeping features
#' selectFeatures(toksh, feats, selection = "keep")
#' selectFeatures(toksh, feats, selection = "keep", padding = TRUE)
#' selectFeatures(toksh, feats, selection = "keep", case_insensitive = FALSE)
#' selectFeatures(toksh, feats, selection = "keep", padding = TRUE, case_insensitive = FALSE)
#' # removing features
#' selectFeatures(toksh, feats, selection = "remove")
#' selectFeatures(toksh, feats, selection = "remove", padding = TRUE)
#' selectFeatures(toksh, feats, selection = "remove", case_insensitive = FALSE)
#' selectFeatures(toksh, feats, selection = "remove", padding = TRUE, case_insensitive = FALSE)
selectFeatures.tokens <- function(x, features, selection = c("keep", "remove"), 
                                  valuetype = c("glob", "regex", "fixed"),
                                  case_insensitive = TRUE, padding = FALSE, ...) {
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)

    # convert to regular expressions if not already valuetype = "regex"
    if (valuetype == "glob" | valuetype == "fixed") 
        features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)

    # index the matching features from the types vector
    features <- unique(unlist(features, use.names = FALSE))  # to convert any dictionaries
    features_index <- 
        which(stringi::stri_detect_regex(types(x), paste(features, collapse = "|"),
                                         case_insensitive = case_insensitive))
    
    # invert the match index to remove features
    if (selection == "remove")
        features_index <- setdiff(seq_along(types(x)), features_index)
    
    # save the attributes
    attrs_pre <- attributes(x)
    
    # match the features
    match_list <- lapply(unclass(x), fastmatch::fmatch, features_index)
    # get the new word types
    newfeats_types <- types(x)[features_index[!is.na(features_index)]]
    
    if (!padding) {
        newfeats <- lapply(match_list, function(y) y[!is.na(y)])
    } else {
        newfeats_types <- c(newfeats_types, "")
        newfeats <- lapply(match_list, function(y) { 
            # add an empty type for non-matches
            y[is.na(y)] <- length(newfeats_types)
            y 
        })
    } 
    
    # replace the attributes
    attributes(newfeats) <- attrs_pre
    # but use the new types
    types(newfeats) <- newfeats_types
    
    newfeats
}



#' @rdname selectFeatures
#' @param pos indexes of word position if called on collocations: remove if word
#'   \code{pos} is a stopword
#' @examples  
#' 
#' ## example for collocations
#' (myCollocs <- collocations(data_char_inaugural[1:3], n=20))
#' selectFeatures(myCollocs, stopwords("english"), "remove")
#' @export
selectFeatures.collocations <- function(x, features, selection = c("keep", "remove"), 
                                        valuetype = c("glob", "regex", "fixed"),
                                        case_insensitive = TRUE,
                                        verbose = TRUE, pos = 1:3, ...) {
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features <- unique(unlist(features))  # to convert any dictionaries
    if (case_insensitive) features <- toLower(features)
    
    if (valuetype == "regex")
        stop("regex not currently supported for selectFeatures.collocations")
    
    if (selection == "keep")
        stop("keep not currently supported for selectFeatures.collocations")
    
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            stop("glob not currently supported for selectFeatures.collocations")
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    word <- word1 <- word2 <- word3 <- NULL
    origclass <- class(x)
    if (!all(pos %in% 1:3))
        stop("pos for collocation position can only be 1, 2, and/or 3")
    nstart <- nrow(x)
    stopwordTable <- data.table(word = features, remove = 1L)
    setkey(stopwordTable, word)
    x$order <- 1:nrow(x)
    
    if (case_insensitive)
        x[, c("word1", "word2", "word3") := list(toLower(word1), toLower(word2), toLower(word3))]
    
    if (3 %in% pos) {
        setnames(stopwordTable, 1, "word3")
        setkey(x, word3)
        x <- stopwordTable[x]
        if (selection == "remove") x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (2 %in% pos) {
        setnames(stopwordTable, 1, "word2")
        setkey(x, word2)
        x <- stopwordTable[x]
        if (selection == "remove") x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    if (1 %in% pos) {
        setnames(stopwordTable, 1, "word1")
        setkey(x, word1)
        x <- stopwordTable[x]
        if (selection == "remove") x <- x[is.na(remove)]
        x[, remove:=NULL]
    }
    setorder(x, order)
    setcolorder(x, c("word1", "word2", "word3", names(x)[4:ncol(x)]))
    x[, order:=NULL]
    nend <- nrow(x)
    if (verbose) catm("Removed ", format(nstart - nend, big.mark=","),  
                     " (", format((nstart - nend)/nstart*100, digits=3),
                     "%) of ", format(nstart, big.mark=","), 
                     " collocations containing one of ", 
                     length(features), " stopwords.\n", sep="")
    class(x) <- origclass
    x
}




#' remove features from an object
#' 
#' Removes features from a variety of objects, such as text, a
#' dfm, or a list of collocations.  The most common usage for
#' \code{removeFeatures} will be to eliminate stop words from a text or
#' text-based object.  This function simply provides a convenience wrapper for
#' \code{\link{selectFeatures}} where \code{selection = "remove"}.
#' 
#' @param x object from which stopwords will be removed
#' @param features character vector of features to remove
#' @param ... additional arguments passed to \code{\link{selectFeatures}}
#' @return an object with matching features removed
#' @name removeFeatures
#' @export
#' @author Kenneth Benoit
#' @seealso \link{stopwords}
#' @examples
#' \dontrun{
#' ## for tokenized texts 
#' txt <- c(wash1 <- "Fellow citizens, I am again called upon by the voice of my country to 
#'                    execute the functions of its Chief Magistrate.",
#'          wash2 <- "When the occasion proper for it shall arrive, I shall endeavor to express
#'                    the high sense I entertain of this distinguished honor.")
#' removeFeatures(tokenize(txt, removePunct = TRUE), stopwords("english"))
#' 
#' itText <- tokenize("Ecco alcuni di testo contenente le parole che vogliamo rimuovere.", 
#'                    removePunct = TRUE)
#' removeFeatures(itText, stopwords("italian"), case_insensitive = TRUE)
#' 
#' ## example for dfm objects
#' mydfm <- dfm(data_char_ukimmig2010, verbose=FALSE)
#' removeFeatures(mydfm, stopwords("english"))
#' 
#' ## example for collocations
#' (myCollocs <- collocations(data_char_inaugural[1:3], n=20))
#' removeFeatures(myCollocs, stopwords("english"))
#' removeFeatures(myCollocs, stopwords("english"), pos = 2)
#' }
removeFeatures <- function(x, features, ...) {
    if ("selection" %in% names(list(...)))
        stop("cannot override selection argument in removeFeatures")
    selectFeatures(x, features, selection = "remove", ...)
}


### WORKING/TESTING CODE #####################

# @rdname selectFeatures
# @examples
# ## example for collocations
# (myCollocs <- collocations(data_char_inaugural[1:3], n=20))
# selectFeatures(myCollocs, stopwords("english"), "remove")
# @export
selectFeatures_collocations <- function(x, features, selection = c("keep", "remove"), 
                                        valuetype = c("glob", "regex", "fixed"),
                                        case_insensitive = TRUE,
                                        verbose = TRUE, pos = 1:3, ...) {
    
    # necessary to pass by value - inefficient but "R-like"
    # xloc <- data.table::copy(x)
    
    selection <- match.arg(selection)
    valuetype <- match.arg(valuetype)
    features <- unique(unlist(features))  # to convert any dictionaries
    
    word <- word1 <- word2 <- word3 <- NULL
    if (!all(pos %in% 1:3))
        stop("pos for collocation position can only be 1, 2, and/or 3")
    
    # convert glob to fixed if no actual glob characters (since fixed is much faster)
    if (valuetype == "glob") {
        # treat as fixed if no glob characters detected
        if (!sum(stringi::stri_detect_charclass(features, c("[*?]"))))
            valuetype <- "fixed"
        else {
            features <- sapply(features, utils::glob2rx, USE.NAMES = FALSE)
            valuetype <- "regex"
        }
    }
    
    if (valuetype == "fixed") {
        
        if (case_insensitive) {
            features <- toLower(features)
            x[, paste0("word", pos) := lapply(x[, pos, with = FALSE], tolower)]
        }            
        x[, paste0("i", pos) := lapply(x[, pos, with = FALSE], function(y) {
            if (selection == "remove") 
                !(y %in% features)
            else 
                (y %in% features)
        })]
        
    } else if (valuetype == "regex") {
        
        x[, paste0("i", pos) := lapply(x[, pos, with = FALSE], function(y) {
            if (selection == "remove") 
                !stringi::stri_detect_regex(y, paste0(features, collapse = "|"), case_insensitive = case_insensitive, ...)
            else 
                stringi::stri_detect_regex(y, paste0(features, collapse = "|"), case_insensitive = case_insensitive, ...)
        })]
        
    }
    
    x <- x[apply(x[, grep("^i", colnames(x)), with = FALSE], 1, all) == TRUE]
    x[, -(grep("^i", colnames(x))), with = FALSE]
}

# x <- data.table::copy(myCollocs)
# features <- stopwords("english")
# selection = c("remove") 
# valuetype = c("glob")
# case_insensitive = TRUE
# verbose = TRUE
# pos = 1:3

# require(microbenchmark)
# myCollocs <- collocations(data_corpus_inaugural, size=2:3)
# microbenchmark::microbenchmark(
#     old = removeFeatures(myCollocs, stopwords("english"), verbose = FALSE),
#                          new = selectFeatures(myCollocs, stopwords("english"), "remove"),
#                          unit = "relative", times = 30
#     )


