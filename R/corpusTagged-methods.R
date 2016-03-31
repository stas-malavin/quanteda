#' @include tokenize.R 
NULL

setGeneric("tokens", function(x) standardGeneric("tokens")) 
setMethod("tokens", signature(x = "corpusTokenized"), 
          function(x) x@documentsDT) 

#' @rdname nsentence
setMethod("nsentence", signature(x = "corpusTokenized"), 
          function(x) {
              nsent <- tokens(x)[, max(sentenceNo), by = docname][, V1]
              names(nsent) <- unique(tokens(x)[, docname])
              nsent
          })

#' @rdname docnames
setMethod("docnames", signature(x = "corpusTokenized"), 
          function(x) {
              toksDT <- tokens(x)
              setkey(toksDT, docname)
              as.character(unique(toksDT)[, docname])
           }
          #as.character(unique(tokens(x)[, docname])))
)

# #' @rdname ntoken
# setMethod("ntoken", signature(x = "corpusTokenized"), 
#           function(x, ...) ntoken(tokenize(x, ...)))
# 
# #' @rdname ntype
# setMethod("ntype", signature(x = "corpusTokenized"), 
#           function(x, ...) ntype(tokenize(x, ...)))
# 

#' @rdname tokenize
setMethod("tokenize", signature(x = "corpusTokenized"),
          function(x, what = c("word", "sentence", "character", "fastestword", "fasterword"),
                   removeNumbers = FALSE, 
                   removePunct = FALSE,
                   removeSeparators = TRUE,
                   removeTwitter = FALSE,
                   removeHyphens = FALSE,
                   # removeURL = TRUE,
                   ngrams = 1L,
                   skip = 0L,
                   concatenator = "_",
                   simplify = FALSE,
                   verbose = FALSE,  ## FOR TESTING
                   ...) {
              
              what <- match.arg(what)
              if (what == "character")
                  stop("what = \"character\" not yet implemented for corpusTokenized")
              
              toks <- tokens(x)
              excludeIndex <- rep(FALSE, nrow(toks))
              if (removePunct)
                  excludeIndex[stringi::stri_detect_regex(toks[, token], "^\\p{P}$")] <- TRUE
              if (removeSeparators)
                  excludeIndex[toks[, POS] == "spc"] <- TRUE
              if (removeNumbers)
                  excludeIndex[stringi::stri_detect_regex(toks[, token], "^\\p{N}+$")] <- TRUE
              
              # toks <- toks[!excludeIndex]
              
              result <- split(toks[, token], toks[, docname])
              
              # make this an S3 class item, if a list
              if (simplify == FALSE & !is.tokenizedTexts(result)) {
                  class(result) <- c("tokenizedTexts", class(result))
              }
              
              attr(result, "what") <- what
              attr(result, "ngrams") <- ngrams
              attr(result, "concatenator") <- ifelse(all.equal(ngrams, 1L)==TRUE, "", concatenator)
              result
          }
)

#' 