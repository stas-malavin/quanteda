#' @include settings.R 
#' @include corpus.R
NULL


#' Virtual class "corpusTagged" for an indexed and tagged corpus object
#' 
#' @description The \code{corpusTagged} object is a special type of 
#'   \link{corpus} that has been indexed by document, sentence, and token, and 
#'   tagged either minimally (distinguishing terms and numbers from white space 
#'   and punctuation characters) or fully using a part-of-speech tagger, if one 
#'   is installed and available (see Details).
#' @slot documents The original texts, and document vars, as a 
#'   \link{data.frame}.
#' @slot documentsDT The indexed corpus, as a \link{data.table}.
#' @slot tagger character indicating which tagger was used.  Current permissible
#'   values are \code{"none"} and \code{"spaCy"}.
#' @slot tagset character indicating which tagset was applied to the corpus. 
#'   Current permissible values are \code{"Penn"}, \code{"Google"}, and
#'   \code{"none"} (which will be applied if no tagger was invoked). White space
#'   characters, defined as a match with the Unicode property
#'   \code{WHITE_SPACE}, are tagged as \code{spc}, and punctuation characters
#'   not identified by a POS tagger are matched to the Unicode property
#'   \code{PUNCTUATION} and marked as "punct".
#' @seealso \link{corpus}
#' @import methods
#' @docType class
#' @name corpusTokenized
# setClass("corpusTokenized",
#          slots = c(documents = "data.frame",
#                    documentsDT = "data.table",
#                    tagger = "character",
#                    tagset = "character",
#                    metadata = "list",
#                    settings = "settings"),
#          prototype = list(tagger = "none", 
#                           tagset = "none", 
#                           settings = settings()),
#          contains = "corpus")
setClass("corpusTokenized",
         slots = c(documentsDT = "data.table",
                   tagger = "character",
                   tagset = "character"),
         prototype = list(tagger = "none", 
                          tagset = "none"),
         contains = "corpus")


#' @rdname corpusTokenized
#' @export
setGeneric("as.corpus", function(object) standardGeneric("as.corpus"))

#' @rdname corpusTokenized
#' @export
setMethod("as.corpus", signature(object = "corpusTokenized"),
    function(object) 
        structure(list(documents = object@documents, 
                       metadata = object@metadata, 
                       settings = object@settings),
                  class = "corpus")
)

#' @rdname corpusTokenized
#' @export
setMethod("as.corpus", signature(object = "corpus"),
    function(object) object)

    
#' Index (and tag) a corpus into documents, sentences, and tokens
#' 
#' Implements indexing and tagging for corpus objects.  This can include
#' applying a part-of-speech tagger, if one is installed and available.
#' Indexing corpus objects can be time-consuming, but means that subsequent operations such as tokenizing, 
#' grouping, or resampling are performed extremely efficiently.
#' @export
setGeneric("index",
           function(x, tagPOS, tagset = c("Penn", "Google"), tagger = c("none", "spaCy"), verbose = TRUE)
               standardGeneric("index"))

#' @rdname index
#' @return An S4 object of class \link{corpusTokenized} that inherits from class \link{corpus}.
#' @examples 
#' oldCorpus <- corpus(c("This, is document 1.  It's Bill's\nsecond sentence.", 
#'                       "Isn't this\tthe 2nd document?  Document\rtwo, second sentence!", 
#'                       "And: voila!"))
#' newCorpus <- index(oldCorpus, verbose = TRUE)
#' \dontrun{# with part of speech tagging
#' newCorpusTagged <- index(oldCorpus, tagger = "spaCy")
#' }
setMethod("index", signature(x = "corpus"), 
          function(x, tagPOS, tagset = c("Penn", "Google"), tagger = c("none", "spaCy"), verbose = TRUE) {
              
              tagset <- match.arg(tagset)
              tagger <- match.arg(tagger)

              if (verbose) cat("Starting corpus indexing...\n")
              
              # separate contractions if using a POS tagger
              if (tagger != "none") {
                  if (verbose) cat("  ...separating contractions (English)\n")
                  texts(x) <- sapply(texts(x), splitContractions, USE.NAMES = FALSE)
              }

              # tokenize the documents by sentence
              if (verbose) cat("  ...segmenting sentences\n")
              sents <- tokenize(x, what = "sentence")
              # make into a data.table, repeat docnames as factor
              sents <- data.table(docname = factor(rep(docnames(x), lengths(sents))), 
                                   token = unlist(sents, use.names = FALSE))
              # add sentence number within document
              if (verbose) cat("  ...indexing sentences\n")
              sents[, sentenceNo := 1:.N, by = docname]

              # tokenize each sentence
              if (verbose) cat("  ...word tokenizing sentences\n")
              toks <- tokenize(sents[, token], removeSeparators = FALSE)
              # create a data table of all tokens 
              tokens <- data.table(docname = rep(sents[, docname], lengths(toks)),
                                   sentenceNo = rep(sents[, sentenceNo], lengths(toks)),
                                   token = unlist(tokenize(sents[, token], removeSeparators = FALSE)))
              # word index within sentence
              if (verbose) cat("  ...indexing tokens\n")
              tokens[, tokenNo := 1:.N, by = list(docname, sentenceNo)]
              
              tokens[, POS := NA]
#               # add POS
#               if (tagset == "Google") {
#                   tokens[, POS := factor(NA, 
#                                          levels = 1:13, 
#                                          labels = c("VERB", "NOUN", "PRON", "ADJ", 
#                                                     "ADV", "ADP", # adpositions (prepositions and postpositions)
#                                                     "CONJ",
#                                                     "DET", 
#                                                     "NUM", 
#                                                     "PRT", 
#                                                     "X",
#                                                     "punct", "spc"))]
#               } else {
#                   tokens[, POS := factor(NA, 
#                                          levels = 1:13, 
#                                          labels = c("VERB", "NOUN", "PRON", "ADJ", 
#                                                     "ADV", "ADP", # adpositions (prepositions and postpositions)
#                                                     "CONJ",
#                                                     "DET", 
#                                                     "NUM", 
#                                                     "PRT", 
#                                                     "X",
#                                                     "punct", "spc"))]]
#               }
                  
              # tag white space
              if (verbose) cat("  ...tagging whitespace tokens\n")
              tokens[stringi::stri_detect_regex(token, "^\\p{Z}$"), POS := "spc"]
              if (verbose) cat("  ...tagging punctuation tokens\n")
              tokens[stringi::stri_detect_regex(token, "^\\p{P}$"), POS := "punct"]
              
              if (tagger == "spaCy") {
                  ## NEED TO CHECK TO SEE IF spaCy EXISTS, OTHERWISE RETURN AN ERROR
                  ## code to check goes here
                  
                  # tag using a system call
                  if (verbose) cat("  ...tagging parts-of-speech using spaCy\n")
                  toksTagged <- tagSentences_spaCy(tokens, tagset)
                  ## THIS COULD BE FASTER WITH A DIRECT CALL TO PYTHON
                  ## ALSO NEED TO CHECK THAT THIS IS THE CORRECT LOCATION FOR .py SOURCE CODE
                  
                  tokens[is.na(POS), POS := tokenize(toksTagged, what = "fastestword", simplify = TRUE)]
              
              } else if (tagger == "none") {
                  if (verbose) cat("  ...tagging punctuation tokens\n")
                  tokens[stringi::stri_detect_regex(token, "^\\p{P}$"), POS := "punct"]
                  tagset <- "none"
              } 

              if (verbose) cat("  ...indexing complete.\n")
              new("corpusTokenized", documentsDT = tokens, tagger = tagger, tagset = tagset)
          })

getSentences <- function(tokensDT) {
    # this replaces any non-space whitespace character with a simple space
    # returns a vector of POS tags, one element per sentence
    tokensDT[is.na(POS), paste(token, collapse = " "), by = .(docname, sentenceNo)][, V1]
}

tagSentences_spaCy <- function(tokensDT, tagset = c("Penn", "Google")) {
    tagset <- match.arg(tagset)
    tmpfile <- tempfile()
    cat(getSentences(tokensDT), file = tmpfile)
    system2("src/posTag.py", args = ifelse(tagset == "Penn", "--penn", character()),
            stdin = tmpfile, stdout = TRUE)
}

splitContractions <- function(charVec) {
    CONTRACTIONS <- c("n't", "'ve", "'s", "'d", "'ll")
    ## function to add white space before English contractions
    stringi::stri_replace_all_fixed(charVec, CONTRACTIONS, paste0(" ", CONTRACTIONS), 
                                    vectorize_all = FALSE)
}
   




