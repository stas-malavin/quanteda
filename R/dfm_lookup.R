#' apply a dictionary to a dfm
#' 
#' Apply a dictionary to a dfm by looking up all dfm features for matches in a
#' a set of \link{dictionary} values, and combine replace those features with a 
#' count of the dictionary's keys.  If \code{exclusive = FALSE} then the 
#' behaviour is to apply a "thesaurus" where each value match is replaced by 
#' the dictionary key, converted to capitals if \code{capkeys = TRUE} (so that 
#' the replacements are easily distinguished from features that were terms
#' found originally in the document).
#' @param x the dfm to which the dictionary will be applied
#' @param dictionary a \link{dictionary} class object
#' @param exclusive if \code{TRUE}, remove all features not in dictionary, 
#'   otherwise, replace values in dictionary with keys while leaving other 
#'   features unaffected
#' @param valuetype how to interpret dictionary values: \code{"glob"} for 
#'   "glob"-style wildcard expressions (the format used in Wordstat and LIWC
#'   formatted dictionary values); \code{"regex"} for regular expressions; or
#'   \code{"fixed"} for exact matching (entire words, for instance)
#' @param case_insensitive ignore the case of dictionary values if \code{TRUE}
#' @param capkeys if \code{TRUE}, convert dictionary keys to
#'   uppercase to distinguish them from other features
#' @param verbose print status messages if \code{TRUE}
#' @export
#' @keywords dfm
#' @examples
#' myDict <- dictionary(list(christmas = c("Christmas", "Santa", "holiday"),
#'                           opposition = c("Opposition", "reject", "notincorpus"),
#'                           taxglob = "tax*",
#'                           taxregex = "tax.+$",
#'                           country = c("United_States", "Sweden")))
#' myDfm <- dfm(c("My Christmas was ruined by your opposition tax plan.", 
#'                "Does the United_States or Sweden have more progressive taxation?"),
#'              remove = stopwords("english"), verbose = FALSE)
#' myDfm
#' 
#' # glob format
#' dfm_lookup(myDfm, myDict, valuetype = "glob")
#' dfm_lookup(myDfm, myDict, valuetype = "glob", case_insensitive = FALSE)
#'
#' # regex v. glob format: note that "united_states" is a regex match for "tax*"
#' dfm_lookup(myDfm, myDict, valuetype = "glob")
#' dfm_lookup(myDfm, myDict, valuetype = "regex", case_insensitive = TRUE)
#' 
#' # fixed format: no pattern matching
#' dfm_lookup(myDfm, myDict, valuetype = "fixed")
#' dfm_lookup(myDfm, myDict, valuetype = "fixed", case_insensitive = FALSE)
dfm_lookup <- function(x, dictionary, exclusive = TRUE, valuetype = c("glob", "regex", "fixed"), 
                       case_insensitive = TRUE,
                       capkeys = !exclusive,
                       verbose = TRUE) {
    
    if (!is.dfm(x))
        stop("x must be a dfm object")
    
    # cannot/should not apply dictionaries with multi-word keys to a dfm
    if (any(stringi::stri_detect_charclass(unlist(dictionary, use.names = FALSE), "\\p{Z}"))) {
        warning("You will probably not get correct behaviour applying a dictionary with multi-word keys to a dfm.")
    }
    
    valuetype <- match.arg(valuetype)
    dictionary <- dictionary_flatten(dictionary)

    if (verbose) catm("applying a dictionary consisting of ", length(dictionary), " key", 
                      ifelse(length(dictionary) > 1, "s", ""), "\n", sep="")
    
    # convert wildcards to regular expressions (if needed)
    if (valuetype == "glob") {
        dictionary <- lapply(dictionary, utils::glob2rx)
        # because glob2rx doesn't get closing parens
        dictionary <- lapply(dictionary, function(y) gsub("\\)", "\\\\\\)", y))
    } # else if (valuetype == "fixed")
    # dictionary <- lapply(dictionary, function(x) paste0("^", x, "$"))
    
    newDocIndex <- rep(1:nrow(x), length(dictionary))
    newFeatures <- names(dictionary)
    uniqueFeatures <- features(x)
    newFeatureIndexList <- lapply(dictionary, function(x) {
        # ind <- grep(paste(x, collapse = "|"), uniqueFeatures, ignore.case = case_insensitive)
        if (valuetype == "fixed") {
            if (case_insensitive)  
                ind <- which(toLower(uniqueFeatures) %in% (toLower(x)))
            else ind <- which(uniqueFeatures %in% x)
        }
        else ind <- which(stringi::stri_detect_regex(uniqueFeatures, paste(x, collapse = "|"), case_insensitive = case_insensitive))
        if (length(ind) == 0)
            return(NULL)
        else 
            return(ind)
    })
    if (capkeys) newFeatures <- stringi::stri_trans_toupper(newFeatures)
    newFeatureCountsList <- lapply(newFeatureIndexList,
                                   function(i) {
                                       if (!is.null(i)) 
                                           rowSums(x[, i])
                                       else 
                                           rep(0, nrow(x))
                                   })
    dfmresult2 <- new("dfmSparse", sparseMatrix(i = newDocIndex,
                                                j = rep(1:length(dictionary), each = ndoc(x)),
                                                x = unlist(newFeatureCountsList),
                                                dimnames=list(docs = docnames(x), 
                                                              features = newFeatures)))
    if (!exclusive) {
        if (!all(is.null(keyIndex <- unlist(newFeatureIndexList, use.names = FALSE))))
            dfmresult2 <- cbind(x[, -keyIndex], dfmresult2)
        else
            dfmresult2 <- cbind(x, dfmresult2)
    }
    
    dfmresult2
}


