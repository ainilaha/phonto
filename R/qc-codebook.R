


## QC based on codebook metadata for one variable at a time. The main
## goal is to detect and report inconsistencies in a variable within
## or across cycles. By default, the variable is looked for in all
## cycles, but specific cycles may also be specified
## (QuestionnaireDescriptions has BeginYear and EndYear for each
## table)

## Question: fetch Metadata tables in advance and subset in R, because
## they are not that big, or just get for specific variable? Unless we
## cache, second option is probably better (do once inside qc_var).


.create_query <- function(from, ._variable_name_ = NULL, ._table_name_ = NULL, verbose = FALSE) {
    ## Use mangled argument names to avoid potential NSE (mis)matches
    query <- dplyr::tbl(cn(), I(MetadataTable(from)))
    if (!is.null(._variable_name_)) query <- dplyr::filter(query, Variable %in% ._variable_name_)
    if (!is.null(._table_name_)) query <- dplyr::filter(query, TableName %in% ._table_name_)
    if (verbose) show_query(query)
    query
}

##' Access Metadata Tables in NHANES Postgres DB
##'
##' \code{metadata_cb} retrieves data from the VariableCodebook table,
##' \code{metadata_var} retrieves data from the QuestionnaireVariables table
##' and \code{metadata_tab} retrieves data from the QuestionnaireDescriptions table.
##' Where appropriate the returned value can be a subset for a single variable, or
##' NHANES table. The contents of these tables is described in the MetaData vignette for the
##' package.
##'
##' @rdname MetadataTables
##' @aliases metadata_cb metadata_tab metadata_var
##' @title Metadata Tables : Access Postgres DB
##' @param variable Character vector naming one or more variable
##' @param table Character vector naming one or more NHANES table
##' @param UseConstraints If FALSE then only tables with no use constraints are returned.
##' @return A dataframe or tibble with the appropriate subset of the metadata table.
##' @details metadata_var accesses the QuestionnaireVariables metadata table.
##' metadata_cb accesses the VariableCodebook metadata table.
##' metadata_tab accesses the QuestionnaireDescriptions table.
##' The returned object has entries for each variable/table combination where there is a match,
##' in the sense that the table has a variable with the supplied name.
##' @examples
##' ex1 = metadata_cb(variable = "LBDLDL")
##' ex2 = metadata_var(table = "DEMO_D")
##' ex3 = metadata_tab(table = "ACQ_J")
##' ex4 = metadata_var(variable=c("RIDAGEYR",  "RIAGENDR", "BPXSY1"), table=c("DEMO", "DEMO_B", "DEMO_C", "BPX_C"))
##' @export
##' @author Deepayan Sarkar
metadata_cb <- function(variable = NULL, table = NULL) {
    .create_query("VariableCodebook", variable, table) |> dplyr::collect() |> as.data.frame()
}
##' @rdname MetadataTables
##' @export
metadata_var <- function(variable = NULL, table = NULL) {
    .create_query("QuestionnaireVariables", variable, table) |> dplyr::collect() |> as.data.frame()
}
##' @rdname MetadataTables
##' @export
metadata_tab <- function(table = NULL, UseConstraints = FALSE) {
    ans = .create_query("QuestionnaireDescriptions", NULL, table) |> dplyr::collect()
    if (!UseConstraints) ans = ans |> dplyr::filter(UseConstraints == "None")
    return (as.data.frame(ans))
}

## The specific types of discrepancies we look for are:

## - Whether x appears in multiple tables in a given cycle
## - var is the result of a call to metadata_var, cb a result of a call to metadata_cb
## - and tab a result of a call to metadata_tb : these calls may have restricted the entries in
## - those tables
## If yes, should be followed up by a check of whether values are consistent

qc_var_multtable <- function(x, var, cb, tab)
{
    wtable <- subset(var, Variable == x)$TableName
    tsub <- subset(tab, TableName %in% wtable)
    cycle <- with(tsub, paste(BeginYear, EndYear, sep = "-"))
    if (anyDuplicated(cycle)) {
        o <- order(cycle, tsub$TableName)
        return(list(multiple_tables = data.frame(cycle = cycle[o],
                                                 TableName = tsub$TableName[o])))
    }
    return(NULL)
}

## - Inconsistency in Description / SasLabel (mostly benign)
## - qc_var doesn't have an option to pass along ignore.case
qc_var_description <- function(x, var, cb, tab, ignore.case = FALSE)
{
    description <- subset(var, Variable == x)[["Description"]]
    if (ignore.case) description <- tolower(description)
    tt <- table(description)
    if (length(tt) > 1) list(description_mismatch = table(description))
    else NULL
}


qc_var_saslabel <- function(x, var, cb, tab, ignore.case = FALSE)
{
    saslabel <- subset(var, Variable == x)[["SasLabel"]]
    if (ignore.case) saslabel <- tolower(saslabel)
    tt <- table(saslabel)
    if (length(tt) > 1) list(saslabel_mismatch = table(saslabel))
    else NULL
}

## compare the targeting of the question
qc_var_target <- function(x, var, cb, tab, ignore.case = FALSE)
{
    target <- subset(var, Variable == x)[["Target"]]
    if (ignore.case) target <- tolower(target)
    tt <- table(target)
    if (length(tt) > 1) list(target_mismatch = table(target))
    else NULL
}

## compare the levels (Value Description) of the variable.  Can be
## useful for numeric as well, if some interpretations have
## changed (e.g. age coarsening --- see RIDAGEYR). Can also have some cycles
## (apparently) numeric and some categorical. TODO

qc_var_levels <- function(x, var, cb, tab, ignore.case = FALSE)
{
    varlevels <- subset(cb, Variable == x)
    ans <- xtabs( ~ ValueDescription + TableName, varlevels)
    ## if categorical, all entries should be 1
    if (all(ans == 1)) return(NULL)
    ## can still be OK, if numeric --- indicated by the presence of "Range of Values"
    ## TODO
    ## otherwise, return the levels that appear in some but not all tables
    ok <- rowSums(ans) == ncol(ans)
    if (all(ok)) stop("Unexpected counts")
    return(list(levels_mismatch = ans[!ok, , drop = FALSE]))
}




## - Inconsistency in type (numeric / categorical)

## - Inconsistency in levels for categorical variables (capitalization / other)

## - Presence of 'special' values in numeric variables, and
##   inconsistency in them (including different codes for same
##   value). Should have option to exclude common examples like "Don't
##   know", "Refused", etc.

## - Data coarsening (this may be tricky to identify)

## - Whether variable may be skipped. This requires preparing an
##   initial table-level summary.

## For variables appearing in multiple tables in the same cycle, an
## additional check could be to see if it records the same data. This
## should be a separate check, as it involves accessing the actual
## data.





##' QC report for a single variable in NHANES
##'
##' @title qc_var: QC on NHANES variable
##' @param x Character vector of length one, naming a variable in one or more NHANES tables
##' @param var Optional data frame containing variable metadata
##' @param cb Optional data frame containing codebook metadata
##' @param tab Optional data frame containing table metadata
##' @param UseConstraints If FALSE, the default, then only publicly available tables are searched.
##' @return An object of S3 class \code{"qc_var"} with suitable print and summary methods.
##' @export
##' @details
##' The arguments var, cb and tab, will be default use the corresponding metadata tables (Variables, CodeBook and Questionnaires).
##'
##' @examples
##' t1 = qc_var("DMDEDUC3")
##' ## restrict the tables
##' t2 = qc_var("DMDEDUC3", tables=c("DEMO_B", "DEMO_J"))
##' print(t2, show = "saslabel")
##' qc_var("LBCBHC")
##' qc_var("LBXHCT")
##' @author Deepayan Sarkar
qc_var <- function(x, tables = tables, var = metadata_var(x), cb = metadata_cb(x), tab = metadata_tab(), UseConstraints = FALSE)
{
    if( !UseConstraints)
      tab = metadata_tab(UseConstraints=UseConstraints)
    availableTables = tab$TableName
    if(!missing(tables)) {
        missingTabs = setdiff(tables, availableTables)
        availableTables = intersect(tables, availableTables)
        if(length(missingTabs > 0 )) warning(paste("Specified tables:", missingTabs,
                                                   "are not public or don't exist"))
        cb = metadata_cb(x, availableTables)
        var = metadata_var(x, availableTables)
    }

    res <- c(qc_var_multtable(x, var, cb, tab),
             qc_var_description(x, var, cb, tab),
             qc_var_saslabel(x, var, cb, tab),
             qc_var_target(x, var, cb, tab),
             qc_var_levels(x, var, cb, tab))
    if (is.null(res)) res <- list()
    structure(res,
              variable = x,
              class = "qc_var")
}

#' @rdname qc_var
#' @export
#' @param object An object of class \code{"qv_var"}
#' @param ... Additional arguments, ignored
summary.qc_var <- function(object, ...)
{
    data.frame(Variable = attr(object, "variable"),
               multtable = !is.null(object$multiple_tables),
               description = !is.null(object$description_mismatch),
               saslabel = !is.null(object$saslabel_mismatch),
               target = !is.null(object$target_mismatch))
}



#' @rdname qc_var
#' @param x An object of class \code{"qv_var"}
#' @param show A vector giving the types of identified anomalies to
#'     print. Currently four types of anomalies are identified:
#'     presence of the variable in multiple tables in the same cycle;
#'     mismatches in the Description field, mismatches in SAS Label,
#'     and mismatch in the Target specification. Some of these
#'     mimatches are benign, so it is useful to suppress them when
#'     printing.
#' @export
print.qc_var <- function(x, ...,
                         show = c("multiple", "description",
                                  "saslabel", "target", "levels"))
{
    ok <- TRUE
    cat("Variable: ", attr(x, "variable"))
    if (!is.null(x$multiple_tables) && "multiple" %in% show)
    {
        ok <- FALSE
        cat("\nAppears in multiple tables within same cycle:\n")
        ## wcycle <- which(duplicated(x$multiple_tables$cycle))
        ## wsub <- subset(x$multiple_tables, cycle %in% cycle[wcycle])
        tapply(x$multiple_tables, ~ cycle, function(d) paste(d$TableName, collapse = " / ")) |>
            array2DF(responseName = "Tables") |> print()
    }
    if (!is.null(x$description_mismatch) && "description" %in% show)
    {
        ok <- FALSE
        cat("\nMismatch in Description:\n")
        print(array2DF(x$description_mismatch, responseName = "Frequency"))
    }
    if (!is.null(x$saslabel_mismatch) && "saslabel" %in% show)
    {
        ok <- FALSE
        cat("\nMismatch in Saslabel:\n")
        print(array2DF(x$saslabel_mismatch, responseName = "Frequency"))
    }
    if (!is.null(x$target_mismatch) && "target" %in% show)
    {
        ok <- FALSE
        cat("\nMismatch in Target:\n")
        print(array2DF(x$target_mismatch, responseName = "Frequency"))
    }
    if (!is.null(x$levels_mismatch) && "levels" %in% show)
    {
        ok <- FALSE
        cat("\nMismatch in Levels:\n")
        present <- rowSums(x$levels_mismatch)
        print(cbind(present = present,
                    absent = ncol(x$levels_mismatch) - present))
    }
    if (ok) cat(" --- no problems found")
    invisible(x)
}



