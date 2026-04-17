#' Manage Operating Model Files
#'
#' A family of functions for listing, loading, and deleting saved Operating
#' Model (OM) files stored under the `Objects/OM/` directory structure.
#'
#' @param OMpath `character(1)`. Path to the directory containing OM
#'   subdirectories. Defaults to `"Objects/OM"`.
#' @param type `character(1)`. Type of Operating Model. One of
#'   `"SingleStock"` (default) or `"MultiStock"`. Determines the subdirectory
#'   used within `OMpath`.
#' @param print `logical(1)`. If `TRUE`, prints the list of found
#'   OM files to the console. If `FALSE` (default), returns the file paths invisibly
#'   without printing.
#' @param name `character(1)`. Name of the OM to load, without the `.om`
#'   extension. Defaults to `"BlackSeaBass"`. (`LoadOM` only.)
#'
#' @return
#' - `ListOMFiles`: character vector of full file paths. Returned invisibly
#'   when `print = FALSE`.
#' - `LoadOM`: the `OM` object.
#' - `DeleteOMs`: a logical vector from [base::file.remove()], one element per
#'   file, indicating whether each deletion succeeded.
#'
#' @name OM-files
NULL

#' @export
#' @rdname OM-files
ListOMFiles <- function(OMpath='Objects/OM', type=c('SingleStock', 'MultiStock'), print=FALSE) {
  type <- match.arg(type)
  path <- file.path(OMpath, type)
  om.files <- list.files(path, full.names = TRUE)
  if (print) {
    cli::cli_text("OM objects in  {.val {path}}:")
    for (i in seq_along(om.files)) {
      cli::cli_li("{basename(om.files[i])}")
    }
  }

  if (!print)
    return(invisible(om.files))
  om.files
}

#' @export
#' @rdname OM-files
LoadOM <- function(name=NULL, OMpath='Objects/OM', type=c('SingleStock', 'MultiStock')) {

  type <- match.arg(type)

  if (is.null(name))
    name <- ListOMFiles(OMpath=OMpath, type=type, print=FALSE)

  name <- gsub(' ', '', name)

  name <- basename(name) |> tools::file_path_sans_ext()

  om.files <- ListOMFiles(OMpath, type, print=FALSE)
  om.file <- paste0(basename(name), '.om')

  chk <- om.file %in% basename(om.files)

  if (!all(chk))
    cli::cli_abort(c("x"="{.val {om.file[!chk]}} not found in directory: {.val {file.path(OMpath, type)}}",
                     'i'='Available files are: {.val {basename(om.files)}} '))

  if (length(om.file)==1)
    return(readRDS(file.path(OMpath, type, om.file)))

  OM_List <- lapply(om.file, function(i)
    readRDS(file.path(OMpath, type, i))
    )

  names(OM_List) <- name
  OM_List

}


#' @export
#' @rdname OM-files
DeleteOMs <- function(OMpath='Objects/OM', type=c('SingleStock', 'MultiStock')) {
  type <- match.arg(type)
  om.files <- ListOMFiles(OMpath, type, print=FALSE)

  path <- file.path('Objects', OMpath, type)
  cli::cli_text("Deleting {.val {basename(om.files)}} from path: {.val {path}}")
  if (type == 'SingleStock') {
    cli::cli_text("See script {.val 1. Import BAM OMs.R} to re-create OMs")
  } else {
    cli::cli_text("See script {.val 3. MultiStock OM.R} to re-create OMs")
  }

  file.remove(om.files)

}
