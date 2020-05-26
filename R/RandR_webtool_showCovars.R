
#' Show R&R GDM covariates
#'
#' A function to display the current covariate information stored in a R&R GDM model object.
#'
#' @param thisModel Character. Full path the .Rd file for an R&R GDM model object
#' @return Nothing
#' @export
#'
#' @examples
showModelCovars <- function(thisModel)
{
  if (!(file.exists(thisModel))) stop("showModelCovars: Cannot find GDM object.")

  load(thisModel)

  # Jason calls the model object "md", so we will assume that this is the name retrieved by a successful call to load():
  if (!("gdm" %in% class(md$model))) stop("showModelCovars: The GDM model object is malformed.")

  cat("showModelCovars: R&R Webtool GDM Model:", basename(thisModel), "\n")
  cat("\tmodel_predictors:", paste0(md$model$predictors, collapse = "; "),"\n")

  cat("\tHas S:",md$S,"\n")

  if (class(md$sdata) == "RasterStack")
    cat("\t\tsrast path:",md$sdata@layers[[1]]@file@name,"\n")
  else
    cat("\t\tsrast path:\n")

  cat("\tHas Q:",md$Q,"\n")
  if (md$Q)
  {
    cat("\t\tQ-files:\n") #, paste0(md$qdata, collapse = "; "),"\n")
    if (is.null(md$qdata))
      cat(crayon::yellow("\t\t\tWARNING: Q-file object is missing!\n"))
    else
      lapply(md$qdata@layers, function(el) {cat("\t\t\t", el@file@name, "\n")})
  }

  cat("\tHas E:",md$E,"\n")
  if (md$E)
  {
    cat("\t\tE-files:", "\n")
    if (is.null(md$edata))
      cat("\t\t\tWARNING: Q-file object is missing!\n")
    else
      lapply(md$edata@layers, function(el) {cat("\t\t\t", el@file@name, "\n")})
  }

  cat("\n\tthreshold:", md$threshold, "\n")


  cat("\n")
}
