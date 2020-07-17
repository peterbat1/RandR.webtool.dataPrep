
#' Set R&R GDM covariate paths
#'
#' Set paths to covariate files within an R&R GDM model object to the standard paths for the webtool
#'
#' @param thisModel Character. Full path the .Rd file for an R&R GDM model object.
#' @param thisTaxon Character. Name of the taxon corresponding to the model given in \emph{thisModel}.
#' @param envDataPath Character. Full path to the folder of environmental covariate layers. When environmental covariates are present in the model object, a default of NULL sets the path to the standard Linux environment on which the webtool operates (i.e. '/home/RandR/envData/eastOZ').
#' @param qDataPath Character. Full path to the folder of environmental ancestral mixing coefficent (Q-data covariate) layers. When Q covariates are present in the model object, a default of NULL sets the path to the standard Linux environment on which the webtool operates (i.e. '/home/RandR/qData/eastOZ').
#' @param threshold Numeric. A value between 0 and 1 to replace the threshold value of the model object. Default of NULL leaves the threshold unchanged.
#' @param trace Logical. Should some hopefully useful information be output to the terminal? Default is FALSE.
#'
#' @return NULL
#' @export
#'
#' @details {The R&R webtool allows the use of two types of covariate files: Environmental covariates, and ancestry mixing coefficients (Q-files).
#'
#' Environmental covariates are stored in /home/RandR/envData with sub-folders for geographically-defined extents. At present, the only sub-folder used is "eastOZ".
#'
#' Q-file covariates are stored on the server in /home/RandR/qData, and there is also, at present, only one sub-folder 'eastOZ'.
#'
#' This function looks at the covariates listed for the fitted GDM model, confirms that the matching covariate files are present in the necessary location, and then sets the necessary elements of the model object to load raster stacks of covariates from the appropriate folders described above.
#'
#' A new value for the threshold applied to the GIS raster output by the model to create the "local" genetic region or provenance can be set by passing a new value in the parameter \emph{threshold}. Threshold values must be between 0 and 1; a value passed into the function outside this range causes the function to stop. The default value of NULL leaves the threshold value unchanged.
#'
#' The updated model object is then saved over the original model object. For safety, it is recommended that a copy is archived in a folder not used by this function.}
#'
#' @examples
setModelCovars <- function(thisModel, thisTaxon = NULL, envDataPath = NULL, qDataPath = NULL, numQfiles = 0, newThreshold = NULL, trace = FALSE)
{
  if (!(file.exists(thisModel))) stop("setModelCovars: Cannot find GDM object.")

  if (is.null(thisTaxon)) stop("setModelCovars: thisTaxon cannot be NULL (default).")

  if (is.null(envDataPath)) envDataPath <- "/home/RandR/envData/eastOZ"

  if ((!is.null(envDataPath)) && (!dir.exists(envDataPath))) stop("setModelCovars: Cannot find envDataPath")

  if (is.null(qDataPath)) qDataPath <- "/home/RandR/qData/eastOZ"

  if ((!is.null(envDataPath)) && (!dir.exists(qDataPath))) stop("setModelCovars: Cannot find qDataPath")

  if (!is.null(newThreshold))
  {
    if ((newThreshold > 1) || (newThreshold < 0)) stop("setModelCovars: newThreshold parameter must be a numeric value between 0 and 1.")
  }

  load(thisModel)

  # Jason calls the model object "md", so we will assume that this is the name retrieved by a successful call to load()

  if (!("gdm" %in% class(md$model))) stop("setModelCovars: The GDM model object is malformed.")

  cat("setModelCovars: R&R Webtool GDM Model:", basename(thisModel), "\n")
  cat("\tmodel_predictors:", paste0(md$model$predictors, collapse = "; "),"\n")

  failure <- FALSE

  cat("\tHas S:",md$S,"\n")
  cat("\t\tsrast path:", md$sdata@layers[[1]]@file@name,"\n")

  cat("\tHas Q:",md$Q,"\n")
  if (md$Q)
  {
    cat("\t\tIdentifying Q-files:") #, paste0(md$model$qdata, collapse = "; "),"\n")

    baseQnames <- md$model$predictors[grep("^Qprops", md$model$predictors)]
    if (trace)
    {
      cat("\n==========================================\n")
      print(baseQnames)
      cat("==========================================\n")
    }

    Qpaths <- paste0(qDataPath, "/", gsub(" ", "_", thisTaxon), "_", baseQnames, ".tif")
    if (trace)
    {
      cat("\n==========================================\n")
      print(Qpaths)
      cat("==========================================\n")
    }

    if (all(file.exists(Qpaths)))
    {
      md$qdata <- raster::stack(Qpaths)
      cat(" found", length(baseQnames), "Q-files; paths set OK.\n")
    }
    else
    {
      cat(" FAILED - one or more Q-files not found in", qDataPath, "\n")
      failure <- TRUE
    }
  }

  cat("\tHas E:",md$E,"\n")
  if (md$E)
  {
    cat("\t\tIdentifying E-files:") #, paste0(md$model$qdata, collapse = "; "),"\n")

    # Assume that if there are any covariates named which are not "Geographic"
    # or begin with "Qprops", they represent the names of general environmental
    # covariates:
    baseEnames <- md$model$predictors[-grep("^Geographic|^Qprops", md$model$predictors)]

    if (length(baseEnames) == 0)
    {
      stop("setModelCovars: Model object shows 'E' == TRUE but no environmental covariates can be identified in model predictor names.")
    }
    else
    {
      Epaths <- paste0(envDataPatch, "/", baseEnames, ".tif")
      print(Epaths)

      if (all(file.exists(Epaths)))
      {
        md$edata <- raster::stack(Epaths)
        cat(" paths set OK.\n")
      }
      else
      {
        cat(" FAILED - one or more E-files not found in ", envDataPath, "\n")
        failure <- TRUE
      }
    }
  }

  if (!is.null(newThreshold))
  {
    md$threshold <- newThreshold
    cat(" Set threshold to new value.\n")
  }

  if (!failure)
  {
    save(md, file = thisModel)
    cat("\tUpdated model objects saved.\n\n")
  }
}
