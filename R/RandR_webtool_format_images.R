
#' Format species information images
#'
#' Apply operations such as resizing or padding (with black) so that jpg images to accompany species information are correctly and uniformly formatted.
#'
#' @param sourceFolder Character. Full path to the folder in which jpg images for processing will be found.
#' @param outputFolder Character. Full path to the folder to store processed versions of input images. If the folder is not present, it will be created.
#'
#' @return NULL
#' @export
#'
#' @details { JPG image files which are shown as part of species information on the webtool have a target format of 576 pixels (width) x 500 pixels (height), and an aspect ratio of 1.152. Images are resized using the original image's aspect ratio to determine if the target will be width or height; the image aspect ratio is 'respected' during the resizing. If padding must be applied to acheive the target image dimensions, this is applied symmetrically on all axes. The padding colour is 'black'.}
#'
#' @examples
#' \dontrun{}
formatInfoImages <- function(sourceFolder = NULL, outputFolder = NULL)
{
  #### These values should be global constants:
  targetWidth <- 576 # px
  targetHeight <- 500 # px
  targetAspectRatio <- targetWidth/targetHeight


  if (is.null(sourceFolder)) stop("formatInfoImages: sourceFolder is NULL. Please supply a folder path.")

  if (is.null(outputFolder))
    stop("formatInfoImages: outputFolder is NULL. Please supply a folder path.")
  else
  {
    if (!dir.exists(outputFolder)) dir.create(outputFolder)
  }

  if (!dir.exists(sourceFolder)) stop("formatInfoImages: Folder path supplied in sourceFolder cannot be found.")

  theImages <- list.files(sourceFolder, "*.jpg", full.names = TRUE)

  if (length(theImages) == 0)
  {
    stop("formatInfoImages: No JPG files found in source folder.")
  }
  else
  {
    cat("formatInfoImages:", length(theImages), "images will be processed:\n")

    for (thisPic in theImages)
    {
      cat("  ", basename(thisPic), ": ")

      pic <- imager::load.image(thisPic)

      pic_width <- dim(pic)[1]
      pic_height <- dim(pic)[2]

      aspectRatio <- pic_width/pic_height

      cat(pic_width, "(W) x", pic_height, "(H); aspect_ratio =",aspectRatio,": ")

      if (aspectRatio > targetAspectRatio)
      {
        #newWidth <- targetWidth
        #newHeight <- pic_width/aspectRatio
        pic <- imager::resize(pic, size_x = targetWidth, size_y = targetWidth/aspectRatio, interpolation_type = 5) #quality = 0.7)
      }
      else
      {
        #newWidth <- pic_width * aspectRatio
        #newHeight <- targetHeight
        pic <- imager::resize(pic, size_x = targetWidth * aspectRatio, size_y = targetHeight, interpolation_type = 5) #, quality = 0.7)
      }

      # Re-compute pic_width and pic_height after rescaling...
      pic_width <- dim(pic)[1]
      pic_height <- dim(pic)[2]

      if (pic_width < targetWidth)
      {
        cat("padding width, ")

        delta <- targetWidth - pic_width
        #cat(basename(thisPic),":",delta,"\n")

        pic <- imager::pad(pic, delta, "x")
      }

      if (pic_height < targetHeight)
      {
        cat("padding height, ")

        delta <- targetHeight - pic_height
        #cat(basename(thisPic),":",delta,"\n")

        pic <- imager::pad(pic, delta, "y")
      }

      imager::save.image(pic, paste0(outputFolder, "/", basename(thisPic)), quality = 0.8)
      cat("SAVED\n")
    }
  }

  cat("**** formatInfoImages: End of processing.\n")
}
