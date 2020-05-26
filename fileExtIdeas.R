
# https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
# last accessed 2019-03-07


########################################################
# answered Aug 2 '17 at 17:20
#
# Pisca46
# The regexpr above fails if the extension contains non-alnum (see e.g. https://en.wikipedia.org/wiki/List_of_filename_extensions) As an altenative one may use the following function:

  getFileNameExtension <- function (fn) {
    # remove a path
    splitted    <- strsplit(x=fn, split='/')[[1]]
    # or use .Platform$file.sep in stead of '/'
    fn          <- splitted [length(splitted)]
    ext         <- ''
    splitted    <- strsplit(x=fn, split='\\.')[[1]]
    l           <-length (splitted)
    if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l]
    # the extention must be the suffix of a non-empty name
    ext
  }


  ############################ NOTE ABOVE ANSWER RE USE OF    .Platform$file.sep

  ### And see tools::file_ext()


########################################################
# answered Sep 26 '18 at 7:06
#
# Miguel Vazq

# simple function with no package to load :

  getExtension <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
  }


