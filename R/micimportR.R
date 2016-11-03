#' Micrograph importer
#'
#' This function imports micrographs with EBImage and RBioFormats. It outputs a named list with 3 elements:
#' Data frames of the images, name of each image extracted from metadata and the base name of the project.
#' It is recommended to run one of the analysis functions after this.
#' @param dir root directory for image importing (does not support subdirectories)
#' @keywords microscopR
#' @export
#' @examples
#' micimportR("/home/files/software/data/mouse-kidney")

micimportR <- function(dir) {
  #check if a dataset is already loaded in R buffer, if yes: remove
  if (exists("micimportR_list") == TRUE)
  {
    rm("micimportR_list", envir = .GlobalEnv)
  }

  #Set working directory to directory supplied, use directory name as filename for output files later
  folder_name <- basename(dir)
  setwd(dir)

  #Dependencies
  require("rJava")
  require("devtools")
  require("EBImage")
  require("RBioFormats")
  require("tools")
  require("data.table")
  require("parallel")

  #Read image file list
  input_files <- dir(pattern = ".oif$|.lif$|.lsm$|.tif$|.czi$")

  #set up parallel computing for image reading optimization
  total_cores <- detectCores() - 1
  cl <- makeCluster(total_cores)

  #initialize variable for metadata output
  imgmeta <- list()

  #import files, each block handles a different filetype due to metadata differences.
  #each block is the same aside from filetype idiosyncrasies, therefore only 1 is documented

  #make sure only files of the same filetype are imported
  if (sum(grepl(".oif$", input_files)) / length(input_files) == 1) {
    #parallelized reading of images
    images_list <- parLapply(cl, input_files, read.image)
    #initialize variable to write metadata to
    oif_meta <- list()
    #write all metadata to list in the same order as the images
    for (i in seq_along(images_list)) {
      oif_meta[[i]] <- globalMetadata(images_list[[i]])
      #extract image name from the metadata
      imgmeta[[i]] <- file_path_sans_ext(oif_meta[[i]]$`[File Info] DataName`)
    }
  } else if (sum(grepl(".lif$", input_files)) / length(input_files) == 1) {
    #parallel reading not applied as .lifs are single files
    images_list <- read.image(input_files)
    lif_meta <- list()
    for (i in seq_along(images_list)) {
      lif_meta[[i]] <- seriesMetadata(images_list[[i]])
      imgmeta[[i]] <- lif_meta[[i]]$`Image name`
    }
  } else if (sum(grepl(".lsm$", input_files)) / length(input_files) == 1) {
    images_list <- parLapply(cl, input_files, read.image)
    lsm_meta <- list()
    for (i in seq_along(images_list)) {
      lsm_meta[[i]] <- seriesMetadata(images_list[[i]])
      imgmeta[[i]] <- lsm_meta[[i]]$`Recording Name #1`
    }
  } else if (sum(grepl(".czi$", input_files)) / length(input_files) == 1) {
    images_list <- parLapply(cl, input_files, read.image)
    czi_meta <- list()
    for (i in seq_along(images_list)) {
      czi_meta[[i]] <- globalMetadata(images_list[[i]])
      imgmeta[[i]] <- czi_meta[[i]]$`Information|Document|Name #1`
    }
  } else if (sum(grepl(".tif$", input_files)) / length(input_files) == 1) {
    images_list <- parLapply(cl, input_files, read.image)
    tif_meta <- list()
    for (i in seq_along(images_list)) {
      imgmeta[[i]] <- file_path_sans_ext(input_files[[i]])
          }
  } else {
    stop("Only upload files seperately.")
  }

  #Compile all extracted data and store in a global variable so other functions will be able to use it
  micimportR_list <<- list("images_list" = images_list, "imgmeta" = imgmeta, "folder_name" = folder_name)
  #stop parallel cluster
  stopCluster(cl)
}
