#' Used to check if the data, model and stats directory exist
#'
#' @param ddir1 The path to the data1 dir
#' @param ddir2 The path to the data2 dir
#' @param mdir The path to the model dir
#' @param msdir The path to the models dir
#' @param sdir The path to the stats dir
check_dirs <- function(ddir1, ddir2, mdir, msdir, sdir) {
    # The list of directories
    dirs <- c(ddir1, ddir2, mdir, msdir, sdir)
    # Each directory is created if it does not exist
    for (d in dirs) {
        # If dir does not exist
        if (!dir.exists(d)) {
            # The directory is created
            dir.create(d)
        }
    }
    # The path to the input file name
    ifn <- paste0(ddir1, "/input.txt")
    # If the input.txt file does not exist
    if (!file.exists(ifn)) {
        # The url of the input.txt file
        url <- paste0("https://drive.google.com/uc?export=download")
        url <- paste0(url, "&id=1Ddggtt1vXlq46-ymGJvpooM4oh_vnsdL")
        # The input.txt file is downloaded from Google Drive to ddir1
        download.file(url, ifn, quiet = T)
        # The input.txt file is also copied to ddir2
        file.copy(ifn, ddir2)
    }
}
