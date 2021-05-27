#' Used to check if the data, model and stats directory exist
#'
#' @param ddir1 The path to the data1 dir
#' @param ddir2 The path to the data2 dir
#' @param mdir The path to the model dir
#' @param msdir The path to the models dir
#' @param sdir The path to the stats dir
check_dirs <- function(ddir1, ddir2, mdir, msdir, sdir) {
    # The base directory
    bdir <- gsub("/data1", "", ddir1)
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
    # The file containing the download urls is read
    con <- file(paste0(bdir, "/urls.txt"))
    lines <- readLines(con)
    close(con)
    # Each line is parsed and the url is downloaded to the given folders
    for (l in lines) {
        # The line is parsed
        vals <- strsplit(l, " ")
        # The download url
        url <- vals[[1]][1]
        # The output file name
        ofn <- vals[[1]][2]
        # The output folder name
        odn <- paste0(bdir, "/", vals[[1]][3])
        # If the file does not exist
        if (!file.exists(paste0(odn, "/", ofn))) {
            # The file is downloaded from Google Drive to the given directory
            download.file(url, paste0(odn, "/", ofn), quiet = T)
        }
        # If the optional copy folder name is given
        if (!is.na(vals[[1]][4])) {
            # The optional extra folder to copy the file to
            ocdn <- paste0(bdir, "/", vals[[1]][4])
            ## If the file does not exist
            if (!file.exists(paste0(ocdn, "/", ofn))) {
                # The file is also copied to the extra dir
                file.copy(paste0(odn, "/", ofn), ocdn)
            }
        }
    }
}
