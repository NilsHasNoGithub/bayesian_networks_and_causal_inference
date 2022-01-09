options(error = function() {
  calls <- sys.calls()
  if (length(calls) >= 2L) {
    sink(stderr())
    on.exit(sink(NULL))
    cat("Backtrace:\n")
    calls <- rev(calls[-length(calls)])
    for (i in seq_along(calls)) {
      cat(i, ": ", deparse(calls[[i]], nlines = 1L), "\n", sep = "")
    }
  }
  if (!interactive()) {
    q(status = 1)
  }
})

install_if_not_installed <- function(pack) {
  if(!require(pack)) {
    install.packages(pack)
  }
}

mk_output_path <- function(name, ex = ".") {
    d <- paste0(ex, "/out")
    dir.create(d, showWarnings = FALSE)
    return(paste0(d, "/", name))
}

write_to_file <- function(fp, string, append = FALSE) {
    if(append) {
        open <- "a"
    } else {
        open <- "w"
    }
    fileConn <- file(fp, open = open)
    writeLines(string, fileConn)
    close(fileConn)
}

clear_file <- function(fp) {
    fileConn <- file(fp, open = "w")
    # write("", fileConn)
    writeLines("", fileConn)
    close(fileConn)
}

library(readr)

read_to_string <- function(file_path) {
  return(read_file(file_path))
}

print_to_file <- function(f, obj, append = FALSE) {
    
    capture.output(obj, file=f, append=append)
    # obj_str <- capture.output(obj, file=f)
    
    # Write obj to file
    # write(obj_str, f)
}

standardize_0_1 <- function(x) {
    min_ <- min(x)
    max_ <- max(x)
    return((x - min_) / (max_ - min_))
}