
if (.Platform$OS.type == "windows") {
  is_64 = grepl("x86_64", R.version$arch)
  if (is_64) {
    limit = 10000
  } else {
    limit = 4000
  }
  memory.limit(max(limit, memory.limit()))
}

destroy_field = function(path, field = "Acceleration Scale",
                         replace_field = list()) {
  path <- read.gt3x:::unzip_zipped_gt3x(path, cleanup = TRUE)

  tdir = tempfile()
  out = unzip(path, exdir = tdir, overwrite = TRUE)
  info_file = out[grepl("info.txt", out)]

  if (all(field %in% "all")) {
    out = out[!grepl("info.txt", out)]
  } else {
    info = readLines(info_file)
    info
    keys = sapply(strsplit(info, split = ":"), `[`, 1)
    info = info[!keys %in% field]
    if (length(replace_field) >0) {
      for (ifield in seq_along(replace_field)) {
        n =  names(replace_field)[ifield]
        ind = which(keys == n)
        info[ind] = paste0(n, ": ", replace_field[[ifield]])
      }
    }
    writeLines(info, info_file)
  }
  new_gt3x_file = tempfile(fileext = ".gt3x")
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(files = basename(out),
             zipfile = new_gt3x_file,
             root = tdir,
             include_directories = FALSE)
  }
  return(new_gt3x_file)
}
