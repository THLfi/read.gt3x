library(testthat)
library(read.gt3x)

if (.Platform$OS.type == "windows") {
  is_64 = grepl("x86_64", R.version$arch)
  if (is_64) {
    limit = 10000
  } else {
    limit = 4000
  }
  memory.limit(max(limit, memory.limit()))
}

test_check("read.gt3x")
