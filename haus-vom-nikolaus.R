#     (3)
#     / \
#    /   \
#   /     \
# (2)-----(4)
#  | \   / |
#  |  \ /  |
#  |   x   |
#  |  / \  |
#  | /   \ |
# (1)-----(5)

links <- c("12", "14", "15", "23", "24", "25", "34", "45")

go <- function(path, seen = stats::setNames(logical(length(links)), links)) {
  
  if (length(path) == 9L) {
    cat(paste(path, collapse = "-"), "\n")
    return()
  }
  
  a <- path[length(path)]
  
  for (b in gsub(a, "", grep(a, names(which(! seen)), value = TRUE))) {
    go(c(path, b), `[<-`(seen, paste0(min(a, b), max(a, b)), TRUE))
  }
}

result <- capture.output(for (i in 1:5) go(i))
writeLines(sprintf("%02d: %s", seq_along(result), result))
