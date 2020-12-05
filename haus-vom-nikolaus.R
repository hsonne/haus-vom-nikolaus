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

go <- function(path, seen = logical(length(links))) {  
  
  if (length(path) == 9L) {
    cat(paste(path, collapse = "-"), "\n")
    return()
  }
  
  a <- path[length(path)]
  
  for (link in grep(a, links[! seen], value = TRUE)) {
    go(c(path, gsub(a, "", link)), `[<-`(seen, links == link, TRUE))
  }
}

result <- capture.output(for (i in 1:5) go(i))
writeLines(sprintf("%02d: %s", seq_along(result), result))
