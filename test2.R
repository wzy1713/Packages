
test <- function(i, j) {
  for (k in 1:i) {
    for (n in 1:j) {
      print(k*n)
    }
  }
}

argv <- commandArgs(TRUE)
x <- as.numeric(argv[1])
y <- as.numeric(argv[2])

test(x,y)