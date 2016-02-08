nl2 <- function(v) {
  v / sqrt(v[1]^2 + v[2]^2)
}

th0 <- 15
ab0 <- c(0, 5)
gr0 <- nl2(c(-45,  30) - ab0)
ye0 <- nl2(c(  0,  80) - ab0)
bu0 <- nl2(c(  0, -40) - ab0)
re0 <- nl2(c( 60,  50) - ab0)

f <- function(l, a, b) {
  gr <- (c(a, b) - ab0) %*% gr0
  ye <- (c(a, b) - ab0) %*% ye0
  bu <- (c(a, b) - ab0) %*% bu0
  re <- (c(a, b) - ab0) %*% re0
  be <- max(gr, ye, bu, re)
  if (be < th0) {
    if (l < 30)
      "black"
    else
      "gray"
  } else if (gr == be)
    "green"
  else if (ye == be)
    "yellow"
  else if (bu == be)
    "blue"
  else if (re == be)
    "red"
  else
    "gray"
}


w <- fread("subset.tsv")
wc0 <- rgb(w$Red / 255, w$Green / 255, w$Blue / 255)
wc1 <- w[, f(`L`, `A`, `B`), by = 1:nrow(w)]$V1
pairs(w[, .(L, A, B)], col=wc0, pch=".")
pairs(w[, .(L, A, B)], col=wc1, pch=".")
