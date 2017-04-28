## ------------------------------------------------------------------------
list(1, 2, 3, 4)

## ------------------------------------------------------------------------
1:4

## ------------------------------------------------------------------------
as.list(1:4)

## ------------------------------------------------------------------------
list(1:4)

## ------------------------------------------------------------------------
is_even <- function(x) x %% 2 == 0
unlist(Filter(is_even, 1:10))

## ------------------------------------------------------------------------
larger_than <- function(x) function(y) y > x
unlist(Filter(larger_than(5), 1:10))

## ---- echo=FALSE---------------------------------------------------------
curry2 <- function(f) function(x) function(y) f(x, y)

## ------------------------------------------------------------------------
unlist(Filter(curry2(`<`)(5), 1:10))
unlist(Filter(curry2(`>=`)(5), 1:10))

## ------------------------------------------------------------------------
rcurry2 <- function(f) function(y) function(x) f(x, y)
unlist(Filter(rcurry2(`>=`)(5), 1:10))
unlist(Filter(rcurry2(`<`)(5), 1:10))

## ------------------------------------------------------------------------
s <- list(a = 1:10, b = list(1,2,3,4,5,6), 
          c = y ~ x1 + x2 + x3, d = vector("numeric"))
Filter(function(x) length(x) > 5, s)

## ------------------------------------------------------------------------
unlist(Map(is_even, 1:5))

## ------------------------------------------------------------------------
add <- function(x) function(y) x + y
unlist(Map(add(2), 1:5))
unlist(Map(add(3), 1:5))

## ------------------------------------------------------------------------
s <- list(a = 1:10, b = list(1,2,3,4,5,6), 
          c = y ~ x1 + x2 + x3, d = vector("numeric"))
unlist(Map(length, s))

## ------------------------------------------------------------------------
unlist(Map(`+`, 1:5, 1:5))

## ------------------------------------------------------------------------
x <- 1:10
y <- c(NA, x)
s <- list(x = x, y = y)
unlist(Map(mean, s))
unlist(Map(mean, s, na.rm = TRUE))

## ------------------------------------------------------------------------
unlist(Map(mean, s, MoreArgs = list(na.rm = TRUE)))

## ------------------------------------------------------------------------
scale <- function(x, y) (x - mean(y))/sd(y)

## ------------------------------------------------------------------------
unlist(Map(scale, 1:10, 1:5))

## ------------------------------------------------------------------------
unlist(Map(scale, 1:10, y = 1:5))

## ------------------------------------------------------------------------
unlist(Map(scale, 1:10, MoreArgs = list(y = 1:5)))

## ------------------------------------------------------------------------
s <- list(a = 1:10, b = list(1,2,3,4,5,6), 
          c = y ~ x1 + x2 + x3, d = vector("numeric"))
unlist(Map(length, s))

## ------------------------------------------------------------------------
Reduce(`+`, 1:5)

## ------------------------------------------------------------------------
Reduce(`+`, 1:5, accumulate = TRUE)

## ------------------------------------------------------------------------
Reduce(`+`, 1:5, right = TRUE, accumulate = TRUE)

## ------------------------------------------------------------------------
Reduce(`+`, 1:5, init = 10, accumulate = TRUE)

## ------------------------------------------------------------------------
Reduce(`*`, 1:5)
Reduce(`*`, 1:5, accumulate = TRUE)
Reduce(`*`, 1:5, right = TRUE, accumulate = TRUE)

## ------------------------------------------------------------------------
samples <- replicate(3, sample(1:10, replace = TRUE), 
                                     simplify = FALSE)
str(samples)
Reduce(intersect, samples)

## ---- echo=FALSE---------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL) 
  list(name = name, left = left, right = right)

print_tree <- function(tree) {
  build_string <- function(node) {
    if (is.null(node$left) && is.null(node$right)) {
        node$name
    } else {
        left <- build_string(node$left)
        right <- build_string(node$right)
        paste0("(", left, ",", right, ")")
    }
  }
  build_string(tree)
}

size_of_tree <- function(node) {
  if (is.null(node$left) && is.null(node$right)) {
    size <- 1
  } else {
    left_size <- size_of_tree(node$left)
    right_size <- size_of_tree(node$right)
    size <- left_size + right_size + 1
  }
  size
}

## ------------------------------------------------------------------------
A <- make_node("A")
C <- make_node("C", make_node("A"), 
                    make_node("B"))
E <- make_node("E", 
               make_node("C", make_node("A"), make_node("B")),
               make_node("D"))

trees <- list(A = A, C = C, E = E)

## ------------------------------------------------------------------------
trees[[2]]
unlist(trees[[2]])
print_tree(trees[[2]])

## ------------------------------------------------------------------------
Map(print_tree, trees)
unlist(Map(print_tree, trees))

## ------------------------------------------------------------------------
unlist(Map(print_tree, 
           Filter(function(tree) size_of_tree(tree) > 1, trees)))

## ------------------------------------------------------------------------
unlist(Map(size_of_tree, trees))
Reduce(`+`, Map(size_of_tree, trees), 0)

## ---- echo=FALSE---------------------------------------------------------
node_depth <- function(tree, name, depth = 0) {
  if (is.null(tree))     return(NA)
  if (tree$name == name) return(depth)
  
  left <- node_depth(tree$left, name, depth + 1)
  if (!is.na(left)) return(left)
  right <- node_depth(tree$right, name, depth + 1)
  return(right)
}

## ------------------------------------------------------------------------
node_depth_B <- function(tree) node_depth(tree, "B")
unlist(Map(node_depth_B, trees))

## ------------------------------------------------------------------------
unlist(Map(node_depth_B, trees), use.names = FALSE)

## ------------------------------------------------------------------------
Filter(function(x) !is.na(x), 
       unlist(Map(node_depth_B, trees), use.names = FALSE))

## ------------------------------------------------------------------------
has_B <- function(node) {
  if (node$name == "B") return(TRUE)
  if (is.null(node$left) && is.null(node$right)) return(FALSE)
  has_B(node$left) || has_B(node$right)
}
unlist(Map(node_depth_B, Filter(has_B, trees)), use.names = FALSE)

## ------------------------------------------------------------------------
sapply(trees, size_of_tree)
sapply(trees, identity)

## ------------------------------------------------------------------------
vapply(trees, size_of_tree, 1)

## ------------------------------------------------------------------------
lapply(trees, size_of_tree)

## ------------------------------------------------------------------------
(m <- matrix(1:6, nrow=2, byrow=TRUE))

## ------------------------------------------------------------------------
collaps_input <- function(x) paste(x, collapse = ":")

## ------------------------------------------------------------------------
apply(m, 1, collaps_input)

## ------------------------------------------------------------------------
apply(m, 2, collaps_input)

## ------------------------------------------------------------------------
apply(m, c(1, 2), collaps_input)

## ------------------------------------------------------------------------
(x <- rnorm(10))
(categories <- sample(c("A", "B", "C"), size = 10, replace = TRUE))
tapply(x, categories, mean)

## ------------------------------------------------------------------------
(categories2 <- sample(c("X", "Y"), size = 10, replace = TRUE))
tapply(x, list(categories, categories2), mean)

## ------------------------------------------------------------------------
library(purrr)

## ------------------------------------------------------------------------
keep(1:5, rcurry2(`>`)(3))
discard(1:5, rcurry2(`>`)(3))

## ------------------------------------------------------------------------
keep(as.list(1:5), rcurry2(`>`)(3))

## ------------------------------------------------------------------------
every(1:5, rcurry2(`>`)(0))
every(1:5, rcurry2(`>`)(3))
some(1:5, rcurry2(`>`)(3))
some(1:5, rcurry2(`>`)(6))

## ------------------------------------------------------------------------
keep(1:5, ~ .x > 3)
discard(1:5, ~ .x > 3)

## ------------------------------------------------------------------------
map(1:5, ~ .x + 2)
map_dbl(1:5, ~ .x + 2)

## ------------------------------------------------------------------------
map2(1:5, 6:10, ~ 2 * .x + .y)
map2_dbl(1:5, 6:10, ~ 2 * .x + .y)

## ------------------------------------------------------------------------
pmap(list(1:5, 6:10, 11:15), 
     function(x, y, z) x + y + z)
pmap_dbl(list(1:5, 6:10, 11:15),
         function(x, y, z) x + y + z)

## ------------------------------------------------------------------------
unlist(map_if(1:5, ~ .x %% 2 == 1, ~ 2*.x))

## ------------------------------------------------------------------------
map_chr(map(keep(trees, ~ size_of_tree(.x) > 1), "left"),
        print_tree)

## ------------------------------------------------------------------------
reduce(1:5, `+`)
reduce_right(1:5, `*`)

