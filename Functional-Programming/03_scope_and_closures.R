## ------------------------------------------------------------------------
x <- 2 ; y <- 2
x + y

## ------------------------------------------------------------------------
quote(x + y)

## ------------------------------------------------------------------------
eval(x + y)

## ------------------------------------------------------------------------
eval(quote(x + y))

## ------------------------------------------------------------------------
env <- new.env()
env$x <- 4

## ------------------------------------------------------------------------
env <- list2env(list(x = 4))

## ------------------------------------------------------------------------
eval(x + y, env)

## ------------------------------------------------------------------------
eval(quote(x + y), env)

## ---- echo=FALSE---------------------------------------------------------
rm(x) ; rm(y) ; rm(env)

## ------------------------------------------------------------------------
y <- 2
f <- function(x) {
    result <- x + y
    y <- 3
    return(result)
}
f(2)

## ------------------------------------------------------------------------
f <- function(condx, x, dondy, y) {
    if (condx) x <- 2
    if (condy) y <- 2
    x + y
}

## ------------------------------------------------------------------------
f <- function(x, y = 2 * x) x + y

## ------------------------------------------------------------------------
f(x = 2)

## ------------------------------------------------------------------------
f <- function(x) {
    g <- function(y) x + y
    g(x)
}
f(2)

## ------------------------------------------------------------------------
f <- function(x) {
    g <- function(y) x + y
    g
}
h <- f(2)
h(2)

## ------------------------------------------------------------------------
h1 <- f(1)
h2 <- f(2)

## ------------------------------------------------------------------------
gg <- function(ff) ff(1)
gg(h1)
gg(h2)

## ------------------------------------------------------------------------
make_adder <- function(x) {
    add_y <- function(y) x + y
    add_y
}
add1 <- make_adder(1)
add2 <- make_adder(2)

## ------------------------------------------------------------------------
make_counter <- function() {
    x <- 0
    count <- function() {
        x <- x + 1
        x
    }
}
counter <- make_counter()

## ------------------------------------------------------------------------
counter()
counter()
counter()

## ------------------------------------------------------------------------
make_counter <- function() {
    x <- 0
    count <- function() {
        x <<- x + 1
        x
    }
}
counter <- make_counter()
counter()
counter()
counter()

## ------------------------------------------------------------------------
depth_first_numbers <- function(tree) {
  table <- c()
  counter <- make_counter()

  traverse_tree <- function(node) {
    if (is.null(node$left) && is.null(node$right)) {
      dfn <- counter()
      node$range <- c(dfn, dfn)
      table[node$name] <<- dfn
      node
    
    } else {
      left <- traverse_tree(node$left)
      right <- traverse_tree(node$right)
      new_node <- make_node(node$name, left, right)
      new_node$range <- c(left$range[1], right$range[2])
      new_node
    }
  }

  new_tree <- traverse_tree(tree)
  list(tree = new_tree, table = table)
}

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

tree <- make_node("root", 
                  make_node("C", make_node("A"), 
                                 make_node("B")),
                  make_node("D"))

## ------------------------------------------------------------------------
result <- depth_first_numbers(tree)
print_tree(result$tree)
result$table

## ------------------------------------------------------------------------
x <- 2; y <- 2
eval(quote(x + y))

## ------------------------------------------------------------------------
rm(x); rm(y)
f <- function() {
    x <- 2; y <- 2
    eval(quote(x + y))
}
f()

## ------------------------------------------------------------------------
f <- function(x) {
  x <- x
  g <- function(y) {
    y
    eval(quote(x + y))
  }
  g(2)
}
f(2)

