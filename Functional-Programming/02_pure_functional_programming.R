## ---- echo=FALSE---------------------------------------------------------
is_empty <- function(x) length(x) == 0
first <- function(x) x[1]
rest <- function(x) {
    if (length(x) == 1) NULL
    else x[2:length(x)]
}

## ------------------------------------------------------------------------
lin_search <- function(element, sequence) {
    if (is_empty(sequence))              FALSE
    else if (first(sequence) == element) TRUE
    else lin_search(element, rest(sequence))
}

x <- 1:5
lin_search(0, x)
lin_search(1, x)
lin_search(5, x)
lin_search(6, x)

## ------------------------------------------------------------------------
is_empty <- function(x) length(x) == 0
first <- function(x) x[1]
rest <- function(x) {
    if (length(x) == 1) NULL else x[2:length(x)]
}

## ------------------------------------------------------------------------
next_list <- function(element, rest = NULL)
    list(element = element, rest = rest)

## ------------------------------------------------------------------------
x <- next_list(1, 
               next_list(2, 
                         next_list(3, 
                                   next_list(4))))


## ------------------------------------------------------------------------
nl_is_empty <- function(nl) is.null(nl)
nl_first <- function(nl) nl$element
nl_rest <- function(nl) nl$rest

## ------------------------------------------------------------------------
nl_lin_search <- function(element, sequence) {
    if (nl_is_empty(sequence))              FALSE
    else if (nl_first(sequence) == element) TRUE
    else nl_lin_search(element, nl_rest(sequence))
}

## ------------------------------------------------------------------------
vector_to_next_list <- function(x) {
    if (is_empty(x)) NULL
    else next_list(first(x), vector_to_next_list(rest(x)))
}

## ------------------------------------------------------------------------
i_is_empty <- function(x, i) i > length(x)
i_first <- function(x, i) x[i]

## ------------------------------------------------------------------------
i_vector_to_next_list <- function(x, i = 1) {
    if (i_is_empty(x, i)) NULL
    else next_list(i_first(x, i), i_vector_to_next_list(x, i + 1))
}

## ------------------------------------------------------------------------
i_lin_search <- function(element, sequence, i = 1) {
    if (i_is_empty(sequence, i))              FALSE
    else if (i_first(sequence, i) == element) TRUE
    else i_lin_search(element, sequence, i + 1)
}

## ------------------------------------------------------------------------
lin_search <- function(element, sequence, i = 1) {
    if (i > length(sequence)) FALSE
    else if (sequence[i] == element) TRUE
    else lin_search(element, sequence, i + 1)
}

## ---- echo=FALSE---------------------------------------------------------
assert(lin_search(0, 1:5) == FALSE)
assert(lin_search(1, 1:5) == TRUE)

## ------------------------------------------------------------------------
binary_search <- function(element, x, 
                          first = 1, last = length(x)) {

    if (last < first) return(FALSE) # empty sequence
  
    middle <- (last - first) %/% 2 + first
    if (element == x[middle]) {
        TRUE
    } else if (element < x[middle]) {
        binary_search(element, x, first, middle)
    } else {
        binary_search(element, x, middle, last)
    }
}

## ------------------------------------------------------------------------
binary_search <- function(element, x, 
                          first = 1, last = length(x)) {

    if (last < first) return(FALSE) # empty sequence
  
    middle <- (last - first) %/% 2 + first
    if (element == x[middle]) {
        TRUE
    } else if (element < x[middle]) {
        binary_search(element, x, first, middle - 1)
    } else {
        binary_search(element, x, middle + 1, last)
    }
}

## ---- echo=FALSE---------------------------------------------------------
assert(binary_search(0, 1:5) == FALSE)
assert(binary_search(1, 1:5) == TRUE)
assert(binary_search(2, 1:5) == TRUE)
assert(binary_search(3, 1:5) == TRUE)
assert(binary_search(4, 1:5) == TRUE)
assert(binary_search(5, 1:5) == TRUE)
assert(binary_search(6, 1:5) == FALSE)

## ------------------------------------------------------------------------
node_depth <- function(tree, name, depth = 0) {
    if (is.null(tree))     return(NA)
    if (tree$name == name) return(depth)

    left <- node_depth(tree$left, name, depth + 1)
    if (!is.na(left)) return(left)
    right <- node_depth(tree$right, name, depth + 1)
    return(right)
}

## ------------------------------------------------------------------------
factorial <- function(n) {
    if (n == 1) 1
    else n * factorial(n - 1)
}

## ------------------------------------------------------------------------
nl_rm_duplicates <- function(x) {
    if (is.null(x)) return(NULL)
    else if (is.null(x$rest)) return(x)

    rest <- nl_rm_duplicates(x$rest)
    if (x$element == rest$element) rest
    else next_list(x$element, rest)
}

(x <- next_list(1, next_list(1, next_list(2, next_list(2)))))
nl_rm_duplicates(x)

## ---- echo=FALSE---------------------------------------------------------
find_duplicates <- which %.% duplicated

## ------------------------------------------------------------------------
vector_rm_duplicates <- function(x) {
    dup <- find_duplicates(x)
    x[-dup]
}
vector_rm_duplicates(c(1, 1, 2, 2))

## ---- echo=FALSE---------------------------------------------------------
builtin_find_duplicates <- which %.% duplicated

## ------------------------------------------------------------------------
find_duplicates <- function(x, i = 1) {
    if (i >= length(x)) return(c())

    rest <- find_duplicates(x, i + 1)
    if (x[i] == x[i + 1]) c(i, rest)
    else rest
}

## ---- echo=FALSE---------------------------------------------------------
x <- c(1,1,2,3,4,4)
assert(all(builtin_find_duplicates(x)-1 == find_duplicates(x)))

## ------------------------------------------------------------------------
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

## ---- echo=FALSE---------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL) 
  list(name = name, left = left, right = right)

## ------------------------------------------------------------------------
tree <- make_node("root", 
                  make_node("C", make_node("A"), 
                                 make_node("B")),
                  make_node("D"))

size_of_tree(tree)

## ------------------------------------------------------------------------
set_size_of_subtrees <- function(node) {
  if (is.null(node$left) && is.null(node$right)) {
    node$size <- 1
  } else {
    left_size <- set_size_of_subtrees(node$left)
    right_size <- set_size_of_subtrees(node$right)
    node$size <- left_size + right_size + 1
  }
  node$size
}

## ------------------------------------------------------------------------
set_size_of_subtrees(tree)
tree$size

## ------------------------------------------------------------------------
set_size_of_subtrees <- function(node) {
  if (is.null(node$left) && is.null(node$right)) {
    node$size <- 1
  } else {
    left <- set_size_of_subtrees(node$left)
    right <- set_size_of_subtrees(node$right)
    node$size <- left$size + right$size + 1
  }
  node
}

tree <- set_size_of_subtrees(tree)
tree$size

## ------------------------------------------------------------------------
depth_first_numbers <- function(node, dfn = 1) {
  if (is.null(node$left) && is.null(node$right)) {
    node$range <- c(dfn, dfn)
    new_table <- table
    table <- c()
    table[node$name] <- dfn
    list(node = node, new_dfn = dfn + 1, table = table)
    
  } else {
    left <- depth_first_numbers(node$left, dfn)
    right <- depth_first_numbers(node$right, left$new_dfn)
    
    new_dfn <- right$new_dfn
    new_node <- make_node(node$name, left$node, right$node)
    new_node$range <- c(left$node$range[1], new_dfn)
    table <- c(left$table, right$table)
    table[node$name] <- new_dfn
    list(node = new_node, new_dfn = new_dfn + 1, table = table)
  }
}

## ------------------------------------------------------------------------
df <- depth_first_numbers(tree)
df$node$range
df$table

## ------------------------------------------------------------------------
in_df_range <- function(i, df_range) 
    df_range[1] <= i && i <= df_range[2]

## ------------------------------------------------------------------------
node_depth <- function(tree, name, dfn_table, depth = 0) {
    dfn <- dfn_table[name]

    if (is.null(tree) || !in_df_range(dfn, tree$range)) {
       return(NA)
    }
    if (tree$name == name) {
        return(depth)
    }

    if (in_df_range(dfn, tree$left$range)) {
        node_depth(tree$left, name, dfn_table, depth + 1)
    } else if (in_df_range(dfn, tree$right$range)) {
        node_depth(tree$right, name, dfn_table, depth + 1)
    } else {
        NA
    }
}

node_depth <- Vectorize(node_depth, 
                        vectorize.args = "name",
                        USE.NAMES = FALSE)
node_depth(df$node, LETTERS[1:4], df$table)

## ------------------------------------------------------------------------
factorial <- function(n) {
    if (n == 1) 1
    else n * factorial(n - 1)
}

## ------------------------------------------------------------------------
factorial <- function(n, acc = 1) {
    if (n == 1) acc
    else factorial(n - 1, acc * n)
}

## ---- echo=FALSE---------------------------------------------------------
assert(factorial(3) == 3*2)

## ------------------------------------------------------------------------
find_duplicates <- function(x, i = 1) { 
    if (i >= length(x)) return(c())
    rest <- find_duplicates(x, i + 1) 
    if (x[i] == x[i + 1]) c(i, rest) else rest
}

## ------------------------------------------------------------------------
find_duplicates <- function(x, i = 1, acc = c()) { 
    if (i >= length(x)) return(acc)
    if (x[i] == x[i + 1]) find_duplicates(x, i + 1, c(acc, i))
    else find_duplicates(x, i + 1, acc)
}

## ---- echo=FALSE---------------------------------------------------------
assert(all(find_duplicates(c(1,1,2,2)) == c(1,3)))

## ------------------------------------------------------------------------
r_lin_search <- function(element, sequence, i = 1) {
  if (i > length(sequence)) FALSE
  else if (sequence[i] == element) TRUE
  else r_lin_search(element, sequence, i + 1)
}

## ------------------------------------------------------------------------
l_lin_search <- function(element, sequence) {
  for (e in sequence) {
    if (e == element) return(TRUE)
  }
  return(FALSE)
}

## ----lin_search_comparison, cache=TRUE-----------------------------------
x <- 1:1000
microbenchmark(r_lin_search(-1, x),
               l_lin_search(-1, x))


## ------------------------------------------------------------------------
r_binary_search <- function(element, x, 
                            first = 1, last = length(x)) {
  if (last < first) return(FALSE) # empty sequence
  
  middle <- (last - first) %/% 2 + first
  if (element == x[middle]) TRUE
  else if (element < x[middle]) {
    r_binary_search(element, x, first, middle - 1)
  } else {
    r_binary_search(element, x, middle + 1, last)
  }
}

## ------------------------------------------------------------------------
l_binary_search <- function(element, x, 
                            first = 1, last = length(x)) {
  repeat {
    if (last < first) return(FALSE) # empty sequence  
    
    middle <- (last - first) %/% 2 + first
    if (element == x[middle]) return(TRUE)
    
    else if (element < x[middle]) {
      last <- middle - 1
    } else {
      first <- middle + 1
    }
  }
}

## ----bin_search_benchmark, cache=TRUE------------------------------------
x <- 1:10000000
microbenchmark(r_binary_search(-1, x),
               l_binary_search(-1, x))

