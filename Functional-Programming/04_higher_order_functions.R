## ------------------------------------------------------------------------
sapply(1:4, sqrt)

## ------------------------------------------------------------------------
myapply <- function(x, f) {
  result <- x
  for (i in seq_along(x)) result[i] <- f(x[i])
  result
}

myapply(1:4, sqrt)

## ------------------------------------------------------------------------
rescale <- function(x) {
  m <- mean(x)
  s <- sd(x)
  (x - m) / s
}
rescale(1:4)

## ------------------------------------------------------------------------
rescale <- function(x) {
  m <- mean(x)
  s <- sd(x)
  f <- function(y) (y - m) / s
  myapply(x, f)
}
rescale(1:4)

## ------------------------------------------------------------------------
rescale <- function(x) {
  m <- mean(x)
  s <- sd(x)
  myapply(x, function(y) (y - m) / s)
}
rescale(1:4)

## ------------------------------------------------------------------------
f <- function(x, y) x + y

## ------------------------------------------------------------------------
g <- function(y) f(2, y)
myapply(1:4, g)

## ------------------------------------------------------------------------
h <- function(x) function(y) f(x, y)
myapply(1:4, h(2))

## ------------------------------------------------------------------------
f(2, 2)
h(2)(2)

## ------------------------------------------------------------------------
curry2 <- function(f)
  function(x) function(y) f(x, y)

## ------------------------------------------------------------------------
h <- curry2(f)
f(2, 3)
h(2)(3)

## ------------------------------------------------------------------------
h <- curry2(`+`)
h(2)(3)

## ------------------------------------------------------------------------
myapply(1:4, curry2(`+`)(2))

## ------------------------------------------------------------------------
curry <- function(f) {
  n <- length(formals(f))
  if (n == 1) return(f) # no currying needed

  arguments <- vector("list", length = n)
  last <- function(x) {
    arguments[n] <<- x
    do.call(f, arguments)
  }
  make_i <- function(i, continuation) {
    force(i) ; force(continuation)
    function(x) {
      arguments[i] <<- x
      continuation
    }
  }

  continuation <- last
  for (i in seq(n-1, 1)) {
    continuation <- make_i(i, continuation)
  }
  continuation
}

## ------------------------------------------------------------------------
f <- function(x, y, z) x + 2*y + 3*z
f(1, 2, 3)
curry(f)(1)(2)(3)

## ------------------------------------------------------------------------
bind_parameters <- function(f, ...) {
  remembered <- list(...)
  function(...) {
    new <- list(...)
    do.call(f, c(remembered, new))
  }
}

f <- function(x, y, z, w = 4) x + 2*y + 3*z + 4*w

f(1, 2, 3, 4)
g <- bind_parameters(f, y = 2)
g(x = 1, z = 3)

h <- bind_parameters(f, y = 1, w = 1)
f(2, 1, 3, 1)
h(x = 2, z = 3)

## ------------------------------------------------------------------------
my_sum_direct <- function(lst) {
  if (is_empty(lst)) 0
  else first(lst) + my_sum_direct(rest(lst))
}
my_sum_acc <- function(lst, acc = 0) {
  if (is_empty(lst)) acc
  else my_sum_acc(rest(lst), first(lst) + acc)
}
my_sum_cont <- function(lst, cont = identity) {
  if (is_empty(lst)) cont(0)
  else my_sum_cont(rest(lst), 
                   function(acc) cont(first(lst) + acc))
}

## ---- echo=FALSE---------------------------------------------------------
make_node <- function(name, left = NULL, right = NULL)
  list(name = name, left = left, right = right)

tree <- make_node("root",
                  make_node("C", make_node("A"),
                                 make_node("B")),
                  make_node("D"))

## ------------------------------------------------------------------------
size_of_tree <- function(node, continuation = identity) {
  if (is.null(node$left) && is.null(node$right)) {
    continuation(1)
  } else {
    new_continuation <- function(left_result) {
      continuation(left_result + size_of_tree(node$right) + 1)
    }
    size_of_tree(node$left, new_continuation)
  }
}

size_of_tree(tree)

## ------------------------------------------------------------------------
size_of_tree <- function(node) {
  continuation <- identity # function(x) x
  repeat {
    if (is.null(node$left) && is.null(node$right)) {
      return(continuation(1))
    }
    new_continuation <- function(continuation) {
      force(continuation)
      function(left_result) {
        continuation(left_result + size_of_tree(node$right) + 1)
      }
    }
    # simulated recursive call
    node <- node$left
    continuation <- new_continuation(continuation)
  }
}

size_of_tree(tree)

## ------------------------------------------------------------------------
make_thunk <- function(f, ...) { 
  force(f)
  params <- list(...)
  function() do.call(f, params)
}

## ------------------------------------------------------------------------
f <- function(x, y) x + y
thunk <- make_thunk(f, 2, 2)
thunk()

## ------------------------------------------------------------------------
trampoline <- function(thunk) {
  while (is.function(thunk)) thunk <- thunk()
  thunk
}

## ------------------------------------------------------------------------
factorial <- function(n, acc = 1) {
  if (n == 1) acc
  else factorial(n - 1, acc * n)
}

## ----cp_factorial, cache=TRUE--------------------------------------------
cp_factorial <- function(n, continuation = identity) {
  if (n == 1) {
    continuation(1)
  } else {
    new_continuation <- function(result) {
      continuation(result * n)
    }
    cp_factorial(n - 1, new_continuation)
  } 
}

factorial(10)
cp_factorial(10)

## ------------------------------------------------------------------------
thunk_factorial <- function(n, continuation = identity) {
  if (n == 1) {
    continuation(1)
  } else {
    new_continuation <- function(result) {
      make_thunk(continuation, n * result)
    }
    make_thunk(thunk_factorial, n - 1, new_continuation)
  }
}

## ------------------------------------------------------------------------
thunk_factorial(1)

## ------------------------------------------------------------------------
thunk_factorial(2)()()

## ----thunk_factorial_explicit, cache=TRUE--------------------------------
thunk_factorial(3)()()()()
thunk_factorial(4)()()()()()()
thunk_factorial(5)()()()()()()()()

## ----trampoline_thunk, cache=TRUE----------------------------------------
trampoline(thunk_factorial(100))

## ----trampoline_thunk_function, cache=TRUE-------------------------------
make_trampoline <- function(f) function(...) trampoline(f(...))
factorial <- make_trampoline(thunk_factorial)
factorial(100)

## ------------------------------------------------------------------------
thunk_size <- function(node, continuation = identity) {
  if (is.null(node$left) && is.null(node$right)) {
    continuation(1)
  } else {
    new_continuation <- function(left_result) 
      make_thunk(continuation, 
                 left_result + thunk_size(node$right) + 1)
    make_thunk(thunk_size, node$left, new_continuation)
  }
}

size_of_tree <- make_trampoline(thunk_size)
size_of_tree(tree)

