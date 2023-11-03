Assignment-B1
================
Ian Coccimiglio
2023-11-03

## Creating a non-zero counter function

The function created here will take in a numeric vector, and report the
number of non-zero entries. Zeros here are loosely defined, and include
anything that is ‘falsy’ - such as 0, “0”, and FALSE. It does not work
on entries that equal NA.

``` r
#' Non-zero element counter
#' This function returns the number of non-zero elements in a vector 
#' (including values that can be coerced to equal 0, such as FALSE and "0").
#' It is also capable of handling non-vector inputs, such as lists and dataframes. 
#' Values of NA will throw an error as they cannot be assigned a value of true/false.
#' 
#' @param vec A vector of inputs. 
#' The name suggests that this function is designed to operate on generic vectors, 
#' and not explicitly designed for higher level structures.
#'
#' @return numNonZero A count of non zero values
#' @export
#'
#' @examples countNonZero(seq(0,5))
countNonZero <- function(vec) {
  if (is.data.frame(vec)) {
    print("Note: Counting non-zero entries across all rows and columns")
  }
  if (any(is.na(vec))) stop("NAs detected in input")
  nonZeroElements <- (vec != 0)
  numNonZero <- sum(nonZeroElements)
  return(numNonZero)
}
```

## Examples

This function is very flexible and works on a variety of inputs. In its
simplest usage, this function takes in an ordinary numeric vector, and
returns the number of non-zero elements.

``` r
ordinary_vector <- c(0, 0, 5, 2, 3, 0, 1)
countNonZero(ordinary_vector)
```

    ## [1] 4

However, the function handles non-typical inputs as well, such as string
vectors and boolean vectors:

``` r
string_vec <- countNonZero(c("0", "0", "1", "2"))
bool_vec <- countNonZero(c(FALSE, TRUE, TRUE, TRUE, FALSE))
print(paste("String Vector =",string_vec))
```

    ## [1] "String Vector = 2"

``` r
print(paste("Boolean Vector =",bool_vec))
```

    ## [1] "Boolean Vector = 3"

In typical usage, you might have a problem like “counting the number of
students who have first aid training”, where there is a binary column in
a dataframe. We can easily use countNonZero to assess the number of
students who have this training.

``` r
df <- data.frame('Name'=c("John", "Sarah", "Jeff", "Karen", "James"), "First_Aid_Training"=c(0,1,1,0,1))
countNonZero(df$First_Aid_Training)
```

    ## [1] 3

In more niche use cases, you can count the total number of zeros present
within a dataframe across all rows and columns. This prints a note so
the user knows what’s happening.

``` r
df <- data.frame('Name'=c("John", "Sarah", "Jeff", "Karen", "0"), "First_Aid_Training"=c(0,1,1,0,1))
countNonZero(df)
```

    ## [1] "Note: Counting non-zero entries across all rows and columns"

    ## [1] 7

## Tests

``` r
df <- data.frame('Name'=c("John", "Sarah", "Jeff", "Karen", "Jeff"), "First_Aid_Training"=c(0,1,1,0,1))

test_that("numeric vectors work", {
  expect_equal(countNonZero(c(0, 0, 0)), 0)
  expect_equal(countNonZero(c(1, 1, 1)), 3)
  expect_equal(countNonZero(c(0, 2, 2)), 2)
})
```

    ## Test passed 🌈

``` r
test_that("string vectors work", {
  expect_equal(countNonZero(c("0", "0", "0")), 0)
  expect_equal(countNonZero(c("0", "1", "1")), 2)
})
```

    ## Test passed 🥳

``` r
test_that("NAs create errors", {
  expect_error(countNonZero(c(NA, NA)))
  expect_error(countNonZero(c(1, 0, NA)))
})
```

    ## Test passed 🥳

``` r
test_that("Dataframes are counted across all columns", {
  expect_equal(countNonZero(df), 8)
})
```

    ## [1] "Note: Counting non-zero entries across all rows and columns"
    ## Test passed 😀
