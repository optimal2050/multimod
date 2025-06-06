% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_word_num}
\alias{is_word_num}
\title{Check if a string is alphanumeric (letters, digits, or underscores)}
\usage{
is_word_num(ch)
}
\arguments{
\item{ch}{A character string or vector of strings to test.}
}
\value{
A logical vector the same length as \code{ch}, where each element is \code{TRUE}
if the corresponding string contains only alphanumeric characters and underscores,
\code{FALSE} otherwise.
}
\description{
This function tests whether the input string consists entirely of
letters (\verb{a–z}, \verb{A–Z}), digits (\verb{0–9}), or underscores (\verb{_}).
}
\examples{
is_word_num("alpha123")     # TRUE
is_word_num("var_1")        # TRUE
is_word_num("a-b")          # FALSE
is_word_num(c("abc", "123", "a_b", "x-y"))  # TRUE, TRUE, TRUE, FALSE
}
