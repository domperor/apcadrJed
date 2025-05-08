#' apcadr
#'
#' @param x Row of a dataset.
#'
#' @examples
#' data<-(c("腺腫|腺腫|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症"))
#' apcadr(data)
#'
#' # returns "APC:0.66667, ADR:0.33333"
#'
#' @note
#' This function counts "腺腫" while excluding "家族性大腸腺腫症", "Peutz-Jeghers症候群", "若年性ポリポーシス" and "Serrated polyposis syndrome", according to JED-style database.
#' You can find sample database in "tests/testthat/sample_CF_data.csv".
#'
#' @export
#'
apcadr <- function(x) {
  split_values <- unlist(strsplit(x, "\\|"))
  num_adenoma <- sum(grepl("\u817a\u816b", split_values))-sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", split_values))
  num_positive <- sum(grepl("\u817a\u816b", x))-sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", x))
  num_polyposis <- sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7|Peutz-Jeghers\u75c7\u5019\u7fa4|\u82e5\u5e74\u6027\u30dd\u30ea\u30dd\u30fc\u30b7\u30b9|polyposis", x))
  apc <- num_adenoma/(length(x)-num_polyposis)
  adr <- num_positive/(length(x)-num_polyposis)
  print(paste0("APC:",format(apc,digits=5),", ADR:",format(adr,digits=5)))
}
