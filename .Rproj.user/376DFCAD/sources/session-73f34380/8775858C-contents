apcadr <- function(x) {
  split_values <- unlist(strsplit(x, "\\|"))
  num_adenoma <- sum(grepl("腺腫", split_values))-sum(grepl("家族性大腸腺腫症", split_values))
  num_positive <- sum(grepl("腺腫", x))-sum(grepl("家族性大腸腺腫症", x))
  num_polyposis <- sum(grepl("家族性大腸腺腫症|Peutz-Jeghers症候群|若年性ポリポーシス|polyposis", x))
  apc <- num_adenoma/(length(x)-num_polyposis)
  adr <- num_positive/(length(x)-num_polyposis)
  print(paste0("APC:",apc,", ADR:",adr))
}

