#' apcadr
#'
#' @param x Row of a dataset.
#'
#' @examples
#' data<-(c("腺腫|腺腫|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症", "進行大腸癌|ポリープ", "polyp|adenoma"))
#' x<-apcadr(data)
#'
#' # prints "APC:0.6, ADR:0.4, PPC:1, PDR:0.66667, ACN DR:0.2, ACN per colonoscopy:0.2"
#' # Then you can access each value with x$APC, x$ADR, x$PPC, x$PDR, x$ACNDR and x$ACNPC.
#'
#' @note
#' This function calculates adenoma per colonoscopy (APC), adenoma detection rate (ADR), polyp per colonoscopy (PPC), polyp detection rate (PDR), advanced colorectal neoplasm detection rate (ACN DR), and advanced colorectal neoplasm per colonoscopy (ACN per colonoscopy) from JED-style database.
#' You can find sample database in "tests/testthat/sample_CF_data.csv".
#'
#' @export
#'
apcadr <- function(x) {
  split_values <- unlist(strsplit(x, "\\|"))

  polyposis_terms <- c(
    "\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", "Peutz-Jeghers", "\u30dd\u30ea\u30dd\u30fc\u30b7\u30b9", "polyposis",
    "Cronkhite-Canada", "Lynch", "Cowden", "Gardner", "Turcot")

  polyp_terms <- c("\u30dd\u30ea\u30fc\u30d7", "\u817a\u816b", "polyp", "serrated", "adenoma",
                   "\u92f8\u6b6f\u72b6", "HP", "\uff28\uff30", "Tsp", "\uff34\uff53\uff50", "Ts", "\uff34\uff53",
                   "Tp", "\uff34\uff50", "Ua", "\uff35\uff41", "Uc", "\uff35\uff43", "Ip", "\uff29\uff50",
                   "Isp", "\uff29\uff53\uff50", "Is", "\uff29\uff53", "SSA/P", "\uff33\uff33\uff21\uff0f\uff30",
                   "IIa", "\uff29\uff29\uff41", "IIb", "\uff29\uff29\uff42", "IIc", "\uff29\uff29\uff43",
                   "III", "\uff29\uff29\uff29", "LST", "\uff2c\uff33\uff34", "\u5074\u65b9\u767a\u80b2\u578b")

  polyp_pattern <- paste(c(polyp_terms, polyposis_terms), collapse = "|")
  polyposis_pattern <- paste(polyposis_terms, collapse = "|")

  num_polyp <- sum(grepl(polyp_pattern, split_values)) - sum(grepl("polypectomy\u5f8c|Polypectomy\u5f8c", split_values))
  num_polyppositive <- sum(grepl(polyp_pattern, x)) - sum(grepl("polypectomy\u5f8c|Polypectomy\u5f8c", x))
  num_adenoma <- sum(grepl("\u817a\u816b|adenoma", split_values)) - sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", split_values))
  num_adenomapositive <- sum(grepl("\u817a\u816b|adenoma", x)) - sum(grepl("\u5bb6\u65cf\u6027\u5927\u8178\u817a\u816b\u75c7", x))
  num_polyposis <- sum(grepl(polyposis_pattern, x))

  ppc <- num_polyp / length(x)
  pdr <- num_polyppositive / length(x)
  apc <- num_adenoma / (length(x) - num_polyposis)
  adr <- num_adenomapositive / (length(x) - num_polyposis)

  advcancerpositive <- grepl("\u9032\u884c", x)
  nonrelapsed_cancer_yes <- grepl("\u518d\u767a[^|]{0,15}(\u7121|\u7121\u3057|\u306a\u3057|\u7121\u3044|\u306a\u3044|\u8a8d\u3081\u305a|\u307f\u3068\u3081\u305a|\u307e\u305b\u3093|\u7121\u3055\u305d\u3046|\u306a\u3055\u305d\u3046|\u6307\u6458\u3067\u304d\u305a|\u6307\u6458\u3057\u5f97\u305a|\u6307\u6458\u3057\u3048\u305a|\u898b\u5f53\u305f\u3089\u305a|\u307f\u3042\u305f\u3089\u305a)", advcancerpositive)

  advcancer <- grepl("\u9032\u884c", split_values)
  nonrelapsed_cancer <- grepl("\u518d\u767a.{0,15}(\u7121|\u7121\u3057|\u306a\u3057|\u7121\u3044|\u306a\u3044|\u8a8d\u3081\u305a|\u307f\u3068\u3081\u305a|\u307e\u305b\u3093|\u7121\u3055\u305d\u3046|\u306a\u3055\u305d\u3046|\u6307\u6458\u3067\u304d\u305a|\u6307\u6458\u3057\u5f97\u305a|\u6307\u6458\u3057\u3048\u305a|\u898b\u5f53\u305f\u3089\u305a|\u307f\u3042\u305f\u3089\u305a)", advcancer)

  acndr <- (sum(advcancerpositive) - sum(nonrelapsed_cancer_yes))/(length(x) - num_polyposis)
  acnpc <- (sum(advcancer) - sum(nonrelapsed_cancer))/(length(x) - num_polyposis)

  result <- list(
    APC = apc, ADR = adr, PPC = ppc, PDR = pdr, ACNDR = acndr, ACNPC = acnpc
  )

  print(paste0("APC:", format(apc, digits = 5),
               ", ADR:", format(adr, digits = 5),
               ", PPC:", format(ppc, digits = 5),
               ", PDR:", format(pdr, digits = 5),
               ", ACN DR:", format(acndr, digits = 5),
               ", ACN per colonoscopy:", format(acnpc, digits = 5)))

  return(result)
}
