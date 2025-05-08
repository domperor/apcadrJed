test_that("apcadr_works", {
  data<-read.csv("sample_CF_data.csv")
  expect_equal(apcadr(data$統計_診断), "APC:0.59596, ADR:0.39394")
})
