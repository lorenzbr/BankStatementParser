test_that("price and value are numeric", {
  
  ## Parse all documents and extract information
  ## Check that price and value can be identified and are numeric
  
  files <- list.files(system.file("extdata", package = "BankStatementParser"), pattern = "pdf", full.names = TRUE)
  document.names <- basename(files)
  
  list.dfs <- mapply(BankStatementParser::get_transactions, files, document.names, SIMPLIFY = FALSE)
  df.transactions <- do.call(rbind, unname(list.dfs))
  
  expect_equal(all(is.na(df.transactions$transaction_price)), FALSE)
  expect_equal(all(is.na(df.transactions$transaction_value)), FALSE)
  
})