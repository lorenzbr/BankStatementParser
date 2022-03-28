files <- list.files(system.file("extdata", package = "BankStatementParser"), 
                    pattern = "pdf", full.names = TRUE)
document_names <- basename(files)

list_dfs <- mapply(BankStatementParser::get_transactions, files, 
                   document_names, SIMPLIFY = FALSE)
df_transactions <- do.call(rbind, unname(list_dfs))

test_that("columns in data frame are not NA", {
  
  expect_equal(all(is.na(df_transactions$quantity)), FALSE)
  expect_equal(all(is.na(df_transactions$transaction_price)), FALSE)
  expect_equal(all(is.na(df_transactions$transaction_value)), FALSE)
  
})