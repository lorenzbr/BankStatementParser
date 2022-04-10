files <- list.files(system.file("extdata", package = "BankStatementParser"), 
                    pattern = "pdf", full.names = TRUE)
document_names <- basename(files)

test_that("parsing sample transaction data is successful", {
  
  expect_error(
    list_dfs <- mapply(get_transactions, 
                       files, 
                       document_names, 
                       SIMPLIFY = FALSE),
    NA)
  
  expect_error(
    df_transactions <- do.call(rbind, unname(list_dfs)),
    NA)
  
  expect_equal(all(is.na(df_transactions$quantity)), FALSE)
  expect_equal(all(is.na(df_transactions$transaction_price)), FALSE)
  expect_equal(all(is.na(df_transactions$transaction_value)), FALSE)
  
})

test_that("parsing private transaction data is successful", {
  
  #### Run some more tests with private transactions
  
  file_path_transactions <- test_path("testdata/transactions")
  
  skip_if_not(file.exists(file_path_transactions))
  
  file_path_cortalconsors <- file.path(file_path_transactions, "cortal_consors")
  file_path_dkb <- file.path(file_path_transactions, "dkb")
  file_path_onvista <- file.path(file_path_transactions, "onvista")
  file_path_scalablecapital <- file.path(file_path_transactions, 
                                         "scalable_capital")
  
  expect_error(
    list_cortalconsors <- mapply(get_transactions,
                                 list.files(file_path_cortalconsors, 
                                            full.names = TRUE), 
                                 list.files(file_path_cortalconsors), 
                                 SIMPLIFY = FALSE), 
    NA)
  expect_error(
    df_transactions_cortalconsors <- do.call(rbind, unname(list_cortalconsors)),
    NA)
  
  expect_error(
    list_dkb <- mapply(get_transactions,
                       list.files(file_path_dkb, full.names = TRUE), 
                       list.files(file_path_dkb), 
                       SIMPLIFY = FALSE),
    NA)
  expect_error(
    df_transactions_dkb <- do.call(rbind, unname(list_dkb)),
    NA)
  
  expect_error(
    list_onvista <- mapply(get_transactions, 
                           list.files(file_path_onvista, full.names = TRUE), 
                           list.files(file_path_onvista), 
                           SIMPLIFY = FALSE),
    NA)
  df_transactions_onvista <- do.call(rbind, unname(list_onvista))
  
  expect_error(
    list_scalablecapital <- mapply(get_transactions,
                                   list.files(file_path_scalablecapital, 
                                              full.names = TRUE), 
                                   list.files(file_path_scalablecapital), 
                                   SIMPLIFY = FALSE),
    NA)
  expect_error(
    df_transactions_scalablecapital <- do.call(rbind, 
                                               unname(list_scalablecapital)),
    NA)
  
  expect_error(
    df_all_transactions <- rbind(
      df_transactions_cortalconsors,
      df_transactions_dkb,
      df_transactions_onvista,
      df_transactions_scalablecapital
      ),
    NA)
  
  expect_equal(all(is.na(df_all_transactions$quantity)), FALSE)
  expect_equal(all(is.na(df_all_transactions$transaction_price)), FALSE)
  expect_equal(all(is.na(df_all_transactions$transaction_value)), FALSE)
  
})