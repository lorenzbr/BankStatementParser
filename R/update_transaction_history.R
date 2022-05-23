#' Create empty csv file for transaction history
#'
#' @usage init_transaction_history(path, file, overwrite = FALSE)
#' 
#' @param path A single character string. The path of the csv file.
#' @param file A single character string. The name of the csv file.
#' @param overwrite Logical (default is FALSE) indicates whether csv 
#' shall be overwritten.
#' 
#' @export
init_transaction_history <- function(path, file, overwrite = FALSE) {

  transaction_colnames <- c("isin", "wkn", "name", "quantity", 
                            "transaction_price", "transaction_value", 
                            "transaction_fee", "transaction_date", 
                            "transaction_time", "transaction_type", 
                            "document_page", "document_name")

  df_transaction_history_init <- data.frame(
    matrix(NA, nrow = 0, ncol = length(transaction_colnames),
           dimnames = list(NULL, transaction_colnames)))

  file_path <- file.path(path, file)
  
  if (!file.exists(file_path)) {
    data.table::fwrite(df_transaction_history_init, 
                       file_path, 
                       quote = TRUE)
  } else if (overwrite) {
    data.table::fwrite(df_transaction_history_init, file_path)
  }

}

#' Update transaction history stored in a csv file
#'
#' @usage update_transaction_history(df_transactions, path, file)
#' 
#' @param df_transactions A data frame which contains transactions 
#' (e.g., results from [get_transactions()])
#' @param path A single character string. The path of the csv file.
#' @param file A single character string. The name of the csv file.
#'
#' @export
update_transaction_history <- function(df_transactions, path, file) {

  init_transaction_history(path, file)
  
  file_path <- file.path(path, file)

  data.table::fwrite(df_transactions, 
                     file_path, 
                     append = TRUE, 
                     quote = TRUE)

  ## Check that transactions are unique (i.e., no duplicates based 
  ## on all columns)
  df_transaction_history <- data.table::fread(file_path)
  df_transaction_history <- unique(df_transaction_history)
  data.table::fwrite(df_transaction_history, 
                     file_path, 
                     quote = TRUE)

}
