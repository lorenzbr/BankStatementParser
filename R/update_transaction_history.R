#' Create empty csv file for transaction history
#'
#' @usage init_transaction_history(path, file, overwrite = FALSE)
#' @param path A single character string. The path of the csv file.
#' @param file A single character string. The name of the csv file.
#' @param overwrite Logical (default is FALSE) indicates whether csv shall be overwritten.
#' @export
init_transaction_history <- function(path, file, overwrite = FALSE) {

  transaction.colnames <- c("isin", "wkn", "name", "quantity", "transaction_price", "transaction_value", "transaction_fee",
                            "transaction_date", "transaction_time", "transaction_type", "document_page", "document_name")

  df.transaction.history.init <- data.frame(matrix(NA, nrow = 0, ncol = length(transaction.colnames),
                                                   dimnames = list(NULL, transaction.colnames)))

  if ( !file.exists(paste0(path, file)) ) {
    data.table::fwrite(df.transaction.history.init,paste0(path, file), sep = ";", quote = TRUE)
  } else if (overwrite) {
    data.table::fwrite(df.transaction.history.init, paste0(path, file))
  }

}

#' Update transaction history stored in a csv file
#'
#' @usage update_transaction_history(df.transactions, path, file)
#' @param df.transactions A data.frame which contains transactions (e.g., results from [get_transactions()])
#' @param path A single character string. The path of the csv file.
#' @param file A single character string. The name of the csv file.
#'
#' @export
update_transaction_history <- function(df.transactions, path, file) {

  ## if no transaction history csv exists create it
  init_transaction_history(path, file)

  ## load files and append to transaction history
  data.table::fwrite(df.transactions, paste0(path, file), append = TRUE, sep = ";", quote = TRUE)

  ## check that transactions are unique (i.e., no duplicates based on all columns)
  df.transaction.history <- data.table::fread(paste0(path, file))
  df.transaction.history <- unique(df.transaction.history)
  data.table::fwrite(df.transaction.history, paste0(path, file), sep = ";", quote = TRUE)

}
