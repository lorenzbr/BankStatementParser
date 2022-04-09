#' Get transaction from Scalable Capital statement
#'
#' @usage get_scalablecapital_transaction(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parse transaction.
#'
#' @export
get_scalablecapital_transaction <- function(df_pdf_page) {

  ## To do: Better to replace all df_pdf with df_pdf_page
  df_pdf <- df_pdf_page

  purchase_identifier <- "wertpapierabrechnung: kauf"

  if (any(grepl(purchase_identifier, df_pdf$text))) {

    df_transaction_output <- get_scalablecapital_purchase(df_pdf)

  } else {

    stop("Transaction type of document is unknown.")

  }

  return(df_transaction_output)

}

#' Get purchase transaction from Scalable Capital statement
#'
#' @usage get_scalablecapital_purchase(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_scalablecapital_purchase <- function(df_pdf_page) {

  transaction_type  <- "Purchase"

  start_transaction <- grep("wertpapierabrechnung: kauf", 
                                 df_pdf_page$text) + 1
  end_transaction <- grep("f.?r die ausf.?hrung dieses auftrags gelten", 
                               df_pdf_page$text) + 1

  df_transaction <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN and Gattungsbezeichnung
  position_isin <- grep("isin", df_transaction$text)

  ## Last word one row later is actual ISIN
  isin_wkn <- strsplit(df_transaction$text_original[position_isin], 
                       "\\s+\\s+")[[1]][2]

  isin <- strsplit(isin_wkn, " WKN: ")[[1]][1]
  isin <- gsub("ISIN: ", "", isin)
  wkn <- strsplit(isin_wkn, "WKN: ")[[1]][2]

  investment_name <- df_transaction$text_original[position_isin + 1]
  investment_name <- strsplit(investment_name, "\\s+\\s+")[[1]][2]

  position_quantity_and_price <- grep("^stk", df_transaction$text)[1]
  quantity <- strsplit(
    df_transaction$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ","", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  transaction_price <- strsplit(
    df_transaction$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][4]
  transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                      fixed = TRUE))

  position_datetime <- grep("handels-", df_transaction$text) + 2
  transaction_date <- strsplit(df_transaction$text_original[position_datetime], 
                               "\\s+\\s+")[[1]][5]
  transaction_time <- strsplit(df_transaction$text_original[position_datetime], 
                               "\\s+\\s+")[[1]][6]

  position_transaction_value <- grep("zu lasten konto", df_transaction$text)
  transaction_value <- strsplit(
    df_transaction$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][4]
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  ## Identify transaction fee (not available in PDF statement, 
  ## often it is EUR 1.00)
  transaction_fee <- NA

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}
