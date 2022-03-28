#' Get transaction from Scalable Capital statement
#'
#' @usage get_scalablecapital_transaction(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#' @return A data frame containing the parse transaction.
#'
#' @export
get_scalablecapital_transaction <- function(df.pdf.page) {

  ## To do: Better to replace all df.pdf with df.pdf.page
  df.pdf <- df.pdf.page

  purchase.identifier <- "wertpapierabrechnung: kauf"

  if (any(grepl(purchase.identifier, df.pdf$text))) {

    df.transaction.output <- get_scalablecapital_purchase(df.pdf)

  } else {

    stop("Transaction type of document is unknown.")

  }

  return(df.transaction.output)

}

#' Get purchase transaction from Scalable Capital statement
#'
#' @usage get_scalablecapital_purchase(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_scalablecapital_purchase <- function(df.pdf.page) {

  transaction.type  <- "Purchase"

  start.transaction.data <- grep("wertpapierabrechnung: kauf", 
                                 df.pdf.page$text) + 1
  end.transaction.data <- grep("f.?r die ausf.?hrung dieses auftrags gelten", 
                               df.pdf.page$text) + 1

  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN and Gattungsbezeichnung
  position.isin <- grep("isin", df.transaction.data$text)

  ## Last word one row later is actual ISIN
  isin.wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                       "\\s+\\s+")[[1]][2]

  isin <- strsplit(isin.wkn, " WKN: ")[[1]][1]
  isin <- gsub("ISIN: ", "", isin)
  wkn <- strsplit(isin.wkn, "WKN: ")[[1]][2]

  investment.name <- df.transaction.data$text_original[position.isin + 1]
  investment.name <- strsplit(investment.name, "\\s+\\s+")[[1]][2]

  position.quantity.and.price <- grep("^stk", df.transaction.data$text)[1]
  quantity <- strsplit(df.transaction.data$text_original[position.quantity.and.price], 
                       "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ","", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  transaction.price <- strsplit(df.transaction.data$text_original[position.quantity.and.price], 
                                "\\s+\\s+")[[1]][4]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  position.date.time <- grep("handels-", df.transaction.data$text) + 2
  transaction.date <- strsplit(df.transaction.data$text_original[position.date.time], 
                               "\\s+\\s+")[[1]][5]
  transaction.time <- strsplit(df.transaction.data$text_original[position.date.time], 
                               "\\s+\\s+")[[1]][6]

  position.transaction.value <- grep("zu lasten konto", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][4]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## Identify transaction fee (not available in PDF statement, often it is EUR 1.00)
  transaction.fee <- NA

  df.transaction.output <- data.frame(
    isin = isin, wkn = wkn, name = investment.name, quantity = quantity, 
    transaction_price = transaction.price, transaction_value = transaction.value, 
    transaction_fee = transaction.fee, transaction_date = transaction.date, 
    transaction_time = transaction.time, transaction_type = transaction.type
  )

  return(df.transaction.output)

}
