#' Get transaction from Deutsche Kreditbank (DKB AG) bank statement
#'
#' @usage get_dkb_transaction(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_dkb_transaction <- function(df_pdf_page) {

  purchase_identifier <- "die wertpapiere schreiben wir ihrem depotkonto gut"
  dividend_identifier <- "^aussch.?ttung"
  storno_identifier <- "storno"

  if (any(grepl(purchase_identifier, df_pdf_page$text))) {

    df_transaction_output <- get_dkb_purchase(df_pdf_page)

  } else if (any(grepl(dividend_identifier, df_pdf_page$text)) 
             && !(any(grepl(storno_identifier, df_pdf_page$text)))) {

    df_transaction_output <- get_dkb_dividend(df_pdf_page)

  } else {

    stop("Transaction type of document is unknown.")

  }

  return(df_transaction_output)

}

#' Get purchase transaction from Deutsche Kreditbank AG bank statement
#'
#' @usage get_dkb_purchase(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_dkb_purchase <- function(df_pdf_page) {

  transaction_type  <- "Purchase"

  start_transaction <- grep("wertpapier abrechnung", df_pdf_page$text) + 1
  end_transaction <- grep("ausmachender betrag", df_pdf_page$text)

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN, WKN, quantity and name
  position_isin <- grep("isin", df_transaction_data$text) + 1

  isin <- strsplit(df_transaction_data$text_original[position_isin], 
                   "\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df_transaction_data$text_original[position_isin], 
                  "\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  investment_name <- strsplit(df_transaction_data$text_original[position_isin], 
                             "\\s+\\s+\\s+")[[1]][2]

  quantity <- strsplit(df_transaction_data$text_original[position_isin], 
                       "\\s+\\s+\\s+")[[1]][1]
  quantity <- gsub("St.?ck ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position_price <- grep("ausf.?hrungskurs", df_transaction_data$text)
  transaction_price <- strsplit(
    df_transaction_data$text_original[position_price], "\\s+\\s+")[[1]][1]
  transaction_price <- gsub(" EUR","", transaction_price)
  transaction_price <- gsub("Ausf.?hrungskurs ","", transaction_price)
  transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                      fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[grep("^schlusstag", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]
  transaction_time <- NA

  position_transaction_value <- grep("ausmachender betrag", 
                                     df_transaction_data$text)
  transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][2]
  transaction_value <- gsub("- EUR", "", transaction_value)
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  position_fee <- grep("provision", df_transaction_data$text)
  if (length(position_fee) > 0) {
    transaction_fee <- strsplit(
      df_transaction_data$text_original[position_fee], 
      "\\s+\\s+")[[1]][2]
    transaction_fee <- sub("- EUR", "", transaction_fee, fixed = TRUE)
    transaction_fee <- as.numeric(sub(",", ".", transaction_fee, fixed = TRUE))
  }

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}

#' Get dividend transaction from Deutsche Kreditbank AG bank statement
#'
#' @usage get_dkb_dividend(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_dkb_dividend <- function(df_pdf_page) {

  transaction_type  <- "Dividend"

  start_transaction <- grep("^aussch.?ttung", df_pdf_page$text)[1] + 1
  end_transaction <- grep("ausmachender betrag", df_pdf_page$text)

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN, WKN, quantity and name
  position_isin <- grep("isin", df_transaction_data$text) + 1

  isin <- strsplit(df_transaction_data$text_original[position_isin], 
                   "\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df_transaction_data$text_original[position_isin], 
                  "\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  investment_name <- strsplit(df_transaction_data$text_original[position_isin], 
                             "\\s+\\s+\\s+")[[1]][2]

  quantity <- strsplit(df_transaction_data$text_original[position_isin], 
                       "\\s+\\s+\\s+")[[1]][1]
  quantity <- gsub("St.?ck ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position_price <- grep("aussch.?ttung pro st", df_transaction_data$text)
  transaction_price <- strsplit(
    df_transaction_data$text_original[position_price], "\\s+\\s+")[[1]][4]
  transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                      fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[grep("^zahlbarkeitstag", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]
  transaction_time <- NA

  position_transaction_value <- grep("ausmachender betrag", 
                                     df_transaction_data$text)
  transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][2]
  transaction_value <- gsub("\\+ EUR", "", transaction_value)
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  transaction_fee <- NA

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}
