#' Get transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_transaction(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_cortalconsors_transaction <- function(df_pdf_page) {

  purchase_identifier <- "kauf"
  sale_identifier <- "verkauf"
  salepart_identifier <- "verk. teil"
  dividend_identifier <- "dividendengutschrift"
  storno_identifier <- "storno"

  if (any(startsWith(df_pdf_page$text, purchase_identifier))) {

    df_transaction_output <- get_cortalconsors_purchase(df_pdf_page)

  } else if (any(startsWith(df_pdf_page$text, sale_identifier)) &&
             !(any(startsWith(df_pdf_page$text, salepart_identifier)))) {

    df_transaction_output <- get_cortalconsors_sale(df_pdf_page)

  } else if (any(startsWith(df_pdf_page$text, salepart_identifier))) {

    df_transaction_output <- get_cortalconsors_salepart(df_pdf_page)

  } else if (any(startsWith(df_pdf_page$text, dividend_identifier)) &&
             !(any(grepl(storno_identifier, df_pdf_page$text)))) {

    df_transaction_output <- get_cortalconsors_dividend(df_pdf_page)

  } else {

    stop("Transaction type of document is unknown.")

  }

  return(df_transaction_output)

}

#' Get purchase transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_purchase(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_cortalconsors_purchase <- function(df_pdf_page) {

  transaction_type  <- "Purchase"

  ## Get start and end position of transaction
  start_transaction <- grep("^wertpapierabrechnung", df_pdf_page$text) + 1
  end_transaction <- grep("zulasten konto", df_pdf_page$text)

  ## Keep only text with transaction information
  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN, WKN, quantity and name
  position_isin <- grep("isin", df_transaction_data$text) + 1

  wkn <- strsplit(df_transaction_data$text_original[position_isin], 
                  "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df_transaction_data$text_original[position_isin], 
                   "\\s+\\s+\\s+")[[1]][3]

  investment_name <- strsplit(df_transaction_data$text_original[position_isin], 
                             "\\s+\\s+\\s+")[[1]][1]

  quantity <- strsplit(
    df_transaction_data$text_original[startsWith(df_transaction_data$text,
                                                 "st")],
    "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position_price <- grep("^kurs ", df_transaction_data$text)

  transaction_price <- strsplit(df_transaction_data$text_original[position_price], 
                                "\\s+\\s+")[[1]][2]
  transaction_price <- strsplit(transaction_price, " EUR ")[[1]][1]
  transaction_price <- as.numeric(sub(",", ".", transaction_price, fixed = TRUE))

  transaction_datetime <- strsplit(df_transaction_data$text_original[grep("^kauf",
                                        df_transaction_data$text)], 
                                   "\\s+\\s+")[[1]][2]
  transaction_date <- strsplit(transaction_datetime, " UM ")[[1]][1]
  transaction_date <- gsub("AM ", "", transaction_date)
  transaction_time <- strsplit(transaction_datetime, " UM ")[[1]][2]

  position_transaction_value <- grep("^wert ", df_transaction_data$text)
  transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][3]
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  position_fee <- grep("grundgeb.?hr", df_transaction_data$text)
  if (length(position_fee) > 0) {
    transaction_fee <- strsplit(df_transaction_data$text_original[position_fee], 
                                 "\\s+\\s+")[[1]][3]
    transaction_fee <- as.numeric(sub(",", ".", transaction_fee, 
                                      fixed = TRUE))
  }
  
  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}

#' Get sale transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_sale(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_cortalconsors_sale <- function(df_pdf_page) {

  transaction_type  <- "Sale"

  start_transaction <- grep("^wertpapierabrechnung", df_pdf_page$text) + 1
  end_transaction <- grep("zugunsten konto", df_pdf_page$text)

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN, WKN, quantity and name
  position_isin <- grep("isin", df_transaction_data$text) + 1

  wkn <- strsplit(df_transaction_data$text_original[position_isin], 
                  "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df_transaction_data$text_original[position_isin], 
                   "\\s+\\s+\\s+")[[1]][3]

  investment_name <- strsplit(df_transaction_data$text_original[position_isin], 
                             "\\s+\\s+\\s+")[[1]][1]

  quantity <- strsplit(
    df_transaction_data$text_original[startsWith(df_transaction_data$text, 
                                                 "st")],
    "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position_price <- grep("^kurs ", df_transaction_data$text)
  transaction_price <- strsplit(df_transaction_data$text_original[position_price], 
                                "\\s+\\s+")[[1]][2]
  transaction_price <- strsplit(transaction_price, " EUR ")[[1]][1]
  transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                      fixed = TRUE))

  transaction_datetime <- strsplit(
    df_transaction_data$text_original[grep("^verkauf", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]
  transaction_date <- strsplit(transaction_datetime, " UM ")[[1]][1]
  transaction_date <- gsub("AM ", "", transaction_date)
  transaction_time <- strsplit(transaction_datetime, " UM ")[[1]][2]

  position_transaction_value <- grep("^wert ", df_transaction_data$text)
  transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][3]
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  transaction_fee <- NA
  position_fee <- grep("provision", df_transaction_data$text)
  if (length(position_fee) > 0) {
    transaction_fee <- strsplit(df_transaction_data$text_original[position_fee], 
                                 "\\s+\\s+")[[1]][3]
    transaction_fee <- as.numeric(sub(",", ".", transaction_fee, 
                                      fixed = TRUE))
  }


  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}

#' Get sale in part transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_salepart(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_cortalconsors_salepart <- function(df_pdf_page) {

  transaction_type  <- "Sale - Part"

  start_transaction <- grep("^wertpapierabrechnung", df_pdf_page$text) + 1
  end_transaction <- grep("zugunsten konto", df_pdf_page$text)

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN, WKN, quantity and name
  position_isin <- grep("isin", df_transaction_data$text) + 1

  wkn <- strsplit(df_transaction_data$text_original[position_isin], 
                  "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df_transaction_data$text_original[position_isin], 
                   "\\s+\\s+\\s+")[[1]][3]

  investment_name <- strsplit(df_transaction_data$text_original[position_isin], 
                             "\\s+\\s+\\s+")[[1]][1]

  quantity <- strsplit(
    df_transaction_data$text_original[startsWith(df_transaction_data$text, 
                                                 "st")],
    "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position_price <- grep("^kurs ", df_transaction_data$text)
  transaction_price <- strsplit(
    df_transaction_data$text_original[position_price], "\\s+\\s+")[[1]][2]
  transaction_price <- strsplit(transaction_price, " EUR ")[[1]][1]
  transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                      fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[grep("^verk. teil", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]
  transaction_date <- gsub("AM ", "", transaction_date)
  transaction_time <- NA

  position_transaction_value <- grep("^wert ", df_transaction_data$text)
  transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][3]
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  fee_terms <- "provision"
  transaction_fee <- NA
  for (i in 1:length(fee_terms)) {
    position_fee1 <- grep(fee_terms[i], df_transaction_data$text)
    if (length(position_fee1) > 0) {
      transaction_fee1 <- strsplit(
        df_transaction_data$text_original[position_fee1], "\\s+\\s+")[[1]][3]
      transaction_fee1 <- as.numeric(sub(",", ".", transaction_fee1, 
                                         fixed = TRUE))
      transaction_fee <- sum(transaction_fee, transaction_fee1, na.rm = TRUE)
    }
  }

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}

#' Get dividend transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_dividend(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_cortalconsors_dividend <- function(df_pdf_page) {

  transaction_type  <- "Dividend"

  start_transaction <- grep("^dividendengutschrift", df_pdf_page$text) + 1
  end_transaction <- grep("zu gunsten konto", df_pdf_page$text)

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of WKN, quantity and name
  position_wkn <- grep("wkn", df_transaction_data$text)

  ## Identify WKN; ISIN is not available for this type
  isin <- ""
  wkn <- strsplit(df_transaction_data$text_original[position_wkn], 
                  "\\s+\\s+\\s+")[[1]][4]

  investment_name <- df_transaction_data$text_original[position_wkn + 1]

  quantity <- strsplit(
    df_transaction_data$text_original[startsWith(df_transaction_data$text, 
                                                 "st")],
    "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position_price <- grep("dividendensatz", df_transaction_data$text)
  transaction_price <- strsplit(
    df_transaction_data$text_original[position_price], "\\s+\\s+")[[1]][2]
  transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                      fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[grep("schlusstag per ", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][3]
  transaction_date <- strsplit(transaction_date, " SCHLUSSTAG PER ")[[1]][2]
  transaction_time <- NA

  position_transaction_value <- grep("^wert ", df_transaction_data$text)
  transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]][3]
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
