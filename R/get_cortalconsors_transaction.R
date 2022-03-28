#' Get transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_transaction(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_transaction <- function(df.pdf.page) {

  ## Identify type of transaction
  purchase.identifier <- "kauf"
  sale.identifier <- "verkauf"
  salepart.identifier <- "verk. teil"
  dividend.identifier <- "dividendengutschrift"
  storno.identifier <- "storno"

  if (any(startsWith(df.pdf.page$text, purchase.identifier))) {

    df.transaction.output <- get_cortalconsors_purchase(df.pdf.page)

  } else if (any(startsWith(df.pdf.page$text, sale.identifier)) 
             && !(any(startsWith(df.pdf.page$text, salepart.identifier)))) {

    df.transaction.output <- get_cortalconsors_sale(df.pdf.page)

  } else if (any(startsWith(df.pdf.page$text, salepart.identifier))) {

    df.transaction.output <- get_cortalconsors_salepart(df.pdf.page)

  } else if (any(startsWith(df.pdf.page$text, dividend.identifier)) 
             && !(any(grepl(storno.identifier, df.pdf.page$text)))) {

    df.transaction.output <- get_cortalconsors_dividend(df.pdf.page)

  } else {

    stop(paste("Transaction type of document is unknown."))

  }

  return(df.transaction.output)

}

#' Get purchase transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_purchase(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_purchase <- function(df.pdf.page) {

  transaction.type  <- "Purchase"

  ## Get start and end position of transaction
  start.transaction.data <- grep("^wertpapierabrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("zulasten konto", df.pdf.page$text)

  ## Keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                  "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df.transaction.data$text_original[position.isin], 
                   "\\s+\\s+\\s+")[[1]][3]

  investmentname <- strsplit(df.transaction.data$text_original[position.isin], 
                             "\\s+\\s+\\s+")[[1]][1]

  quantity <- strsplit(df.transaction.data$text_original[startsWith(df.transaction.data$text, 
                                                                    "st")],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position.price <- grep("^kurs ", df.transaction.data$text)

  transaction.price <- strsplit(df.transaction.data$text_original[position.price], 
                                "\\s+\\s+")[[1]][2]
  transaction.price <- strsplit(transaction.price, " EUR ")[[1]][1]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  transaction.datetime <- strsplit(df.transaction.data$text_original[grep("^kauf",
                                        df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.date <- strsplit(transaction.datetime, " UM ")[[1]][1]
  transaction.date <- gsub("AM ", "", transaction.date)
  transaction.time <- strsplit(transaction.datetime, " UM ")[[1]][2]

  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## Identify transaction fee
  fee.terms <- c("grundgeb.?hr")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (length(position.fee1) > 0) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], 
                                   "\\s+\\s+")[[1]][3]
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee, transaction.fee1, na.rm = TRUE)
    }
  }
  
  df.transaction.output <- data.frame(
    isin = isin, wkn = wkn, name = investmentname, quantity = quantity, 
    transaction_price = transaction.price, transaction_value = transaction.value, 
    transaction_fee = transaction.fee, transaction_date = transaction.date, 
    transaction_time = transaction.time, transaction_type = transaction.type
  )

  return(df.transaction.output)

}

#' Get sale transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_sale(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_sale <- function(df.pdf.page) {

  transaction.type  <- "Sale"

  ## Get start and end position of transaction
  start.transaction.data <- grep("^wertpapierabrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("zugunsten konto", df.pdf.page$text)

  ## Keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## Identify WKN and ISIN
  wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                  "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df.transaction.data$text_original[position.isin], 
                   "\\s+\\s+\\s+")[[1]][3]

  ## Identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], 
                             "\\s+\\s+\\s+")[[1]][1]

  ## Identify quantity
  quantity <- strsplit(df.transaction.data$text_original[startsWith(df.transaction.data$text, 
                                                                    "st")],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## Identify transaction price
  position.price <- grep("^kurs ", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], 
                                "\\s+\\s+")[[1]][2]
  transaction.price <- strsplit(transaction.price, " EUR ")[[1]][1]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## Identify transaction date
  transaction.datetime <- strsplit(df.transaction.data$text_original[grep("^verkauf",
                                                                          df.transaction.data$text)], 
                                   "\\s+\\s+")[[1]][2]
  transaction.date <- strsplit(transaction.datetime, " UM ")[[1]][1]
  transaction.date <- gsub("AM ", "", transaction.date)
  transaction.time <- strsplit(transaction.datetime, " UM ")[[1]][2]

  ## Identify transaction value
  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## Identify transaction fee (provision)
  fee.terms <- c("provision")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (length(position.fee1) > 0) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], 
                                   "\\s+\\s+")[[1]][3]
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee, transaction.fee1, na.rm = TRUE)
    }
  }

  df.transaction.output <- data.frame(
    isin = isin, wkn = wkn, name = investmentname, quantity = quantity, 
    transaction_price = transaction.price, transaction_value = transaction.value, 
    transaction_fee = transaction.fee, transaction_date = transaction.date, 
    transaction_time = transaction.time, transaction_type = transaction.type
  )

  return(df.transaction.output)

}

#' Get sale in part transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_salepart(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_salepart <- function(df.pdf.page) {

  transaction.type  <- "Sale - Part"

  ## Get start and end position of transaction
  start.transaction.data <- grep("^wertpapierabrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("zugunsten konto", df.pdf.page$text)

  ## Keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## Identify WKN and ISIN
  wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                  "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df.transaction.data$text_original[position.isin], 
                   "\\s+\\s+\\s+")[[1]][3]

  ## Identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], 
                             "\\s+\\s+\\s+")[[1]][1]

  ## Identify quantity
  quantity <- strsplit(df.transaction.data$text_original[startsWith(df.transaction.data$text, 
                                                                    "st")],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## Identify transaction price
  position.price <- grep("^kurs ", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], 
                                "\\s+\\s+")[[1]][2]
  transaction.price <- strsplit(transaction.price, " EUR ")[[1]][1]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## Identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("^verk. teil",
                                                                      df.transaction.data$text)], 
                               "\\s+\\s+")[[1]][2]
  transaction.date <- gsub("AM ", "", transaction.date)
  transaction.time <- NA

  ## Identify transaction value
  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## Identify transaction fee (provision)
  fee.terms <- "provision"
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (length(position.fee1) > 0) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], 
                                   "\\s+\\s+")[[1]][3]
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee, transaction.fee1, na.rm = TRUE)
    }
  }

  df.transaction.output <- data.frame(
    isin = isin, wkn = wkn, name = investmentname, quantity = quantity, 
    transaction_price = transaction.price, transaction_value = transaction.value, 
    transaction_fee = transaction.fee, transaction_date = transaction.date, 
    transaction_time = transaction.time, transaction_type = transaction.type
  )

  return(df.transaction.output)

}

#' Get dividend transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_dividend(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_dividend <- function(df.pdf.page) {

  transaction.type  <- "Dividend"

  start.transaction.data <- grep("^dividendengutschrift", df.pdf.page$text) + 1
  end.transaction.data <- grep("zu gunsten konto", df.pdf.page$text)

  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of WKN, quantity and name
  position.wkn <- grep("wkn", df.transaction.data$text)

  ## Identify WKN; ISIN is not available for this type
  isin <- ""
  wkn <- strsplit(df.transaction.data$text_original[position.wkn], 
                  "\\s+\\s+\\s+")[[1]][4]

  investmentname <- df.transaction.data$text_original[position.wkn + 1]

  quantity <- strsplit(df.transaction.data$text_original[startsWith(df.transaction.data$text, 
                                                                    "st")],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## Identify transaction price
  position.price <- grep("dividendensatz", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], 
                                "\\s+\\s+")[[1]][2]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  transaction.date <- strsplit(df.transaction.data$text_original[grep("schlusstag per ",
                                                                      df.transaction.data$text)], 
                               "\\s+\\s+")[[1]][3]
  transaction.date <- strsplit(transaction.date, " SCHLUSSTAG PER ")[[1]][2]
  transaction.time <- NA

  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  transaction.fee <- NA

  df.transaction.output <- data.frame(
    isin = isin, wkn = wkn, name = investmentname, quantity = quantity, 
    transaction_price = transaction.price, transaction_value = transaction.value, 
    transaction_fee = transaction.fee, transaction_date = transaction.date, 
    transaction_time = transaction.time, transaction_type = transaction.type
  )

  return(df.transaction.output)

}
