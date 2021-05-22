#' Get transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_transaction(df.pdf.page)
#' @param df.pdf.page A \code{data.frame} which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_transaction <- function(df.pdf.page){

  ## identify type of transaction
  purchase.identifier <- "^kauf"
  sale.identifier <- "^verkauf"
  salepart.identifier <- "^verk. teil"
  dividend.identifier <- "^dividendengutschrift"

  if (any(grepl(purchase.identifier, df.pdf.page$text))) {

    df.transaction.output <- get_cortalconsors_purchase(df.pdf.page)

  } else if (any(grepl(sale.identifier, df.pdf.page$text)) & !(any(grepl(salepart.identifier, df.pdf.page$text)))) {

    df.transaction.output <- get_cortalconsors_sale(df.pdf.page)

  } else if (any(grepl(salepart.identifier, df.pdf.page$text))) {

    df.transaction.output <- get_cortalconsors_salepart(df.pdf.page)

  } else if (any(grepl(dividend.identifier, df.pdf.page$text)) & !(any(grepl("storno",df.pdf.page$text)))) {

    df.transaction.output <- get_cortalconsors_dividend(df.pdf.page)

  } else {

    stop(paste("Transaction type of document is unknown."))

  } ## end of if else condition for transaction type (purchase, sale, dividend, storno, steuerliche vorabpauschale)

  return(df.transaction.output)

}

#' Get purchase transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_purchase(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_purchase <- function(df.pdf.page) {

  ## this is a purchase transaction
  transaction.type  <- "Purchase"

  ## get start and end position of transaction
  start.transaction.data <- grep("^wertpapierabrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("zulasten konto", df.pdf.page$text)

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data,]

  ## identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## identify WKN and ISIN
  wkn <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][3]

  ## identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][1]

  ## identify quantity
  quantity <- strsplit(df.transaction.data$text_original[grepl("^st", df.transaction.data$text)],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position.price <- grep("^kurs ", df.transaction.data$text)

  ## identify transaction price
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], "\\s+\\s+")[[1]][2]
  transaction.price <- strsplit(transaction.price, " EUR ")[[1]][1]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.datetime <- strsplit(df.transaction.data$text_original[grep("^kauf",
                                        df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.date <- strsplit(transaction.datetime, " UM ")[[1]][1]
  transaction.date <- gsub("AM ", "", transaction.date)
  transaction.time <- strsplit(transaction.datetime, " UM ")[[1]][2]

  ## identify transaction value
  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee
  fee.terms <- c("grundgeb.?hr")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (!rlang::is_empty(position.fee1)) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], "\\s+\\s+")[[1]][3]
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee, transaction.fee1, na.rm = TRUE)
    }
  } ## end of for loop over all fee terms

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

}

#' Get sale transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_sale(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_sale <- function(df.pdf.page) {

  ## this is a purchase transaction
  transaction.type  <- "Sale"

  ## get start and end position of transaction
  start.transaction.data <- grep("^wertpapierabrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("zugunsten konto", df.pdf.page$text)

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## identify WKN and ISIN
  wkn <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][3]

  ## identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][1]

  ## identify quantity
  quantity <- strsplit(df.transaction.data$text_original[grepl("^st", df.transaction.data$text)],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  position.price <- grep("^kurs ",df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], "\\s+\\s+")[[1]][2]
  transaction.price <- strsplit(transaction.price, " EUR ")[[1]][1]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.datetime <- strsplit(df.transaction.data$text_original[grep("^verkauf",
                                                                          df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.date <- strsplit(transaction.datetime, " UM ")[[1]][1]
  transaction.date <- gsub("AM ", "", transaction.date)
  transaction.time <- strsplit(transaction.datetime, " UM ")[[1]][2]

  ## identify transaction value
  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee (provision)
  fee.terms <- c("provision")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (!rlang::is_empty(position.fee1)) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], "\\s+\\s+")[[1]][3]
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee, transaction.fee1, na.rm = TRUE)
    }
  } ## end of for loop over all fee terms

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

} ## end of function get_cortalconsors_sale

#' Get sale in part transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_salepart(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_salepart <- function(df.pdf.page) {

  ## this is a purchase transaction
  transaction.type  <- "Sale - Part"

  ## get start and end position of transaction
  start.transaction.data <- grep("^wertpapierabrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("zugunsten konto", df.pdf.page$text)

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## identify WKN and ISIN
  wkn <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][2]
  isin <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][3]

  ## identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][1]

  ## identify quantity
  quantity <- strsplit(df.transaction.data$text_original[grepl("^st", df.transaction.data$text)],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  position.price <- grep("^kurs ", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], "\\s+\\s+")[[1]][2]
  transaction.price <- strsplit(transaction.price, " EUR ")[[1]][1]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("^verk. teil",
                                                                          df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.date <- gsub("AM ", "", transaction.date)
  transaction.time <- NA

  ## identify transaction value
  position.transaction.value <- grep("^wert ", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee (provision)
  fee.terms <- "provision"
  transaction.fee <- NA
  for(i in 1:length(fee.terms)){
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (!(rlang::is_empty(position.fee1))) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], "\\s+\\s+")[[1]][3]
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee, transaction.fee1, na.rm = TRUE)
    }
  } ## end of for loop over all fee terms

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

}

#' Get dividend transaction from Cortal Consors bank statement
#'
#' @usage get_cortalconsors_dividend(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_cortalconsors_dividend <- function(df.pdf.page) {

  ## this is a dividend transaction
  transaction.type  <- "Dividend"

  ## get start and end position of transaction
  start.transaction.data <- grep("^dividendengutschrift", df.pdf.page$text) + 1
  end.transaction.data <- grep("zu gunsten konto", df.pdf.page$text)

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of WKN, quantity and name
  position.wkn <- grep("wkn", df.transaction.data$text)

  ## identify WKN
  isin <- ""
  wkn <- strsplit(df.transaction.data$text_original[position.wkn], "\\s+\\s+\\s+")[[1]][4]

  ## identify investment name
  investmentname <- df.transaction.data$text_original[position.wkn + 1]

  ## identify quantity
  quantity <- strsplit(df.transaction.data$text_original[grepl("^st", df.transaction.data$text)],
                       "\\s+\\s+\\s+")[[1]][2]
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  position.price <- grep("dividendensatz", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], "\\s+\\s+")[[1]][2]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("schlusstag per ",
                                                                          df.transaction.data$text)], "\\s+\\s+")[[1]][3]
  transaction.date <- strsplit(transaction.date," SCHLUSSTAG PER ")[[1]][2]
  transaction.time <- NA

  ## identify transaction value
  position.transaction.value <- grep("^wert ",df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value],"\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee
  transaction.fee <- NA

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

}
