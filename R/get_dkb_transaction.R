#' Get transaction from Deutsche Kreditbank (DKB AG) bank statement
#'
#' @usage get_dkb_transaction(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_transaction <- function(df.pdf.page){

  ## identify type of transaction
  purchase.identifier <- "die wertpapiere schreiben wir ihrem depotkonto gut"
  dividend.identifier <- "^aussch.?ttung"

  if (any(grepl(purchase.identifier,df.pdf.page$text))) {

    df.transaction.output <- get_dkb_purchase(df.pdf.page)

  } else if (any(grepl(dividend.identifier,df.pdf.page$text)) & !(any(grepl("storno",df.pdf.page$text)))) {

    df.transaction.output <- get_dkb_dividend(df.pdf.page)

  } else {

    stop(paste("Transaction type of document is unknown."))

  } ## end of if else condition for transaction type (purchase, sale, dividend, storno, steuerliche vorabpauschale)

  return(df.transaction.output)

}

#' Get purchase transaction from Deutsche Kreditbank AG bank statement
#'
#' @usage get_dkb_purchase(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_purchase <- function(df.pdf.page) {

  ## this is a purchase transaction
  transaction.type  <- "Purchase"

  ## get start and end position of transaction
  start.transaction.data <- grep("wertpapier abrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("ausmachender betrag", df.pdf.page$text)

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## identify ISIN and WKN
  isin <- strsplit(df.transaction.data$text_original[position.isin],"\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df.transaction.data$text_original[position.isin],"\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  ## identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][2]

  ## identify quantity
  quantity <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][1]
  quantity <- gsub("St.?ck ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  position.price <- grep("ausf.?hrungskurs", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], "\\s+\\s+")[[1]][1]
  transaction.price <- gsub(" EUR","", transaction.price)
  transaction.price <- gsub("Ausf.?hrungskurs ","", transaction.price)
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("^schlusstag",df.transaction.data$text)],"\\s+\\s+")[[1]][2]
  transaction.time <- NA

  ## identify transaction value
  position.transaction.value <- grep("ausmachender betrag", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][2]
  transaction.value <- gsub("- EUR","", transaction.value)
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee
  fee.terms <- c("provision")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i],df.transaction.data$text)
    if (!rlang::is_empty(position.fee1)) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], "\\s+\\s+")[[1]][2]
      transaction.fee1 <- sub("- EUR", "", transaction.fee1, fixed = TRUE)
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

#' Get dividend transaction from Deutsche Kreditbank AG bank statement
#'
#' @usage get_dkb_dividend(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_dividend <- function(df.pdf.page){

  ## this is a dividend transaction
  transaction.type  <- "Dividend"

  ## get position of transaction
  start.transaction.data <- grep("^aussch.?ttung",df.pdf.page$text)[1] + 1
  end.transaction.data <- grep("ausmachender betrag",df.pdf.page$text)

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data,]

  ## identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## identify ISIN and WKN
  isin <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  ## identify investment name
  investmentname <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][2]

  ## identify quantity
  quantity <- strsplit(df.transaction.data$text_original[position.isin], "\\s+\\s+\\s+")[[1]][1]
  quantity <- gsub("St.?ck ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  position.price <- grep("aussch.?ttung pro st", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], "\\s+\\s+")[[1]][4]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("^zahlbarkeitstag", df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.time <- NA

  ## identify transaction value
  position.transaction.value <- grep("ausmachender betrag", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][2]
  transaction.value <- gsub("\\+ EUR", "", transaction.value)
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
