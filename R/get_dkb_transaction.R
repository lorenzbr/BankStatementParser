#' Get transaction from Deutsche Kreditbank (DKB AG) bank statement
#'
#' @usage get_dkb_transaction(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_transaction <- function(df.pdf.page) {

  ## Identify type of transaction
  purchase.identifier <- "die wertpapiere schreiben wir ihrem depotkonto gut"
  dividend.identifier <- "^aussch.?ttung"
  storno.identifier <- "storno"

  if (any(grepl(purchase.identifier, df.pdf.page$text))) {

    df.transaction.output <- get_dkb_purchase(df.pdf.page)

  } else if (any(grepl(dividend.identifier, df.pdf.page$text)) 
             && !(any(grepl(storno.identifier, df.pdf.page$text)))) {

    df.transaction.output <- get_dkb_dividend(df.pdf.page)

  } else {

    stop(paste("Transaction type of document is unknown."))

  }

  return(df.transaction.output)

}

#' Get purchase transaction from Deutsche Kreditbank AG bank statement
#'
#' @usage get_dkb_purchase(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_purchase <- function(df.pdf.page) {

  transaction.type  <- "Purchase"

  ## Get start and end position of transaction
  start.transaction.data <- grep("wertpapier abrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("ausmachender betrag", df.pdf.page$text)

  ## Keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## Identify ISIN and WKN
  isin <- strsplit(df.transaction.data$text_original[position.isin], 
                   "\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                  "\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  investmentname <- strsplit(df.transaction.data$text_original[position.isin], 
                             "\\s+\\s+\\s+")[[1]][2]

  quantity <- strsplit(df.transaction.data$text_original[position.isin], 
                       "\\s+\\s+\\s+")[[1]][1]
  quantity <- gsub("St.?ck ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position.price <- grep("ausf.?hrungskurs", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], 
                                "\\s+\\s+")[[1]][1]
  transaction.price <- gsub(" EUR","", transaction.price)
  transaction.price <- gsub("Ausf.?hrungskurs ","", transaction.price)
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  transaction.date <- strsplit(df.transaction.data$text_original[grep("^schlusstag", 
                                                                      df.transaction.data$text)], 
                               "\\s+\\s+")[[1]][2]
  transaction.time <- NA

  position.transaction.value <- grep("ausmachender betrag", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][2]
  transaction.value <- gsub("- EUR", "", transaction.value)
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## Identify transaction fees
  fee.terms <- c("provision")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if (length(position.fee1) > 0) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], 
                                   "\\s+\\s+")[[1]][2]
      transaction.fee1 <- sub("- EUR", "", transaction.fee1, fixed = TRUE)
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

#' Get dividend transaction from Deutsche Kreditbank AG bank statement
#'
#' @usage get_dkb_dividend(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_dividend <- function(df.pdf.page) {

  transaction.type  <- "Dividend"

  start.transaction.data <- grep("^aussch.?ttung", df.pdf.page$text)[1] + 1
  end.transaction.data <- grep("ausmachender betrag", df.pdf.page$text)

  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  isin <- strsplit(df.transaction.data$text_original[position.isin], 
                   "\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                  "\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  investmentname <- strsplit(df.transaction.data$text_original[position.isin], 
                             "\\s+\\s+\\s+")[[1]][2]

  quantity <- strsplit(df.transaction.data$text_original[position.isin], 
                       "\\s+\\s+\\s+")[[1]][1]
  quantity <- gsub("St.?ck ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  position.price <- grep("aussch.?ttung pro st", df.transaction.data$text)
  transaction.price <- strsplit(df.transaction.data$text_original[position.price], 
                                "\\s+\\s+")[[1]][4]
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  transaction.date <- strsplit(df.transaction.data$text_original[grep("^zahlbarkeitstag", 
                                                                      df.transaction.data$text)], 
                               "\\s+\\s+")[[1]][2]
  transaction.time <- NA

  position.transaction.value <- grep("ausmachender betrag", df.transaction.data$text)
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], 
                                "\\s+\\s+")[[1]][2]
  transaction.value <- gsub("\\+ EUR", "", transaction.value)
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
