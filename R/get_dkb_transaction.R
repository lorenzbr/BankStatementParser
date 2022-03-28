#' Get transaction from Deutsche Kreditbank (DKB AG) bank statement
#'
#' @usage get_dkb_transaction(df.pdf.page)
#' @param df.pdf.page A data frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_dkb_transaction <- function(df.pdf.page) {

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

  start.transaction.data <- grep("wertpapier abrechnung", df.pdf.page$text) + 1
  end.transaction.data <- grep("ausmachender betrag", df.pdf.page$text)

  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## Identify position of ISIN, WKN, quantity and name
  position.isin <- grep("isin", df.transaction.data$text) + 1

  isin <- strsplit(df.transaction.data$text_original[position.isin], 
                   "\\s+\\s+\\s+")[[1]][3]
  wkn <- strsplit(df.transaction.data$text_original[position.isin], 
                  "\\s+\\s+\\s+")[[1]][4]
  wkn <- gsub("\\(|\\)", "", wkn)

  investment.name <- strsplit(df.transaction.data$text_original[position.isin], 
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

  position.fee <- grep("provision", df.transaction.data$text)
  if (length(position.fee) > 0) {
    transaction.fee <- strsplit(df.transaction.data$text_original[position.fee], 
                                 "\\s+\\s+")[[1]][2]
    transaction.fee <- sub("- EUR", "", transaction.fee, fixed = TRUE)
    transaction.fee <- as.numeric(sub(",", ".", transaction.fee, fixed = TRUE))
  }

  df.transaction.output <- data.frame(
    isin = isin, wkn = wkn, name = investment.name, quantity = quantity, 
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

  investment.name <- strsplit(df.transaction.data$text_original[position.isin], 
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
    isin = isin, wkn = wkn, name = investment.name, quantity = quantity, 
    transaction_price = transaction.price, transaction_value = transaction.value, 
    transaction_fee = transaction.fee, transaction_date = transaction.date, 
    transaction_time = transaction.time, transaction_type = transaction.type
  )

  return(df.transaction.output)

}
