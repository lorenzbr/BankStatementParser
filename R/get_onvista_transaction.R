#' Get transaction from onVista bank statement
#'
#' @usage get_onvista_transaction(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_onvista_transaction <- function(df.pdf.page){

  ## better to replace all df.pdf with df.pdf.page
  df.pdf <- df.pdf.page

  ## identify type of transaction
  purchase.identifier <- "wir haben f.?r sie gekauft"
  sale.identifier <- "wir haben f.?r sie verkauft"
  dividend.identifier <- "ertr.?gnisgutschrift aus wertpapieren"
  storno.identifier <- "storno"
  vorabpauschale.identifier <- "steuerpflichtige vorabpauschale"


  if (any(grepl(purchase.identifier, df.pdf$text))) {

    df.transaction.output <- get_onvista_purchase(df.pdf)

  } else if (any(grepl(sale.identifier,df.pdf$text))) {

    df.transaction.output <- get_onvista_sale(df.pdf)

  } else if (any(grepl(dividend.identifier,df.pdf$text)) & !(any(grepl("storno",df.pdf$text)))) {

    df.transaction.output <- get_onvista_dividend(df.pdf)

  } else if (any(grepl(vorabpauschale.identifier,df.pdf$text))) {

    df.transaction.output <- get_onvista_vorabpauschale(df.pdf)

  } else if (any(grepl(storno.identifier,df.pdf$text))) {

    df.transaction.output <- get_onvista_stornodividend(df.pdf)

  } else {

    stop(paste("Transaction type of document is unknown."))

  } ## end of if else condition for transaction type (purchase, sale, dividend, storno, steuerliche vorabpauschale)

  return(df.transaction.output)

}

#' Get purchase transaction from onVista bank statement
#'
#' @usage get_onvista_purchase(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_onvista_purchase <- function(df.pdf.page) {

  ## this is a purchase transaction
  transaction.type  <- "Purchase"

  ## get start and end position of transaction
  start.transaction.data <- grep("wir haben f.?r sie gekauft",df.pdf.page$text) + 1
  end.transaction.data <- grep("betrag zu ihren lasten",df.pdf.page$text) + 1

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of ISIN and Gattungsbezeichnung
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## last word one row later is actual ISIN
  isin <- stringr::str_extract(df.transaction.data$text_original[position.isin], '\\w+$')
  wkn <- NA

  ## identify investment name
  investmentname <- df.transaction.data$text_original[position.isin]
  investmentname <- gsub(isin,"", investmentname)
  investmentname <- gsub("\\s+"," ", investmentname)
  investmentname <- gsub(" $","", investmentname)

  ## identify quantity
  position.quantity.and.price <- grep("^stk", df.transaction.data$text)
  quantity <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  transaction.price <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][2]
  transaction.price <- gsub("EUR ", "", transaction.price)
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("^handelstag", df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.time <- strsplit(df.transaction.data$text_original[grep("^handelszeit", df.transaction.data$text)], "\\s+\\s+")[[1]][2]

  ## identify transaction value
  position.transaction.value <- grep("betrag zu ihren lasten", df.transaction.data$text) + 1
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee (orderprovision, handelsplatzgeb.?hr, fremdspesen, maklercourtage, b.?rsengeb.?hr)
  fee.terms <- c("orderprovision", "handelsplatzgeb.?hr", "fremdspesen", "maklercourtage", "b.?rsengeb.?hr")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i], df.transaction.data$text)
    if(!(rlang::is_empty(position.fee1))){
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], "\\s+\\s+")[[1]][5]
      transaction.fee1 <- sub("-", "", transaction.fee1, fixed = TRUE)
      transaction.fee1 <- as.numeric(sub(",", ".", transaction.fee1, fixed = TRUE))
      transaction.fee <- sum(transaction.fee,transaction.fee1, na.rm = TRUE)
    }
  } ## end of for loop over all fee terms

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

}

#' Get sale transaction from onVista bank statement
#'
#' @usage get_onvista_sale(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_onvista_sale <- function(df.pdf.page) {

  ## this is a purchase transaction
  transaction.type  <- "Sale"

  ## get position of "wir haben f.?r sie gekauft"
  start.transaction.data <- grep("wir haben f.?r sie verkauft", df.pdf.page$text) + 1
  end.transaction.data <- grep("betrag zu ihren gunsten", df.pdf.page$text) + 1

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of ISIN and Gattungsbezeichnung
  position.isin <- grep("isin",df.transaction.data$text) + 1

  ## last word one row later is actual ISIN
  isin <- stringr::str_extract(df.transaction.data$text_original[position.isin], '\\w+$')
  wkn <- NA

  ## identify investment name
  investmentname <- df.transaction.data$text_original[position.isin]
  investmentname <- gsub(isin, "", investmentname)
  investmentname <- gsub("\\s+", " ", investmentname)
  investmentname <- gsub(" $", "", investmentname)

  ## identify quantity
  position.quantity.and.price <- grep("^stk",df.transaction.data$text)
  quantity <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction price
  transaction.price <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][2]
  transaction.price <- gsub("EUR ", "", transaction.price)
  transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[grep("^handelstag", df.transaction.data$text)], "\\s+\\s+")[[1]][2]
  transaction.time <- strsplit(df.transaction.data$text_original[grep("^handelszeit", df.transaction.data$text)], "\\s+\\s+")[[1]][2]

  ## identify transaction value
  position.transaction.value <- grep("betrag zu ihren gunsten",df.transaction.data$text) + 1
  transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]][3]
  transaction.value <- sub(".", "", transaction.value, fixed = TRUE)
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction fee (orderprovision, handelsplatzgeb.?hr, fremdspesen, maklercourtage, b.?rsengeb.?hr)
  fee.terms <- c("orderprovision", "handelsplatzgeb.?hr", "fremdspesen", "maklercourtage", "b.?rsengeb.?hr")
  transaction.fee <- NA
  for (i in 1:length(fee.terms)) {
    position.fee1 <- grep(fee.terms[i],df.transaction.data$text)
    if (!rlang::is_empty(position.fee1)) {
      transaction.fee1 <- strsplit(df.transaction.data$text_original[position.fee1], "\\s+\\s+")[[1]][5]
      transaction.fee1 <- sub("-", "", transaction.fee1, fixed = TRUE)
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

#' Get dividend transaction from onVista bank statement
#'
#' @usage get_onvista_dividend(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_onvista_dividend <- function(df.pdf.page) {

  ## this is a dividend transaction
  transaction.type  <- "Dividend"

  ## get position of transaction
  start.transaction.data <- grep("^dividendengutschrift|^ertragsthesaurierung|^ertr.?gnisgutschrift", df.pdf.page$text)[2] + 1
  end.transaction.data <- grep("betrag zu ihren gunsten", df.pdf.page$text) + 1

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]

  ## identify position of ISIN and Gattungsbezeichnung
  position.isin <- grep("isin",df.transaction.data$text) + 1

  ## last word one row later is actual ISIN
  isin <- stringr::str_extract(df.transaction.data$text_original[position.isin], '\\w+$')
  wkn <- NA

  ## identify investment name
  investmentname <- df.transaction.data$text_original[position.isin]
  investmentname <- gsub(isin, "", investmentname)
  investmentname <- gsub("\\s+", " ", investmentname)
  investmentname <- gsub(" $", "", investmentname)

  ## identify quantity
  position.quantity.and.price <- grep("^stk", df.transaction.data$text)
  quantity <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][3]
  transaction.time <- NA

  ## identify transaction value (takes into account EUR and USD format of transaction document)
  position.transaction.value <- grep("betrag zu ihren gunsten",df.transaction.data$text) + 1
  strings.with.transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD", strings.with.transaction.value)))) {
    transaction.value.position <- 3
  } else if (any(grepl("EUR/USD",strings.with.transaction.value))) {
    transaction.value.position <- 4
    transaction.exchangerate <- strings.with.transaction.value[2]
    if (grepl("EUR/USD", transaction.exchangerate)) {
      transaction.exchangerate <- gsub("EUR/USD ", "", transaction.exchangerate)
      transaction.exchangerate <- as.numeric(sub(",", ".", transaction.exchangerate, fixed = TRUE))
    }
  }
  transaction.value <- strings.with.transaction.value[transaction.value.position]
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction price
  transaction.price <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][4]
  if (grepl("EUR", transaction.price)) {
    transaction.price <- gsub("EUR ", "", transaction.price)
    transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))
  } else if (grepl("USD",transaction.price)) {
    transaction.price <- gsub("USD ", "", transaction.price)
    transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))
    transaction.price <- transaction.price/transaction.exchangerate
  }

  ## identify transaction fee
  transaction.fee <- NA

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

}

#' Get tax prepayment transaction from onVista bank statement
#'
#' @usage get_onvista_vorabpauschale(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_onvista_vorabpauschale <- function(df.pdf.page) {

  ## this is a dividend transaction
  transaction.type  <- "Steuerpflichtige Vorabpauschale"

  ## get position of transaction
  start.transaction.data <- grep("^steuerpflichtige vorabpauschale", df.pdf.page$text)[1] + 2
  end.transaction.data <- grep("betrag zu ihren lasten", df.pdf.page$text) + 1

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]


  ## identify position of ISIN and Gattungsbezeichnung
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## last word one row later is actual ISIN
  isin <- stringr::str_extract(df.transaction.data$text_original[position.isin], '\\w+$')
  wkn <- NA

  ## identify investment name
  investmentname <- df.transaction.data$text_original[position.isin]
  investmentname <- gsub(isin, "", investmentname)
  investmentname <- gsub("\\s+", " ", investmentname)
  investmentname <- gsub(" $", "", investmentname)

  ## identify quantity
  position.quantity.and.price <- grep("^stk", df.transaction.data$text)
  quantity <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ","",quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][3]
  transaction.time <- NA

  ## identify transaction value (takes into account EUR and USD format of transaction document)
  position.transaction.value <- grep("betrag zu ihren lasten", df.transaction.data$text) + 1
  strings.with.transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD",strings.with.transaction.value)))) {
    transaction.value.position <- 3
  } else if (any(grepl("EUR/USD",strings.with.transaction.value))) {
    transaction.value.position <- 4
    transaction.exchangerate <- strings.with.transaction.value[2]
    if (grepl("EUR/USD",transaction.exchangerate)) {
      transaction.exchangerate <- gsub("EUR/USD ", "", transaction.exchangerate)
      transaction.exchangerate <- as.numeric(sub(",", ".", transaction.exchangerate, fixed = TRUE))
    }
  }
  transaction.value <- strings.with.transaction.value[transaction.value.position]
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction price
  transaction.price <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][4]
  if (grepl("EUR",transaction.price)) {
    transaction.price <- gsub("EUR ", "", transaction.price)
    transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))
  } else if (grepl("USD",transaction.price)) {
    transaction.price <- gsub("USD ", "", transaction.price)
    transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))
    transaction.price <- transaction.price / transaction.exchangerate
  }

  ## identify transaction fee
  transaction.fee <- NA

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)
  return(df.transaction.output)

}

#' Get cancellation of dividend transaction from onVista bank statement
#'
#' @usage get_onvista_stornodividend(df.pdf.page)
#' @param df.pdf.page A data.frame which results from the function [textreadr::read_pdf()].
#'
#' @export
get_onvista_stornodividend <- function(df.pdf.page) {

  ## this is a dividend transaction
  transaction.type  <- "Storno - Dividend"

  ## get position of transaction
  start.transaction.data <- grep("^storno - ertragsthesaurierung|^storno - ertr.?gnisgutschrift", df.pdf.page$text) + 1
  end.transaction.data <- grep("storno unserer ertr.?gnisgutschrift", df.pdf.page$text) - 1

  ## keep only text with transaction information
  df.transaction.data <- df.pdf.page[start.transaction.data:end.transaction.data, ]


  ## identify position of ISIN and Gattungsbezeichnung
  position.isin <- grep("isin", df.transaction.data$text) + 1

  ## last word one row later is actual ISIN
  isin <- stringr::str_extract(df.transaction.data$text_original[position.isin], '\\w+$')
  wkn <- NA

  ## identify investment name
  investmentname <- df.transaction.data$text_original[position.isin]
  investmentname <- gsub(isin, "", investmentname)
  investmentname <- gsub("\\s+", " ", investmentname)
  investmentname <- gsub(" $", "", investmentname)

  ## identify quantity
  position.quantity.and.price <- grep("^stk", df.transaction.data$text)
  quantity <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  ## identify transaction date
  transaction.date <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][3]
  transaction.time <- NA

  ## identify transaction value (takes into account EUR and USD format of transaction document)
  position.transaction.value <- grep("betrag zu ihren lasten", df.transaction.data$text) + 1
  strings.with.transaction.value <- strsplit(df.transaction.data$text_original[position.transaction.value], "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD", strings.with.transaction.value)))) {
    transaction.value.position <- 3
  } else if (any(grepl("EUR/USD",strings.with.transaction.value))) {
    transaction.value.position <- 4
    transaction.exchangerate <- strings.with.transaction.value[2]
    if (grepl("EUR/USD", transaction.exchangerate)) {
      transaction.exchangerate <- gsub("EUR/USD ", "", transaction.exchangerate)
      transaction.exchangerate <- as.numeric(sub(",", ".", transaction.exchangerate, fixed = TRUE))
    }
  }
  transaction.value <- strings.with.transaction.value[transaction.value.position]
  transaction.value <- as.numeric(sub(",", ".", transaction.value, fixed = TRUE))

  ## identify transaction price
  transaction.price <- strsplit(df.transaction.data$text_original[position.quantity.and.price], "\\s+\\s+")[[1]][4]
  if (grepl("EUR", transaction.price)) {
    transaction.price <- gsub("EUR ", "", transaction.price)
    transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))
  } else if (grepl("USD", transaction.price)) {
    transaction.price <- gsub("USD ", "", transaction.price)
    transaction.price <- as.numeric(sub(",", ".", transaction.price, fixed = TRUE))
    transaction.price <- transaction.price/transaction.exchangerate
  }

  ## identify transaction fee
  transaction.fee <- NA

  ## store data in data frame
  df.transaction.output <- data.frame(isin = isin, wkn = wkn, name = investmentname,quantity = quantity, transaction_price = transaction.price,
                                      transaction_value = transaction.value, transaction_fee = transaction.fee,
                                      transaction_date = transaction.date,transaction_time = transaction.time,
                                      transaction_type = transaction.type)

  return(df.transaction.output)

}
