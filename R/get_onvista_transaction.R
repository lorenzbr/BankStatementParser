#' Get transaction from onVista bank statement
#'
#' @usage get_onvista_transaction(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_onvista_transaction <- function(df_pdf_page) {

  ## To do: Better to replace all df_pdf with df_pdf_page
  df_pdf <- df_pdf_page

  purchase_identifier <- "wir haben f.?r sie gekauft"
  sale_identifier <- "wir haben f.?r sie verkauft"
  dividend_identifier <- "ertr.?gnisgutschrift aus wertpapieren"
  storno_identifier <- "storno"
  vorabpauschale.identifier <- "steuerpflichtige vorabpauschale"

  if (any(grepl(purchase_identifier, df_pdf$text))) {

    df_transaction_output <- get_onvista_purchase(df_pdf)

  } else if (any(grepl(sale_identifier, df_pdf$text))) {

    df_transaction_output <- get_onvista_sale(df_pdf)

  } else if (any(grepl(dividend_identifier, df_pdf$text)) 
             && !any(grepl(storno_identifier, df_pdf$text))) {

    df_transaction_output <- get_onvista_dividend(df_pdf)

  } else if (any(grepl(vorabpauschale.identifier, df_pdf$text))) {

    df_transaction_output <- get_onvista_vorabpauschale(df_pdf)

  } else if (any(grepl(storno_identifier, df_pdf$text))) {

    df_transaction_output <- get_onvista_stornodividend(df_pdf)

  } else {

    stop("Transaction type of document is unknown.")

  }

  return(df_transaction_output)

}

#' Get purchase transaction from onVista bank statement
#'
#' @usage get_onvista_purchase(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_onvista_purchase <- function(df_pdf_page) {

  transaction_type  <- "Purchase"

  start_transaction <- grep("wir haben f.?r sie gekauft", df_pdf_page$text) + 1
  end_transaction <- grep("betrag zu ihren lasten", df_pdf_page$text) + 1

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN and Gattungsbezeichnung
  position_isin <- grep("isin", df_transaction_data$text) + 1

  ## Last word one row later is the actual ISIN
  isin <- stringr::str_extract(df_transaction_data$text_original[position_isin], 
                               pattern = '\\w+$')
  wkn <- NA

  investment_name <- df_transaction_data$text_original[position_isin]
  investment_name <- gsub(isin, "", investment_name)
  investment_name <- gsub("\\s+"," ", investment_name)
  investment_name <- gsub(" $","", investment_name)

  position_quantity_and_price <- grep("^stk", df_transaction_data$text)
  quantity <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))
  
  ## Identify transaction value (takes into account EUR and USD format 
  ## of transaction document)
  position_transaction_value <- grep(
    "betrag zu ihren lasten", df_transaction_data$text) + 1
  strings_with_transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD", strings_with_transaction_value)))) {
    transaction_value_position <- 3
  } else if (any(grepl("EUR/USD", strings_with_transaction_value))) {
    transaction_value_position <- 4
    transaction_exchangerate <- strings_with_transaction_value[2]
    if (grepl("EUR/USD", transaction_exchangerate)) {
      transaction_exchangerate <- gsub("EUR/USD ", "", transaction_exchangerate)
      transaction_exchangerate <- as.numeric(
        sub(",", ".", transaction_exchangerate, fixed = TRUE))
    }
  }
  transaction_value <- strings_with_transaction_value[transaction_value_position]
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))
  
  transaction_price <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][2]
  if (grepl("EUR", transaction_price)) {
    transaction_price <- gsub("EUR ", "", transaction_price)
    transaction_price <- sub(".", "", transaction_price, fixed = TRUE)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
  } else if (grepl("USD", transaction_price)) {
    transaction_price <- gsub("USD ", "", transaction_price)
    transaction_price <- sub(".", "", transaction_price, fixed = TRUE)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
    transaction_price <- transaction_price / transaction_exchangerate
  }

  transaction_date <- strsplit(
    df_transaction_data$text_original[grep("^handelstag", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]
  transaction_time <- strsplit(
    df_transaction_data$text_original[grep("^handelszeit", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]

  ## Identify transaction fee (orderprovision, handelsplatzgeb.?hr, 
  ## fremdspesen, maklercourtage, b.?rsengeb.?hr)
  fee_terms <- c("orderprovision", "handelsplatzgeb.?hr", "fremdspesen", 
                 "maklercourtage", "b.?rsengeb.?hr")
  transaction_fee <- NA
  for (i in 1:length(fee_terms)) {
    position_fee1 <- grep(fee_terms[i], df_transaction_data$text)
    if (length(position_fee1) > 0) {
      transaction_fee1 <- strsplit(
        df_transaction_data$text_original[position_fee1], 
        "\\s+\\s+")[[1]][5]
      transaction_fee1 <- sub("-", "", transaction_fee1, fixed = TRUE)
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

#' Get sales transaction from onVista bank statement
#'
#' @usage get_onvista_sale(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_onvista_sale <- function(df_pdf_page) {

  transaction_type  <- "Sale"

  start_transaction <- grep("wir haben f.?r sie verkauft", df_pdf_page$text) + 1
  end_transaction <- grep("betrag zu ihren gunsten", df_pdf_page$text) + 1
  
  ## Sometimes there is 'betrag zu ihren gunsten' twice on the same page 
  ## (because of taxes/veraeusserungsverlust aktien usw. then take the last row)
  end_transaction <- end_transaction[length(end_transaction)]

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN and Gattungsbezeichnung
  position_isin <- grep("isin", df_transaction_data$text) + 1

  ## Last word one row later is actual ISIN
  isin <- stringr::str_extract(df_transaction_data$text_original[position_isin], 
                               pattern = '\\w+$')
  
  ## For other banks WKN is available, here simply set to NA
  wkn <- NA

  investment_name <- df_transaction_data$text_original[position_isin]
  investment_name <- gsub(isin, "", investment_name)
  investment_name <- gsub("\\s+", " ", investment_name)
  investment_name <- gsub(" $", "", investment_name)

  position_quantity_and_price <- grep("^stk", df_transaction_data$text)
  quantity <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))
  
  ## Identify transaction value (takes into account EUR and USD format 
  ## of transaction document)
  position_transaction_values <- grep("betrag zu ihren gunsten", 
                                      df_transaction_data$text) + 1
  transaction_value_final <- 0
  for (position_transaction_value in position_transaction_values) {
    strings_with_transaction_value <- strsplit(
      df_transaction_data$text_original[position_transaction_value], 
      "\\s+\\s+")[[1]]
    if (!any(grepl("EUR/USD", strings_with_transaction_value))) {
      transaction_value_position <- which(strings_with_transaction_value == "EUR") + 1
    } else if (any(grepl("EUR/USD", strings_with_transaction_value))) {
      transaction_value_position <- 4
      transaction_exchangerate <- strings_with_transaction_value[2]
      if (grepl("EUR/USD", transaction_exchangerate)) {
        transaction_exchangerate <- gsub("EUR/USD ", "", transaction_exchangerate)
        transaction_exchangerate <- as.numeric(
          sub(",", ".", transaction_exchangerate, fixed = TRUE))
      }
    }
    transaction_value <- strings_with_transaction_value[transaction_value_position]
    transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
    transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                        fixed = TRUE))
    transaction_value_final <- transaction_value_final + transaction_value
  }
  transaction_value <- transaction_value_final
  
  transaction_price <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][2]
  if (grepl("EUR", transaction_price)) {
    transaction_price <- gsub("EUR ", "", transaction_price)
    transaction_price <- sub(".", "", transaction_price, fixed = TRUE)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
  } else if (grepl("USD", transaction_price)) {
    transaction_price <- gsub("USD ", "", transaction_price)
    transaction_price <- sub(".", "", transaction_price, fixed = TRUE)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
    transaction_price <- transaction_price / transaction_exchangerate
  }

  transaction_date <- strsplit(
    df_transaction_data$text_original[grep("^handelstag", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]
  transaction_time <- strsplit(
    df_transaction_data$text_original[grep("^handelszeit", 
                                           df_transaction_data$text)], 
    "\\s+\\s+")[[1]][2]

  ## Identify transaction fee (orderprovision, handelsplatzgeb.?hr, fremdspesen, 
  ## maklercourtage, b.?rsengeb.?hr)
  fee_terms <- c("orderprovision", "handelsplatzgeb.?hr", "fremdspesen", 
                 "maklercourtage", "b.?rsengeb.?hr")
  transaction_fee <- NA
  for (i in 1:length(fee_terms)) {
    position_fee1 <- grep(fee_terms[i], df_transaction_data$text)
    if (length(position_fee1) > 0) {
      transaction_fee1 <- strsplit(
        df_transaction_data$text_original[position_fee1], 
        "\\s+\\s+")[[1]][5]
      transaction_fee1 <- sub("-", "", transaction_fee1, fixed = TRUE)
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

#' Get dividend transaction from onVista bank statement
#'
#' @usage get_onvista_dividend(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_onvista_dividend <- function(df_pdf_page) {

  transaction_type  <- "Dividend"

  ## Get position of transaction
  start_transaction <- grep(
    "^dividendengutschrift|^ertragsthesaurierung|^ertr.?gnisgutschrift", 
    df_pdf_page$text)[2] + 1
  end_transaction <- grep("betrag zu ihren gunsten", df_pdf_page$text) + 1

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]

  ## Identify position of ISIN and Gattungsbezeichnung
  position_isin <- grep("isin", df_transaction_data$text) + 1

  ## Last word one row later is actual ISIN
  isin <- stringr::str_extract(df_transaction_data$text_original[position_isin], 
                               pattern = '\\w+$')
  wkn <- NA

  investment_name <- df_transaction_data$text_original[position_isin]
  investment_name <- gsub(isin, "", investment_name)
  investment_name <- gsub("\\s+", " ", investment_name)
  investment_name <- gsub(" $", "", investment_name)

  position_quantity_and_price <- grep("^stk", df_transaction_data$text)
  quantity <- strsplit(df_transaction_data$text_original[position_quantity_and_price], 
                       "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][3]
  transaction_time <- NA

  ## Identify transaction value (takes into account EUR and USD format of 
  ## transaction document)
  position_transaction_value <- grep(
    "betrag zu ihren gunsten", df_transaction_data$text) + 1
  strings_with_transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD", strings_with_transaction_value)))) {
    transaction_value_position <- 3
  } else if (any(grepl("EUR/USD", strings_with_transaction_value))) {
    transaction_value_position <- 4
    transaction_exchangerate <- strings_with_transaction_value[2]
    if (grepl("EUR/USD", transaction_exchangerate)) {
      transaction_exchangerate <- gsub("EUR/USD ", "", transaction_exchangerate)
      transaction_exchangerate <- as.numeric(
        sub(",", ".", transaction_exchangerate, fixed = TRUE))
    }
  }
  transaction_value <- strings_with_transaction_value[transaction_value_position]
  transaction_value <- sub(".", "", transaction_value, fixed = TRUE)
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  transaction_price <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][4]
  if (grepl("EUR", transaction_price)) {
    transaction_price <- gsub("EUR ", "", transaction_price)
    transaction_price <- sub(".", "", transaction_price, fixed = TRUE)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
  } else if (grepl("USD", transaction_price)) {
    transaction_price <- gsub("USD ", "", transaction_price)
    transaction_price <- sub(".", "", transaction_price, fixed = TRUE)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
    transaction_price <- transaction_price / transaction_exchangerate
  }

  ## Fee not available for this transaction type
  transaction_fee <- NA

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}

#' Get tax prepayment transaction from onVista bank statement
#'
#' @usage get_onvista_vorabpauschale(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_onvista_vorabpauschale <- function(df_pdf_page) {

  transaction_type  <- "Steuerpflichtige Vorabpauschale"

  ## Get position of transaction
  start_transaction <- grep("^steuerpflichtige vorabpauschale", 
                                 df_pdf_page$text)[1] + 2
  end_transaction <- grep("betrag zu ihren lasten", df_pdf_page$text) + 1

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]


  ## Identify position of ISIN and Gattungsbezeichnung
  position_isin <- grep("isin", df_transaction_data$text) + 1

  ## Last word one row later is actual ISIN
  isin <- stringr::str_extract(df_transaction_data$text_original[position_isin], 
                               pattern = '\\w+$')
  wkn <- NA

  investment_name <- df_transaction_data$text_original[position_isin]
  investment_name <- gsub(isin, "", investment_name)
  investment_name <- gsub("\\s+", " ", investment_name)
  investment_name <- gsub(" $", "", investment_name)

  position_quantity_and_price <- grep("^stk", df_transaction_data$text)
  quantity <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ","", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][3]
  transaction_time <- NA

  ## Identify transaction value (takes into account EUR and USD format of 
  ## transaction document)
  position_transaction_value <- grep(
    "betrag zu ihren lasten", df_transaction_data$text) + 1
  strings_with_transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD", strings_with_transaction_value)))) {
    transaction_value_position <- 3
  } else if (any(grepl("EUR/USD", strings_with_transaction_value))) {
    transaction_value_position <- 4
    transaction_exchangerate <- strings_with_transaction_value[2]
    if (grepl("EUR/USD", transaction_exchangerate)) {
      transaction_exchangerate <- gsub("EUR/USD ", "", transaction_exchangerate)
      transaction_exchangerate <- as.numeric(
        sub(",", ".", transaction_exchangerate, fixed = TRUE))
    }
  }
  transaction_value <- strings_with_transaction_value[transaction_value_position]
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  transaction_price <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][4]
  if (grepl("EUR", transaction_price)) {
    transaction_price <- gsub("EUR ", "", transaction_price)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
  } else if (grepl("USD", transaction_price)) {
    transaction_price <- gsub("USD ", "", transaction_price)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
    transaction_price <- transaction_price / transaction_exchangerate
  }

  ## Fee not available for this transaction type
  transaction_fee <- NA

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )
  
  return(df_transaction_output)

}

#' Get cancellation of dividend transaction from onVista bank statement
#'
#' @usage get_onvista_stornodividend(df_pdf_page)
#' @param df_pdf_page A data frame which results from the 
#' function [textreadr::read_pdf()].
#' 
#' @return A data frame containing the parsed transaction.
#'
#' @export
get_onvista_stornodividend <- function(df_pdf_page) {

  transaction_type  <- "Storno - Dividend"

  ## Get position of transaction
  start_transaction <- grep("^storno - ertragsthesaurierung|^storno - ertr.?gnisgutschrift", 
                                 df_pdf_page$text) + 1
  end_transaction <- grep("storno unserer ertr.?gnisgutschrift", 
                               df_pdf_page$text) - 1

  df_transaction_data <- df_pdf_page[start_transaction:end_transaction, ]


  ## Identify position of ISIN and Gattungsbezeichnung
  position_isin <- grep("isin", df_transaction_data$text) + 1

  ## Last word one row later is actual ISIN
  isin <- stringr::str_extract(df_transaction_data$text_original[position_isin], 
                               pattern = '\\w+$')
  wkn <- NA

  investment_name <- df_transaction_data$text_original[position_isin]
  investment_name <- gsub(isin, "", investment_name)
  investment_name <- gsub("\\s+", " ", investment_name)
  investment_name <- gsub(" $", "", investment_name)

  position_quantity_and_price <- grep("^stk", df_transaction_data$text)
  quantity <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][1]
  quantity <- gsub("STK ", "", quantity)
  quantity <- as.numeric(sub(",", ".", quantity, fixed = TRUE))

  transaction_date <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][3]
  transaction_time <- NA

  ## Identify transaction value (takes into account EUR and USD format 
  ## of transaction document)
  position_transaction_value <- grep(
    "betrag zu ihren lasten", df_transaction_data$text) + 1
  strings_with_transaction_value <- strsplit(
    df_transaction_data$text_original[position_transaction_value], 
    "\\s+\\s+")[[1]]
  if (!(any(grepl("EUR/USD", strings_with_transaction_value)))) {
    transaction_value_position <- 3
  } else if (any(grepl("EUR/USD", strings_with_transaction_value))) {
    transaction_value_position <- 4
    transaction_exchangerate <- strings_with_transaction_value[2]
    if (grepl("EUR/USD", transaction_exchangerate)) {
      transaction_exchangerate <- gsub("EUR/USD ", "", transaction_exchangerate)
      transaction_exchangerate <- as.numeric(
        sub(",", ".", transaction_exchangerate, fixed = TRUE))
    }
  }
  transaction_value <- strings_with_transaction_value[transaction_value_position]
  transaction_value <- as.numeric(sub(",", ".", transaction_value, 
                                      fixed = TRUE))

  transaction_price <- strsplit(
    df_transaction_data$text_original[position_quantity_and_price], 
    "\\s+\\s+")[[1]][4]
  if (grepl("EUR", transaction_price)) {
    transaction_price <- gsub("EUR ", "", transaction_price)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
  } else if (grepl("USD", transaction_price)) {
    transaction_price <- gsub("USD ", "", transaction_price)
    transaction_price <- as.numeric(sub(",", ".", transaction_price, 
                                        fixed = TRUE))
    transaction_price <- transaction_price / transaction_exchangerate
  }

  ## Fee not available for this transaction type
  transaction_fee <- NA

  df_transaction_output <- data.frame(
    isin = isin, wkn = wkn, name = investment_name, quantity = quantity, 
    transaction_price = transaction_price, transaction_value = transaction_value, 
    transaction_fee = transaction_fee, transaction_date = transaction_date, 
    transaction_time = transaction_time, transaction_type = transaction_type
  )

  return(df_transaction_output)

}
