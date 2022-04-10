#' Get transactions from bank statements
#
#' Parse all PDF bank statements to get transaction data
#'
#' @usage get_transactions(file, document_name, all_pages = TRUE, 
#'                         depot_bank = NA)
#' @param file A single character string. The full path and file name 
#' of the PDF.
#' @param document_name A single character string. The name of the PDF.
#' @param all_pages Logical (default is TRUE) which indicates whether to 
#' read every page in PDF.
#' @param depot_bank A single character string. Is optional (default is NA).
#' @return \emph{get_transactions} returns a data frame with the 
#' following columns:
#' \itemize{
#' \item{isin}{ The identifier ISIN}
#' \item{wkn}{ The identifier WKN}
#' \item{name}{ Name of the investment}
#' \item{quantity}{ Quantity/number of stocks.}
#' \item{transaction_price}{ Price of the transaction.}
#' \item{transaction_value}{ Value of the transaction (includes fees).}
#' \item{transaction_fee}{ Fee of the transaction. Adds up all fees.}
#' \item{transaction_date}{ Date of the tranaction.}
#' \item{transaction_time}{ Time of the transaction.}
#' \item{transactiontype}{ Type of the transaction (e.g., purchase, sale).}
#' \item{document_page}{ Page of the transaction in the PDF document.}
#' \item{document_name}{ Name of the PDF document.}
#' }
#'
#' @export
get_transactions <- function(file, document_name, all_pages = TRUE, 
                             depot_bank = NA) {

  tryCatch({

    df_pdf <- read_pdf(file)

    df_pdf$text_original <- df_pdf$text

    df_pdf$text <- tolower(df_pdf$text)

    if (is.na(depot_bank)) {

      ## This is the set of currently all available depot banks / brokers
      onvista <- "OnVista"
      dkb <- "DKB AG"
      cortalconsors <- "Cortal Consors"
      scalablecapital <- "Scalable Capital"

      #### IDENTIFY DEPOT BANK
      if (any(grepl("cortal consors", df_pdf$text))) {
        
        depot_bank <- cortalconsors

      } else if (any(grepl("scalable capital", df_pdf$text))) {
        
        depot_bank <- scalablecapital
        
      } else if (any(grepl("onvista bank", df_pdf$text)) ||
                 any(grepl("dieser beleg wird maschinell erstellt", 
                              df_pdf$text))) {
        
        depot_bank <- onvista
        
      } else if (any(grepl("10919 berlin", df_pdf$text))) {
          
          depot_bank <- dkb
        
      }

    }
    
    if (is.na(depot_bank)) {
      
      ## Need to apply OCR because machine written PDF sometimes does not 
      ## store the name of the bank
      ## THIS IS VERY SLOW!!!
      temp <- tempdir()
      fls <- file.path(
        temp, paste0(gsub("\\.pdf$", "", document_name), ".png"))
      png_files <- pdftools::pdf_convert(
        file, pages = 1, dpi = 150, filenames = fls)
      text <- tesseract::ocr(png_files)
      text <- tolower(text)
      unlink(png_files, TRUE, TRUE)
      
      if (grepl("onvista", text)) {
        
        depot_bank <- onvista
        
      } else if (grepl("deutsche kreditbank", text)) {
        
        depot_bank <- dkb
        
      } else { 
        
        stop("PDF not issued by onVista, DKB or Cortal Consors. Other depot banks or brokers are not included.") 
        
      }
      
    }
    
    first_page <- 1
    df_pdf_page <- df_pdf[df_pdf$page_id == first_page, ]

    ## Get transaction from first page if bank is from a known/identified bank
    if (depot_bank == onvista) {
      
      ## For onVista, sometimes several pages with transactions exist
      ## If second page of a transaction exists with no relevant 
      ## information remove it
      second_page_of_transaction <- df_pdf$page_id[grepl("seite-nr\\.", 
                                                         df_pdf$text)]
      if (length(second_page_of_transaction) > 0) {
        if (max(second_page_of_transaction) > 1) {
          for (i in 1:length(second_page_of_transaction)) { 
            df_pdf <- df_pdf[df_pdf$page_id != second_page_of_transaction[i], ] 
          }
        }
      }

      ## Get transaction from first page
      df_transactions <- get_onvista_transaction(df_pdf_page)

    } else if (depot_bank == dkb) {

      df_transactions <- get_dkb_transaction(df_pdf_page)

    } else if (depot_bank == cortalconsors) {

      ## If second page of a transaction exists with no relevant 
      ## information remove it
      second_page_of_transaction <- df_pdf$page_id[grepl(
        "seite 2 zu|hinweis f.?r zins- und dividendengutschriften", 
        df_pdf$text)]
      if (length(second_page_of_transaction)) {

        if (max(second_page_of_transaction) > 1) {
          for (i in 1:length(second_page_of_transaction)) { 
            df_pdf <- df_pdf[df_pdf$page_id != second_page_of_transaction[i], ] 
          }
        }

      }

      df_transactions <- get_cortalconsors_transaction(df_pdf_page)

    } else if (depot_bank == scalablecapital) {

      ## If second page of a transaction exists with no relevant 
      ## information remove it
      second_page_of_transaction <- df_pdf$page_id[grepl("seite 2/2", 
                                                         df_pdf$text)]
      if (length(second_page_of_transaction)) {

        if (max(second_page_of_transaction) > 1) {
          for (i in 1:length(second_page_of_transaction)) { 
            df_pdf <- df_pdf[df_pdf$page_id != second_page_of_transaction[i], ] 
          }
        }

      }

      df_transactions <- get_scalablecapital_transaction(df_pdf_page)

    }


    df_transactions$document_page <- first_page

    document_size <- length(unique(df_pdf$page_id))

    ## Identify how many transactions, i.e., how many pages in PDF
    document_pages <- unique(df_pdf$page_id)[unique(df_pdf$page_id) != first_page]


    if (document_size > 1) {

      for (document_page in document_pages) {

        ## Select page from entire pdf
        df_pdf_page <- df_pdf[df_pdf$page_id == document_page, ]

        ## IDENTIFY ACTUAL TRANSACTION

        if (depot_bank == onvista)
          df_transaction_temp <- get_onvista_transaction(df_pdf_page)

        df_transaction_temp$document_page <- document_page

        df_transactions <- rbind(df_transactions, df_transaction_temp)

      }

    }

    df_transactions$document_name <- document_name

    df_transactions$transaction_date <- gsub(
      "\\.", "-", df_transactions$transaction_date)

    return(df_transactions) 
    
  },

  error = function(cond) {

    message("Error: Transaction cannot be extracted.")
    message("Original message:")
    message(cond)

  })

}
