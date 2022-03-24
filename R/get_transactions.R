#' Get transactions from bank statements
#
#' Parse all PDF bank statements to get transaction data
#'
#' @usage get_transactions(file, document.name, all.pages = TRUE, depot.bank = NA)
#' @param file A single character string. The full path and file name of the PDF.
#' @param document.name A single character string. The name of the PDF.
#' @param all.pages Logical (default is TRUE) which indicates whether to read every page in PDF.
#' @param depot.bank A single character string. Is optional (default is NA).
#' @return \emph{get_transactions} returns a data.frame with the following columns:
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
get_transactions <- function(file, document.name, all.pages = TRUE, depot.bank = NA) {

  tryCatch({

    document.name.csv <- paste0(gsub(".pdf$", "", document.name), ".csv")

    df.pdf <- read_pdf(file)

    df.pdf$text_original <- df.pdf$text

    df.pdf$text <- tolower(df.pdf$text)

    if (is.na(depot.bank)) {

      ## Define strings for all available depot banks / brokers
      onvista <- "OnVista"
      dkb <- "DKB AG"
      cortalconsors <- "Cortal Consors"
      scalablecapital <- "Scalable Capital"

      #### IDENTIFY DEPOT BANK
      if (any(grepl("cortal consors", df.pdf$text))) { 
        
        depot.bank <- cortalconsors

      } else if (any(grepl("scalable capital", df.pdf$text))) {
        
        depot.bank <- scalablecapital
        
      } else if (any(grepl("onvista bank", df.pdf$text))
                 || any(grepl("dieser beleg wird maschinell erstellt", df.pdf$text))) {
        
        depot.bank <- onvista
        
      } else if (any(grepl("10919 berlin", df.pdf$text))) {
          
          depot.bank <- dkb
        
      }

    }
    
    if (is.na(depot.bank)) {
      
      ## Need to apply OCR because machine written PDF sometimes does not store the name of the bank
      ## THIS IS VERY SLOW!!!
      temp <- tempdir()
      fls <- file.path(temp, paste0(gsub("\\.pdf$", "", document.name), ".png"))
      png_files <- pdftools::pdf_convert(file, pages = 1, dpi = 150, filenames = fls)
      text <- tesseract::ocr(png_files)
      text <- tolower(text)
      unlink(png_files, TRUE, TRUE)
      
      if (grepl("onvista", text)) {
        
        depot.bank <- onvista
        
      } else if (grepl("deutsche kreditbank", text)) {
        
        depot.bank <- dkb
        
      } else { 
        
        stop("PDF not issued by onVista, DKB or Cortal Consors. Other depot banks or brokers are not included.") 
        
      }
      
    }
    
    

    ## First page
    first.page <- 1
    df.pdf.page <- df.pdf[df.pdf$page_id == first.page, ]

    ## Get transaction from first page if bank is from a known/identified bank
    if (depot.bank == onvista) {
      
      ## For onVista, sometimes several pages with transactions exist
      ## If second page of a transaction exists with no relevant information remove it
      second.page.of.transaction <- df.pdf$page_id[grepl("seite-nr\\.", df.pdf$text)]
      if (length(second.page.of.transaction) > 0) {
        if (max(second.page.of.transaction) > 1) {
          for (i in 1:length(second.page.of.transaction)) { 
            df.pdf <- df.pdf[df.pdf$page_id != second.page.of.transaction[i], ] 
          }
        }
      }

      ## Get transaction from first page
      df.transactions <- get_onvista_transaction(df.pdf.page)

    } else if (depot.bank == dkb) {

      df.transactions <- get_dkb_transaction(df.pdf.page)

    } else if (depot.bank == cortalconsors) {

      ## If second page of a transaction exists with no relevant information remove it
      second.page.of.transaction <- df.pdf$page_id[grepl("seite 2 zu|hinweis f.?r zins- und dividendengutschriften",
                                                         df.pdf$text)]
      if (length(second.page.of.transaction)) {

        if (max(second.page.of.transaction) > 1) {
          for (i in 1:length(second.page.of.transaction)) { 
            df.pdf <- df.pdf[df.pdf$page_id != second.page.of.transaction[i], ] 
          }
        }

      }

      df.transactions <- get_cortalconsors_transaction(df.pdf.page)

    } else if (depot.bank == scalablecapital) {

      ## If second page of a transaction exists with no relevant information remove it
      second.page.of.transaction <- df.pdf$page_id[grepl("seite 2/2", df.pdf$text)]
      if (length(second.page.of.transaction)) {

        if (max(second.page.of.transaction) > 1) {
          for (i in 1:length(second.page.of.transaction)) { 
            df.pdf <- df.pdf[df.pdf$page_id != second.page.of.transaction[i], ] 
          }
        }

      }

      df.transactions <- get_scalablecapital_transaction(df.pdf.page)

    } ## End if statement onVista, DKB, Scalable Capital or Cortal Consors transaction


    ## Add column with first page
    df.transactions$document_page <- first.page

    document.size <- length(unique(df.pdf$page_id))

    ## Identify how many transactions, i.e., how many pages in PDF
    document.pages <- unique(df.pdf$page_id)[unique(df.pdf$page_id) != first.page]


    if (document.size > 1) {

      ## For loop over all transaction in PDF document
      for (document.page in document.pages) {

        ## Select page from entire pdf
        df.pdf.page <- df.pdf[df.pdf$page_id == document.page, ]


        ## IDENTIFY ACTUAL TRANSACTION

        ## If depot bank is onVista
        if (depot.bank == onvista) 
          df.transaction.temp <- get_onvista_transaction(df.pdf.page)

        ## Add document page
        df.transaction.temp$document_page <- document.page

        ## Combine entire output in data frame
        df.transactions <- rbind(df.transactions, df.transaction.temp)

      } ## End of for loop over all transactions/pages in pdf

    } ## End if condition: if more than 1 page

    ## Add document name to data frame
    df.transactions$document_name <- document.name

    ## Change date style
    df.transactions$transaction_date <- gsub("\\.", "-", df.transactions$transaction_date)

    return(df.transactions) 
    
  },

  error = function(cond) {

    message("Error: Transaction cannot be extracted.")
    message("Original message:")
    message(cond)

  })

}
