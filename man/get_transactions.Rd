% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_transactions.R
\name{get_transactions}
\alias{get_transactions}
\title{Get transactions from bank statements
Parse all PDF bank statements to get transaction data}
\usage{
get_transactions(file, document_name, all_pages = TRUE, 
                        depot_bank = NA)
}
\arguments{
\item{file}{A single character string. The full path and file name
of the PDF.}

\item{document_name}{A single character string. The name of the PDF.}

\item{all_pages}{Logical (default is TRUE) which indicates whether to
read every page in PDF.}

\item{depot_bank}{A single character string. Is optional (default is NA).}
}
\value{
\emph{get_transactions} returns a data frame with the
following columns:
\itemize{
\item{isin}{ The identifier ISIN}
\item{wkn}{ The identifier WKN}
\item{name}{ Name of the investment}
\item{quantity}{ Quantity/number of stocks.}
\item{transaction_price}{ Price of the transaction.}
\item{transaction_value}{ Value of the transaction (includes fees).}
\item{transaction_fee}{ Fee of the transaction. Adds up all fees.}
\item{transaction_date}{ Date of the tranaction.}
\item{transaction_time}{ Time of the transaction.}
\item{transactiontype}{ Type of the transaction (e.g., purchase, sale).}
\item{document_page}{ Page of the transaction in the PDF document.}
\item{document_name}{ Name of the PDF document.}
}
}
\description{
Get transactions from bank statements
Parse all PDF bank statements to get transaction data
}
