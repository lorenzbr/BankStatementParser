# BankStatementParser

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable-1)
[![R-CMD-check](https://github.com/lorenzbr/BankStatementParser/workflows/R-CMD-check/badge.svg)](https://github.com/lorenzbr/BankStatementParser/actions)
[![Codecov test coverage](https://codecov.io/gh/lorenzbr/BankStatementParser/branch/main/graph/badge.svg)](https://codecov.io/gh/lorenzbr/BankStatementParser?branch=main)
<!-- badges: end -->

### Parse PDF bank statements to get financial transactions of stocks and funds



## Description

This R package contains functions to parse PDF bank statements to get financial transactions of stocks and funds. Several banks located in Germany are included (see below for further information).


## Installation

```R
devtools::install_github("lorenzbr/BankStatementParser")
```

## Usage

```R
## Example

files <- list.files(system.file("extdata", package = "BankStatementParser"), 
                    pattern = "pdf", full.names = TRUE)
document_names <- basename(files)

i <- 1
file <- files[i]
document_name <- document_names[i]
df_transactions <- BankStatementParser::get_transactions(file, document_name)

list_dfs <- mapply(BankStatementParser::get_transactions, files, 
                   document_names, SIMPLIFY = FALSE)
df_transactions <- do.call(rbind, unname(list_dfs))

path_raw_data <- ""
file_transactions_csv <- "transaction_fullhistory.csv"

BankStatementParser::update_transaction_history(df_transactions, path_raw_data, 
                                                file_transactions_csv)

df_test <- data.table::fread(paste0(path_raw_data, file_transactions_csv))
```


## Documentation

Banks and transaction types supported:

| Bank/broker            | Transaction type      					|
|------------------------|--------------------------------------------------------------|
| Cortal Consors         | Purchase, sale, partial sale, dividend	     		|
| DKB AG                 | Purchase, dividend     					|
| onVista                | Purchase, sale, dividend, storno (dividend), tax prepayment	|
| Scalable Capital	 | Purchase							|


## Contact

Please contact <lorenz.brachtendorf@gmx.de> if you want to:
* contribute to this project
* have additional features included (e.g., other banks or brokers)

You can also submit bug reports and suggestions via e-mail or <https://github.com/lorenzbr/BankStatementParser/issues> 


## License

This R package is licensed under the GNU General Public License v3.0.

See [here](https://github.com/lorenzbr/BankStatementParser/blob/main/LICENSE) for further information.
