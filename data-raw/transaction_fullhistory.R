files <- list.files(system.file("extdata", package = "BankStatementParser"), pattern = "pdf", full.names = TRUE)
document.names <- basename(files)

list.dfs <- mapply(get_transactions, files, document.names, SIMPLIFY = FALSE)
df.transactions <- do.call(rbind, unname(list.dfs))

path.raw.data <- "inst/extdata/"
file.transactions.csv <- "transaction_fullhistory.csv"

update_transaction_history(df.transactions, path.raw.data, file.transactions.csv)
