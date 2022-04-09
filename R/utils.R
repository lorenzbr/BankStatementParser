#' Read PDF document modified from R package textreadr
#'
#' @usage read_pdf(file, skip = 0, remove.empty = TRUE, 
#'                 trim = TRUE, ocr = TRUE, ...)
#' @param file A path to a PDF file.
#' @param skip Integer; the number of lines of the data file to skip before 
#' beginning to read data.
#' @param remove.empty logical
#' @param trim logical
#' @param ocr logical
#' @param ... Other arguments to be passed to the function
#' 
#' @return A data frame containing current portfolio.
#'
#' @noRd
read_pdf <- function(file, skip = 0, remove.empty = TRUE, 
                     trim = TRUE, ocr = TRUE, ...) {
  
  ## use pdftools to read in the document
  text <- pdftools::pdf_text(file, ...)
  
  ## Use ocr if pdf is raster not text
  if (all(text %in% '') && isTRUE(ocr)) {
    
    if (requireNamespace("tesseract", quietly = TRUE)) {
      
      temp <- tempdir()
      fls <- file.path(
        temp,
        paste0(gsub('\\.pdf$', '', base_name(file)), '_', 
               pad_left(seq_along(text)), '.png')
      )
      
      ## Convert to png files for tesseract to interact with
      png_files <- pdftools::pdf_convert(file, dpi = 600, filenames = fls)
      
      ## OCR
      text <- tesseract::ocr(png_files)
      
      ## Clean up and remove the png files
      unlink(png_files, TRUE, TRUE)
      
    } else {
      
      warning('\'tesseract\' not available. `ocr = TRUE` ignored.\n\nPlease use `install.packages(\'tesseract\')` and then retry.', call. = FALSE)
      
    }
    
    split <- "\\r\\n|\\n"
    
  } else {
    
    split <- "\\n"
    
  }
  
  ## Convert to UTF-8 encoding and split on carriage returns
  Encoding(text) <- "UTF-8"
  text <- strsplit(text, split)
  
  ## Formatting 1
  if (isTRUE(remove.empty)) 
    text <- lapply(text, function(x) x[!grepl("^\\s*$", x)])
  
  ## Coerce to a data frame with page numbers
  out <- data.frame(page_id = rep(seq_along(text), sapply(text, length)), 
                    element_id = unlist(sapply(text, 
                                               function(x) seq_len(length(x)))),
                    text = unlist(text), stringsAsFactors = FALSE)
  
  ## Formatting 2
  if (skip > 0) out <- utils::tail(out, -c(skip))
  if (isTRUE(trim)) out[['text']] <- trimws(out[['text']])
  
  class(out) <- c("textreadr", "data.frame")
  out
}

pad_left <- function(x, len = 1 + max(nchar(x)), char = '0') {
  
  unlist(lapply(x, function(x) {
    paste0(
      paste(rep(char, len - nchar(x)), collapse = ''),
      x
    )
  }))
  
}

#' Utilities for Looping to Read In Documents Copy paste from textreadr
#' 
#' `base_name` - Like `base::basename` but doesn't choke on long paths.
#' 
#' @param path A character vector, containing path names.
#' @return `base_name` - Returns just the basename of the path.
#' @noRd
base_name <- function(path) gsub('^.+/', '', path)