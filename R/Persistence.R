#' @title write.excel
#' @name write.excel
#' 
#' @export
#' 
#' @param object Object to write to Excel
#' @param file Character string with the name of the file
#' @param overwrite Overwrite the file if it exists?
#' @param ... Additional arguments
#' 
#' @import XLConnect
setGeneric("write.excel", function(object, file, overwrite, ...) {
  standardGeneric("write.excel")
})


#' @export
#' @rdname write.excel
#' 
#' @import XLConnect
#' @param sheetName Name of sheet where the object will be written
setMethod("write.excel", signature=c(object = "OriginPeriod", file="character", overwrite="logical")
          , definition=function(object, file, overwrite=FALSE, sheetName){
            
            if (file.exists(file) & !overwrite){
              stop("Excel file already exists. Either enter a new filename or set the overwrite parameter to TRUE.")
            }
            
            if (missing(sheetName)) sheetName = "OriginPeriod"
            wbk = loadWorkbook(file, create=TRUE)
            createSheet(wbk, name=sheetName)
            
            headers = data.frame(Col1 = c(object@Type, "Period")
                                 , Col2 = c("Start", "Date")
                                 , Col3 = c("End", "Date"))
            
            writeWorksheet(wbk, headers, sheetName, header=FALSE)
            
            df = as.data.frame(object)
            df = df[, c("Moniker", "StartDate", "EndDate")]
            
            writeWorksheet(wbk, df, sheetName, startRow = 3, header=FALSE)
            
            saveWorkbook(wbk)
            
          })

# setMethod("read", )
# How does this work exactly?
