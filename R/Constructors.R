#**********************************************************************************************
# 2. Constructors ====
setGeneric("OriginPeriod", function(StartDate, EndDate, Period, ...) {
  standardGeneric("OriginPeriod")
})

#' @export
#' 
setMethod("OriginPeriod", signature=c(StartDate="ANY", EndDate="ANY", Period="ANY")
          , definition = function(StartDate, EndDate, Period, Moniker, Type, Verbose=FALSE, NumPeriods, StartMonth, StartDay){
            
            if (missing(StartDate)){
              stop("StartDate must be specified")
            }
            
            isPOSIX = intersect(class(StartDate) , c("POSIXct", "POSIXt"))
            if (length(isPOSIX) != 0) {
              StartDate = as.Date(StartDate)
              if (Verbose) message("StartDate was given as a POSIX date/time value. It has been converted to class 'Date'.")
            }
            
            if (class(StartDate) == "numeric"){
              StartDate = as.integer(StartDate)
              if (Verbose) message("StartDate has been converted to an integer. Check that your results are as you expect.")
            }
            
            if (class(StartDate) == "integer") {
              if(missing(StartMonth)) {StartMonth = 1}
              if(missing(StartDay)) {StartDay = 1}
              
              StartDate = as.Date(paste(StartDate, StartMonth, StartDay, sep="-"))
            }
            
            if (missing(EndDate)){
              if (length(StartDate) != 1){
                NumPeriods = length(StartDate)
              } else {
                if (missing(NumPeriods)) NumPeriods = 1
              }
              if(missing(Period)) Period = DefaultPeriod()
            } else {
              if (length(StartDate) != length(EndDate)) {
                errMsg = "StartDate and EndDate must be of equal length."
                errMsg = paste0(errMsg, "StartDate has length ", length(StartDate), ".")
                errMsg = paste0(errMsg, "EndDate has length ", length(EndDate), ".")
                stop(errMsg)
              }
              
              if (length(StartDate) == 1){
                if (missing(Period)) Period = DefaultPeriod()
                NumPeriods = PeriodsBetween(StartDate, EndDate, Period)
              } else {
                if (missing(Period)) Period = InferPeriod(StartDate, EndDate)
                NumPeriods = length(StartDate)
              }
            }
            
            StartDate = StartDate[1] + Period * (0:(NumPeriods-1))
            EndDate = StartDate + Period - days(1)
            
            #             if (missing(Period)) {
            #               if (missing(EndDate)){
            #                 # Missing Period and EndDate. We'll construct using the default period.
            #                 Period = DefaultPeriod()
            #                 if (Verbose) warning("Period will default to one year.")
            #               } else {
            #                 # Period is missing, but we have EndDate. We can infer the period based on the months
            #                 # between StartDate and EndDate. To do this, we pass in a modified EndDate, which has the 
            #                 # same year as StartDate
            #                 adjustedEndDate = EndDate
            #                 year(adjustedEndDate) = year(StartDate)
            #                 Period = InferPeriod(StartDate, adjustedEndDate)
            #               }
            #             }
            #             
            #             if (class(StartDate) == "Date") {
            #                 if (missing(EndDate)) {
            #                   if (length(StartDate) == 1 & !missing(NumPeriods)){
            #                     StartDate = StartDate + Period * (0:(NumPeriods-1))
            #                   }
            #                 } else {
            #                   # StartDate, EndDate and Period are all here. This means one of two things:
            #                   #     1. The user wants a single element from StartDate and EndDate to signify the first and 
            #                   #        last dates which have a common Period between them.
            #                   #     2. Otherwise, it means that the user has passed in a superfluous Period argument.
            #                   if (length(StartDate) == 1){
            #                     # Estimate the number of elements between StartDate and EndDate
            #                     # Very lazy way of working with periods
            #                     
            #                     
            #                     StartDate = StartDate + Period * (0:(periodsBetween-1))
            #                     
            #                     if (StartDate[periodsBetween] < EndDate) StartDate[periodsBetween+1] = StartDate[periodsBetween]+Period
            #                     
            #                   } else {
            #                     if (Verbose) warning("Calling standard constructor with StartDate and EndDate vectors. Period will be ignored.")
            #                     Period = InferPeriod(StartDate, EndDate)
            #                   }
            #               } # class(StartPeriod == "Date")
            #             }
            
            if (missing(Moniker)) {
              Moniker = DefaultMoniker(StartDate)
              if (Verbose) warning("No Moniker has been specified. Defaulting to a blank Moniker.")
            }
            
            if (missing(Type)) {Type = character()}
            
            op = new("OriginPeriod"
                     , StartDate = StartDate
                     , EndDate = EndDate
                     , Period = Period
                     , Moniker = Moniker
                     , Type = Type)
            op
          })

# setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Date", Period = "Period")
#           , function(StartDate, EndDate, Period, Type, Moniker){

# setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "Date", Period = "missing")
#           , definition=function(StartDate, EndDate, Moniker, Type, Verbose=FALSE){

# setMethod("OriginPeriod", signature=c(StartDate = "Date", EndDate = "missing", Period = "Period")
#           , definition=function(StartDate, Period, Moniker, Type, Verbose=FALSE, NumPeriods){
#             
# setMethod("OriginPeriod", signature=c(StartDate = "integer", EndDate = "missing", Period = "missing")
#           , definition=function(StartDate, Moniker, Type, ){

