#' OriginPeriod class
#' 
#' @include HelperFunctions.R
#' @include NewGenerics.R
#' 
#' @docType class
#' 
#' 
#' @name OriginPeriod-class
#' @rdname OriginPeriod-class
#' @exportClass OriginPeriod
#' 
#' @description
#' OriginPeriod is an S4 class used to store information about the time 
#' interval which produces claims. 
#' 
#' @details
#' An OriginPeriod may be of any arbitrary length, though years are most 
#' common. The "type" slot is a character string which indicates the type 
#' of origin period, such as "Accident", "Occurrence", "Report", "Policy", 
#' "Lloyds" and so forth. "Accident" and "Occurrence" are synonymous and 
#' indicate the a claim occurred in a particular period of time. "Report" 
#' is the interval during which a claim is reported, such as exists under 
#' a claims-made policy. "Policy" or "Underwriting" periods are ones wherein 
#' the risk attaches. "Lloyds" refers to a Lloyds year of account. There is 
#' no restriction on what type may be used for an OriginPeriod, meaning that 
#' there is no language restriction either. So, "Zeichnungsjahr", 
#' "Ereignisjahr" and so forth are permitted.
#' 
#' \strong{OriginPeriod construction}
#' \strong{1. Construct an empty OriginPeriod}
#' myOP = OriginPeriod()
#' 
NULL
