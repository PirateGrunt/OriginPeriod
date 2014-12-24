MonthsBetween = function(StartDate, EndDate){
  diff = as.double(EndDate - StartDate)
  diff = round(diff / 30.5)
  as.period(diff, unit="months")
}

MeanMonths = function(Periods){
  z = month(Periods)
  z = mean(z)
  z = as.period(z, unit="months")
}

InferPeriod = function(StartDate, EndDate){
  Period = MonthsBetween(StartDate, EndDate)
  Period = MeanMonths(Period)
}

PeriodsBetween = function(StartDate, EndDate, Period){
  
  if (length(StartDate) != 1 | length(EndDate) != 1) {
    stop("StartDate and EndDate must each have length == 1.")
  }
  
  daysBetween = difftime(EndDate, StartDate, units="days") + 1
  periodsBetween = suppressMessages(daysBetween / (Period / days(1)))
  periodsBetween = round(as.numeric(periodsBetween))
  
  periodsBetween
}

DefaultPeriod = function(){
  years(1)
}

DefaultMoniker = function(startDates){
  as.character(startDates)
}
