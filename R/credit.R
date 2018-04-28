credit <- function(input){
  #input can either be csv file or data	
  newdata <- if(is.character(input) && file.exists(input)){
  	read.csv(input)
  } else {
  	as.data.frame(input)
  }
  stopifnot("Duration.in.month" %in% names(newdata))
  stopifnot("Credit.amount" %in% names(newdata))
  
  newdata$Duration.in.month <- as.numeric(newdata$Duration.in.month)
  newdata$Credit.amount <- as.numeric(newdata$Credit.amount)
  newdata$Installment.rate.in.percentage.of.disposable.income <- as.numeric(newdata$Installment.rate.in.percentage.of.disposable.income)
  newdata$Status.of.existing.checking.accountA13 <- as.numeric(newdata$Status.of.existing.checking.accountA13)
  newdata$Status.of.existing.checking.accountA14 <- as.numeric(newdata$Status.of.existing.checking.accountA14)
  newdata$Credit.historyA32 <- as.numeric(newdata$Credit.historyA32)
  newdata$Credit.historyA33 <- as.numeric(newdata$Credit.historyA33)
  newdata$Credit.historyA34 <- as.numeric(newdata$Credit.historyA34)

  newdata$PurposeA41 <- as.numeric(newdata$PurposeA41)
  newdata$Savings.account.bondsA64 <- as.numeric(newdata$Savings.account.bondsA64)
  newdata$Savings.account.bondsA65 <- as.numeric(newdata$Savings.account.bondsA65)
  newdata$Present.employment.since.A74 <- as.numeric(newdata$Present.employment.since.A74)
  newdata$Other.debtors...guarantorsA103 <- as.numeric(newdata$Other.debtors...guarantorsA103)
  newdata$Other.installment.plansA143  <- as.numeric(newdata$Other.installment.plansA143 )
  newdata$Housing.A15 <- as.numeric(newdata$Housing.A15)
  newdata$Housing.A152 <- as.numeric(newdata$Housing.A152)
  
  newdata$credit_score <- ""
  
  #model_9 is included with the package
  newdata$credit_score <-  predict(model_9, newdata = newdata, type="response")
  return(newdata)
}