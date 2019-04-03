#Portfolio optimizer
library(quantmod);library('ustyc');library("PerformanceAnalytics")
PortfolioStats <- function(Tickers,Weights = "EQUAL",From = "MAX",To = as.Date(Sys.time()),RiskfreeRate12month=2,Benchmark = "^GSPC", PrintVarCovar = TRUE, PrintBetas = TRUE){
  if (Weights[1] == "EQUAL") {
    Weights <- rep(1/length(Tickers),each=length(Tickers))
  }
  #errorcatcher
  if (length(Tickers)!=length(Weights)) {
    stop("Unequal number of tickers and weights")
  }  
  
  #Get startdate from tracebackperiod
  if (!class(From)=="Date") {
    From <- as.Date("1990-01-01")
  } else {
    From <- as.Date(Sys.time())-as.numeric(as.Date(Sys.time())-From)
  }
  #Download benchmark
  BenchIndex <- na.fill(getSymbols(Benchmark,auto.assign = FALSE, from = From, to = To)[,4],fill = NULL)
  #Initiate DF for stock returns
  Returns <- BenchIndex
  #Download data and store in DF
  for (Ticker in Tickers){
    Stock <- na.fill(getSymbols(Ticker,auto.assign = FALSE),fill = NULL)
    Stock <- Stock[which(is.na(Stock[,4])==FALSE),]
    CommonDates <- as.Date(intersect(time(Returns),time(Stock)))
    Returns <- cbind(Returns[CommonDates,],Stock[CommonDates,4])
  }
    Since <- time(head(Returns,1))
    To <- time(tail(Returns,1))
    #Save payoff from each stock
    ER <- (t(matrix(tail(Returns,1))) / as.vector(head(Returns,1))) -1
    colnames(ER) <- c(Benchmark,Tickers)
    BenchmarkReturn <- ER[1]
    #Recalculate in to relative returns
    for (N in 1:ncol(Returns)) {
    Returns[,N] <- diff(log(Returns[,N]))
  }
  colnames(Returns) <- c(Benchmark,Tickers)
  #Make variance covariance matrix
  MyVarCovar <- cov(Returns[-1,])
  #If wanted, print matrix
  if (PrintVarCovar == TRUE) {
    cat("\n",paste("Variance-Covariance Matrix of stocks:"),
        "\n",sep = "")
    print(round(MyVarCovar,5))
  }
  #Calculate Betas
  Betas <- MyVarCovar[1,]/MyVarCovar[1]
  #If wanted print the betas
  if (PrintBetas == TRUE) {
  cat("\n",
      paste("Beta of stocks to benchmark index:"),
      "\n",sep = "")
  print(Betas)
  }
  #Remove index from the matrix
  MyVarCovar <- MyVarCovar[-1,-1]
  #Calc variance and return of portfolio
  PortfolioSD <- sqrt(t(MyVarCovar %*% matrix(Weights)) %*% as.matrix(Weights))
  PortfolioReturn <- ER[-1]%*%Weights
  #Annualize return
  PortfolioReturn <- (PortfolioReturn+1)^(1/(as.numeric(as.Date(Sys.time())-Since)/365))-1
  
  cat("\n",
    "Portfolio contains:", "\n",
    paste(paste(round(Weights*100,digits = 2),"%",sep = ""),Tickers, "  "), "\n",
    paste("Annualized return:",round(PortfolioReturn,digits = 3)*100 ,"%"), "\n",
    paste("Standard deviation:",round(PortfolioSD,digits = 4)*100, "%", "(Annualized: ",round(PortfolioSD*sqrt(252),digits = 4)*100),"%)", "\n",
    paste("Sharpe ratio (with riskfree rate",RiskfreeRate12month,"%):", round((PortfolioReturn-(RiskfreeRate12month)/100)/PortfolioSD,digits = 2)), "\n",
    paste("Traced from:",Since, "to", To, ""), "\n","\n",
    sep = ""
  )

}
#Use here
PortfolioStats(Tickers = c("PSM.DE"),
               Weights = ,#If unassigned, assumes equal weights
               From = as.Date("2017-03-01"), #use format: as.Date("2019-01-01")
               To = , #default is today
               RiskfreeRate12month = 2, #Insert one % as 1
               Benchmark = c("^MDAXI"), #If unassigned, assumes SP500
               PrintVarCovar = TRUE,
               PrintBetas = TRUE
)
