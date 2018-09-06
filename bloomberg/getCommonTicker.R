getCommonTicker <- function(sym){
  
    # Used when converting from bloomberg to local system. If, during the process of conversion,
    # the system changes bloombergs naming, that new name is mapped to the old in 'ticker.key'.
    # This table is used for other setup function to get the correct symbol further down the line.
  
    if(!exists("ticker.key")){
        return(sym);
    }
  
    converted.ticker = ticker.key[ticker.key[,'original'] == sym, 'converted'];
    if(length(converted.ticker) >  0){
        return(converted.ticker);
    }
    
    return(sym);
}