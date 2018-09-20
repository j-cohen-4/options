calcOptionPLFromTable <- function(trade.table){
  
    # This function takes the output from process signals and calculates returns
    trade.signals = which(trade.table$action != 0);
    
    # Should be even numbered, an exit for every entry
    if(length(trade.signals) %% 2 != 0){
        stop("calcOptionPLFromTable: Invalid trade.table...incorrect number of rows!");
    }
    
    pl.table = data.frame(date=as.Date(character()), pl=numeric(), high=numeric(), low=numeric());
    nTrades  = length(trade.signals) / 2;
    for(i in 1:nTrades){
      
        nEntryIdx = (i * 2) - 1;
        nExitIdx  = nEntryIdx + 1;
        nEntry    = trade.signals[nEntryIdx];
        nExit     = trade.signals[nExitIdx];

        # Calc trade pl
        tmp.pl    = optionTablePL(trade.table[nEntry:nExit,]);
        
        # Process results
        trade               = cumprod(tmp.pl+1)-1;
        pl.table[i, "date"] = index(tmp.pl[1]);
        pl.table[i, "pl"]   = as.numeric(tail(trade, 1));
        pl.table[i, "high"] = max(trade);
        pl.table[i, "low"]  = min(trade);
        
        # Accumulate
        if(i == 1){
            total.pl = tmp.pl;
        } else {
            total.pl = rbind(total.pl, tmp.pl);
        }
    }
    
    return(list(total=total.pl, trades=pl.table));
}