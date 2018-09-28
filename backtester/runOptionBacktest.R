runOptionBacktest <- function(opt.ds, 
                              dIVtoMA, 
                              nConeQuadrant, 
                              use.futures, 
                              opt.delta, 
                              trade.len, 
                              product,
                              chart.results=FALSE){
  
    # A few assumptions to start: 1) selling options, 2) using ivTOma
    # In addition, may adjust to sell both calls and puts if use.futures is TRUE,
    # otherwise letting opt.delta determine which option to sell.
  
    # Where does iv need to be relative its ma?
    filtered = opt.ds[!is.na(opt.ds$ivTOma) & opt.ds$ivTOma < dIVtoMA, ];
    
    # What cone?
    if(nConeQuadrant < 0 || nConeQuadrant > 4){
        message(sprintf("'%d' is an invalid for nConeQuadrant. Use an integer from 1 to 4.", nConeQuadrant));
        return(0);
    }
    
    # Relying on knowing where vol cone data reside
    cone.cols = 6:9;
    filtered  = filtered[filtered$iv >= filtered[,cone.cols[nConeQuadrant]], ];
    
    if(use.futures == TRUE){
      
        call.delta = abs(opt.delta);
        put.delta  = -call.delta;
        
        nCols         = ncol(filtered);
        call.filtered = filtered[filtered[,(nCols-1)] <  filtered[,nCols],];
        put.filtered  = filtered[filtered[,(nCols-1)] >= filtered[,nCols],];
        
        # Process to get trades
        call.processed= processSignals(opt.ds$t, call.filtered, is.buy=FALSE, opt.delta=call.delta, hold.period=trade.len, product=product);
        put.processed = processSignals(opt.ds$t, put.filtered, is.buy=FALSE, opt.delta=put.delta, hold.period=trade.len, product=product);
        
        # Assemble output obj
        call.pl       = calcOptionPLFromTable(call.processed)
        put.pl        = calcOptionPLFromTable(put.processed);
        out.obj       = list(trades=rbind(call.pl$trades, put.pl$trades));
        out.obj$total = cbind(call.pl$total, put.pl$total);
        out.obj$total[is.na(out.obj$total)] = 0;
        colnames(out.obj$total) = c("call","put");
        
        # Plot
        if(chart.results == TRUE){
            plotOptionTradeResults(out.obj);
        }
        return(out.obj);
        
    } else {
      
        # Process and calc pl
        trd.processed = processSignals(opt.ds$t, filtered, is.buy=FALSE, opt.delta=opt.delta, hold.period=trade.len, product=product);
        pl            = calcOptionPLFromTable(trd.processed);
        
        # Plot pl and trades
        if(chart.results == TRUE){
            plotOptionTradeResults(pl);
        }
        return(pl);
    }
}