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
    
    # Filtered dataset needs at least 1 row
    if(nrow(filtered) <= 0){
        message("runOptionBacktest: Unable to run because filters return empty dataset!");
        return(NULL);
    }
    
    if(use.futures == TRUE){
      
        call.delta = abs(opt.delta);
        put.delta  = -call.delta;
        out.obj    = NULL;
        
        # Further filtering, start with call
        nCols         = ncol(filtered);
        call.pl       = NULL;
        call.filtered = filtered[filtered[,(nCols-1)] <  filtered[,nCols],];
        if(nrow(call.filtered) > 0){
            call.processed = processSignals(opt.ds$t, call.filtered, is.buy=FALSE, opt.delta=call.delta, hold.period=trade.len, product=product);
            call.pl        = calcOptionPLFromTable(call.processed);
            out.obj        = list(trades=call.pl$trades);
            out.obj$total  = call.pl$total;
            colnames(out.obj$total) = "call";
        }
        
        # ...then put
        put.pl        = NULL;
        put.filtered  = filtered[filtered[,(nCols-1)] >= filtered[,nCols],];
        if(nrow(put.filtered) > 0){
            put.processed = processSignals(opt.ds$t, put.filtered, is.buy=FALSE, opt.delta=put.delta, hold.period=trade.len, product=product);
            put.pl        = calcOptionPLFromTable(put.processed);
            
            if(is.null(out.obj)){
                out.obj       = list(trades=put.pl$trades);
                out.obj$total = put.pl$total;
                colnames(out.obj$total) = "put";
            } else {
                out.obj$trades= rbind(out.obj$trades, put.pl$trades);
                out.obj$total = cbind(out.obj$total, put.pl$total);
                out.obj$total[is.na(out.obj$total)] = 0;
                colnames(out.obj$total) = c("call","put");
            }
        }
        
        # Plot
        if(!is.null(out.obj) && chart.results == TRUE){
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