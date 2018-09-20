option.pl.r <- function(trade.table){
  
    # For output
    daily.r = rep(0, nrow(trade.table));
    
    # Determine action for option and future
    opt.action = trade.table$action[1];
    fut.action = opt.action * -1;
    if(trade.table$type[1] == "P"){
        fut.action = opt.action;
    }
    
    # Iterate through trade
    base   = 0;
    commit = 0;
    nRows    = nrow(trade.table);
    for(i in 1:nRows){
      
        if(i == 1){
            base   = trade.table$price[i] + trade.table$futures[i] * abs(trade.table$delta[i]);
        } else {
          
            # PL each trade part -- by return
            opt.pl = (trade.table$price[i] - trade.table$price[i-1]) / trade.table$price[i-1] * opt.action;
            fut.pl = (trade.table$futures[i] - trade.table$futures[i-1]) / trade.table$futures[i-1] * fut.action;
            
            # Calculate weights
            commit   = trade.table$price[i-1] + trade.table$futures[i-1] * abs(trade.table$delta[i-1]);
            opt.wt   = trade.table$price[i-1] / commit;
            fut.wt   = 1 - opt.wt;
            
            # Adjust by base change
            opt.wt   = opt.wt * (commit / base);
            fut.wt   = fut.wt * (commit / base);
            
            # Now calc return
            r        = opt.pl * opt.wt + fut.pl * fut.wt;
            base     = base * (1 + r);
            daily.r[i] = r;
        }
    }
    
    # Convert to xts obj
    return(xts(daily.r, order.by=trade.table$date));
}

optionTablePL <- function(opt.table){
  
    nRows   = nrow(opt.table);
    capital = opt.table$price[1] + abs(opt.table$delta[1]) * opt.table$futures[1];
    tmp.cap = capital;
    
    opt.action = opt.table$action[1];
    fut.factor = ifelse(opt.table$type[1] == "P", -opt.action, opt.action);
    daily.r    = rep(0, nRows);
    
    for(i in 2:nRows){
      
        # PL options
        pl      = (opt.table$price[i] - opt.table$price[i-1]) * opt.action +
                     (opt.table$futures[i] - opt.table$futures[i-1]) * opt.table$delta[i-1] * fut.factor;
        pl.ret     = pl / tmp.cap;
        tmp.cap    = tmp.cap * (1 + pl.ret);
        daily.r[i] = pl.ret;
    }
    return(xts(daily.r, order.by=opt.table$date));
}