processSignals <- function(all.dates, signals, is.buy, opt.delta, hold.period, product){
  
    # Passed in is the dataset, where every row is a date that passes the trade decision process.
    # This function makes a trade on the first passing day, buys or sells the option nearest
    # the 'opt.delta', holding and exiting after 'hold.period'. The output is a data.frame that
    # is in the format date; long/short, contract, type, price, delta, futures price, fut contract
  
    nRowEntry   = 1;
    trade.table = data.frame(date=as.Date(character()), action=numeric(), contract=character(),
                             type=character(), price=numeric(), delta=numeric(), futures=numeric(),
                             fut.contract=character(), stringsAsFactors=FALSE);
    tradeDt     = NULL;
    closeDt     = NULL;
    trade.dates = all.dates[all.dates >= signals$t[1]];
    nRows       = length(trade.dates);

    for(i in 1:nRows){
        
        current.date = trade.dates[i];
        
        if(is.null(tradeDt) && current.date == signals$t[1] && i < nRows){
            # New trade
            tradeDt = signals$t[1];
            closeDt = tradeDt + hold.period;
            #signals = signals[signals$t > closeDt, ];
            message(sprintf("New trade on %s, will close on %s", tradeDt, closeDt));
            
            # Get the option to trade -- keep the constant for life of trade
            the.opt                            = findOption(tradeDt, dte=120, dDelta=opt.delta, product=product);
            if(is.null(the.opt)){
                # No trade so clear tradeDt
                tradeDt = NULL;
                next; 
            }
            trade.action                       = ifelse(is.buy, 1, -1);
            trade.contract                     = the.opt[1, 'contract'];
            trade.futures                      = the.opt[1, 'future'];
            trade.opt.type                     = the.opt[1, 'type'];
            trade.k                            = the.opt[1, 'strike'];
            
            trade.table[nRowEntry, 'date']     = tradeDt;
            trade.table[nRowEntry, 'action']   = trade.action;
            trade.table[nRowEntry, 'contract'] = trade.contract;
            trade.table[nRowEntry, 'type']     = trade.opt.type;
            trade.table[nRowEntry, 'price']    = the.opt[1, 'price'];
            trade.table[nRowEntry, 'delta']    = the.opt[1, 'delta'];
            trade.table[nRowEntry, 'futures']  = getFuturesPrice(trade.futures, dt=tradeDt);
            trade.table[nRowEntry, 'fut.contract'] =  trade.futures;
            nRowEntry = nRowEntry + 1;
            
        } else {
          
            if(is.null(tradeDt)){
                # Not in a trade so just continue
                next;
            }
          
            # Mid trade -- check relative to closeDt (or if last row, close)
            if(trade.dates[i] >= closeDt || i == nRows){
                message(sprintf("Closing trade on %s", trade.dates[i]));
                signals = signals[signals$t > trade.dates[i], ];
                tradeDt = NULL;
                closeDt = NULL;
  
                if(trade.action == 1){
                    trade.action = -1;
                } else {
                    trade.action = 1;
                }
                
            } else {
                trade.action     = 0;
                message(".", appendLF=FALSE);
            }

            # Get moving parts: today's opt price, fut price and opt delta
            new.dat = getOptionPrice(contract=trade.contract, dt=trade.dates[i], 
                                     futures.contract=trade.futures, opt.type=trade.opt.type,
                                     k=trade.k, incl.delta=TRUE);
            if(is.null(new.dat)){ next; }
               
            trade.table[nRowEntry, 'date']     = trade.dates[i];
            trade.table[nRowEntry, 'action']   = trade.action;
            trade.table[nRowEntry, 'contract'] = trade.contract;
            trade.table[nRowEntry, 'type']     = trade.opt.type;
            trade.table[nRowEntry, 'price']    = new.dat$opt.prc;
            trade.table[nRowEntry, 'delta']    = new.dat$delta;
            trade.table[nRowEntry, 'futures']  = new.dat$fut.prc;
            trade.table[nRowEntry, 'fut.contract'] =  trade.futures;
            nRowEntry = nRowEntry + 1;
            
            # If no signals left, break out of loop
            if(nrow(signals) == 0){
                break;
            }
        }
    }
    return(trade.table);
}