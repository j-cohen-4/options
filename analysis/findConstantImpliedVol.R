findConstantImpliedVol <- function(nDays, nRealizedDays, r=0.0185, product="crude", 
                                      opt.inc=1, near.days=5){
  
    g_env             = globalenv();
    prod.dte.var      = sprintf("%s.dte.table", product);
    prod.realized.var = sprintf("%s.realized.vol", product);
    
    # Sanity
    if(!exists(prod.dte.var, envir=g_env)){
        message(sprintf("findConstantImpliedVol: Unable to run because %s not loaded!", prod.dte.var));
        return(0);
    }
    prod.dte.table = get(prod.dte.var, envir=g_env);
    
    # Calc realized vol
    if(!exists(prod.realized.var, envir=g_env)){
      
        # Need futures
        prod.futures.var = sprintf("%s.futures", product);
        if(!exists(prod.futures.var, envir=g_env)){
            stop("Error! No Futures Loaded!");
        }
        prod.futures = get(prod.futures.var, envir=g_env);
        returns      = tail(diff(log(prod.futures)), -1);
        realized     = rollapply(returns, width=nRealizedDays, FUN=sd, by.column=TRUE) * sqrt(252);
        assign(prod.realized.var, realized, envir=g_env);
    } else {
        realized = get(prod.realized.var, envir=g_env);
    }
    syms     = colnames(prod.dte.table);
    nLen     = nrow(prod.dte.table) - 1;
    nDataRow = 1;
    df       = data.frame(Date=as.Date(character()), CM.IV=double(), front=character(), days1=integer(),
                          iv1=double(), back=character(), days2=integer(), iv2=double(), stringsAsFactors=FALSE);
    for(i in 1:nLen){
        
        idx1 = which(prod.dte.table[i,] <= nDays);
        if(length(idx1) == 0){ next; }

        dt     = index(prod.dte.table[i,]);
        fr.idx = tail(idx1, 1);
        dte.1  = prod.dte.table[i, fr.idx];
        if(dte.1 == nDays){
            
            # No interpolation, front contract only
            sym1 = syms[fr.idx];
            sym2 = NULL;
        } else {
          
            idx2 = which(prod.dte.table[i,] > nDays);
            if(length(idx2) == 0){
              
                if((nDays - dte.1) <= near.days){
                    # No interpolation, front contract only
                    sym1 = syms[fr.idx];
                    sym2 = NULL;
                } else { next; }
              
            } else {
                
                # Interpolate
                sym1 = syms[tail(idx1, 1)];
                sym2 = syms[idx2[1]];
            }
        }

        # Get option data for interpolation/calculation        
        atm1 = findATMCall(dt, sym1, r=r, product=product, opt.inc=opt.inc);
        if(is.null(sym2)){
          
            # Can't do anything with bad/no data
            if(is.na(atm1$s0) || is.na(atm1$cp)) { next; }

            iv   = bscallimpvol(atm1$s0, atm1$k, r, atm1$dte, 0, atm1$cp);
            iv   = list(iv=iv, iv1=iv, iv2=0);
            sym2 = "";
            atm2 = list(dte=0);

            # FIX ME
            rv   = realized[dt, sym1];

        } else {
            atm2 = findATMCall(dt, sym2, r=r, product=product, opt.inc=opt.inc);
            if(!(is.na(atm1$cp) || is.na(atm2$cp) || atm1$dte==0)){
                iv   = interpolateIV(atm1, atm2, nDays, r=r);
                # FIX ME -- RV calculation
                d1 = atm1$dte * 360;
                d2 = atm2$dte * 360;
                wgt1 = (nDays - d1) / (d2 - d1);
                wgt2 = (1 - wgt1);
                rv   = realized[dt, sym1] * wgt1 + realized[dt, sym2] * wgt2;
            } else {
                message("findConstantImpliedVol: NA found, using previous value for IV");
                iv   = list(iv=as.numeric(tail(tbl[,1], 1)), iv1=0, iv2=0);
                rv   = tail(tbl[,2], 1);
            }
        }
        
        if(!exists('tbl')){
            tbl = xts(t(c(iv$iv, rv)), order.by=dt);
        } else {
            tbl = rbind(tbl, xts(t(c(iv$iv, rv)), order.by=dt));
        }
        
        # Fill out df
        df[nDataRow, 'Date']  = dt;
        df[nDataRow, 'CM.IV'] = iv$iv;
        df[nDataRow, 'front'] = sym1;
        df[nDataRow, 'days1'] = atm1$dte * 360;
        df[nDataRow, 'iv1']   = iv$iv1;
        df[nDataRow, 'back']  = sym2;
        df[nDataRow, 'days2'] = atm2$dte * 360;
        df[nDataRow, 'iv2']   = iv$iv2;
        nDataRow = nDataRow + 1;
    }
    
    colnames(tbl) = c("cm.iv", "rv");
    return(list(xts.obj=tbl, df.dat=df));
}

# Find contracts that bracket nDays
#idx1 = which(prod.dte.table[i,] <= nDays);
#idx2 = which(prod.dte.table[i,] > nDays);
#if(length(idx1) > 0 && length(idx2) > 0){
#  
#  dt  = index(prod.dte.table[i,]);
#  
#  # 2 conditions don't need interpolation: an exact date match, dte=0 for front
#  fr.idx = tail(idx1, 1);
#  dte.1  = prod.dte.table[i, fr.idx];
#  bExact = dte.1 == nDays;
#  bZero  = dte.1 == 0 || dte.1 == -1;
#  if(bExact == TRUE || bZero == TRUE){
#    
#    if(bExact){
#      # No interpolation, use exact
#      sym1 = syms[fr.idx];
#      atm1 = findATMCall(dt, sym1, r=r, product=product, opt.inc=opt.inc);
#    } else {
#      # Front month expired, use second month regardless of days (close enough)
#      sym1 = syms[fr.idx + 1];
#      atm1 = findATMCall(dt, sym1, r=r, product=product, opt.inc=opt.inc);
#    }
#    iv   = bscallimpvol(atm1$s0, atm1$k, r, atm1$dte, 0, atm1$cp);
#    iv   = list(iv=iv, iv1=iv, iv2=0);
#    sym2 = "";
#    atm2 = list(dte=0);
#    
#    # FIX ME
#    rv   = realized[dt, sym1];
#  } else {
#    
#    # Interpolate
#    sym1 = syms[tail(idx1, 1)];
#    atm1 = findATMCall(dt, sym1, r=r, product=product, opt.inc=opt.inc);
#    
#    sym2 = syms[idx2[1]];
#    atm2 = findATMCall(dt, sym2, r=r, product=product, opt.inc=opt.inc);
#    
#    if(!(is.na(atm1$cp) || is.na(atm2$cp) || atm1$dte==0)){
#      iv   = interpolateIV(atm1, atm2, nDays, r=r);
#    } else {
#      message("findConstantImpliedVol: NA found, using previous value for IV");
#      iv   = list(iv=as.numeric(tail(tbl[,1], 1)), iv1=0, iv2=0);
#    }
#    
#    # FIX ME
#    d1 = atm1$dte * 360;
#    d2 = atm2$dte * 360;
#    wgt1 = (nDays - d1) / (d2 - d1);
#    wgt2 = (1 - wgt1);
#    rv   = realized[dt, sym1] * wgt1 + realized[dt, sym2] * wgt2;
#  }
#  
#  #rv = realized[dt, sym1];
#  if(!exists('tbl')){
#    tbl = xts(t(c(iv$iv, rv)), order.by=dt);
#  } else {
##    tbl = rbind(tbl, xts(t(c(iv$iv, rv)), order.by=dt));
#  }
  
  # Fill out df
#  df[nDataRow, 'Date']  = dt;
#  df[nDataRow, 'CM.IV'] = iv$iv;
#  df[nDataRow, 'front'] = sym1;
#  df[nDataRow, 'days1'] = atm1$dte * 360;
#  df[nDataRow, 'iv1']   = iv$iv1;
#  df[nDataRow, 'back']  = sym2;
#  df[nDataRow, 'days2'] = atm2$dte * 360;
#  df[nDataRow, 'iv2']   = iv$iv2;
#  nDataRow = nDataRow + 1;
  