buildDatasetForImpliedVol <- function(cmf.ret, 
                                      iv, 
                                      rv          = NULL, 
                                      lb          = (252*5), 
                                      period.days = 30, 
                                      param.list  = NULL){
  
    # This function takes the input data and generates a dataset that can be used to 
    # run regressions.
    #
    # Take the futures return (cmf.ret) and build volatility cones. Then z-score the current
    # implied volatility
  
    nAdded = 0;
    df.dat = data.frame(t=as.Date(character()), iv=numeric(), z=numeric(), 
                        tnext=as.Date(character()), fwdiv=numeric(), bottom=numeric(),
                        two.five=numeric(), five.zero=numeric(), seven.five=numeric(),
                        top=numeric(),stringsAsFactors=FALSE);
    
    vol.cone.idx = 4;
    cone.lengths = c(21, 42, 63, 126, 252);
    if(!is.null(param.list) && param.list$type=="data.param.list"){
        # Match the const expiration length for cone calculations
        if(!(param.list$iv.days %in% cone.lengths)){
            cone.lengths[5] = param.list$iv.days;
            vol.cone.idx    = 5;
        } else {
            vol.cone.idx = which(cone.lengths == param.list$iv.days);
        }
    }
    
    nRows = nrow(iv);
    nNext = 0.1;
    for(i in 1:nRows){
      
        # For this date, use lookback of lb to build volatility cones
        today   = index(iv[i]);
        idx.fut = tail(which(index(cmf.ret) < today), 1);
        
        # Get cones
        if(idx.fut >= lb){
        vc = getVolaCone2(cmf.ret[(idx.fut - lb + 1):idx.fut], 
                         bPlot=FALSE, 
                         cone.lengths=cone.lengths, 
                         ret.type="arith");
        } else { vc = NULL;}
        
        # Does dependent variable exist?
        fwddate   = today + period.days;
        valid     = which(index(iv) >= fwddate);
        if(length(valid) == 0) { next; }              # Not enough data, just continue
        fwdIV     = as.numeric(iv[valid[1]]);
        
        # Z-score current iv -- independent variable
        z.val     = 0;
        currentIV = as.numeric(iv[i]);
        if(idx.fut >= lb){
        vol.dat   = tail(vc$vol[,4], -100);
        z.val     = (currentIV - mean(vol.dat)) / sd(vol.dat);}
        
        nAdded                  = nAdded+1;
        df.dat[nAdded, 't']     = today;
        df.dat[nAdded, 'iv']    = currentIV;
        df.dat[nAdded, 'z']     = z.val;
        df.dat[nAdded, 'tnext'] = index(iv[valid[1]]);
        df.dat[nAdded, 'fwdiv'] = fwdIV;
        
        if(!is.null(vc)){
        df.dat[nAdded, 'bottom']     = as.numeric(vc$cones[vol.cone.idx, 1]);
        df.dat[nAdded, 'two.five']   = as.numeric(vc$cones[vol.cone.idx, 2]);
        df.dat[nAdded, 'five.zero']  = as.numeric(vc$cones[vol.cone.idx, 3]);
        df.dat[nAdded, 'seven.five'] = as.numeric(vc$cones[vol.cone.idx, 4]);
        df.dat[nAdded, 'top']        = as.numeric(vc$cones[vol.cone.idx, 5]);
        }
        if( i / nRows > nNext){
            message(sprintf("\n%.1f percent complete!", i / nRows));
            nNext = nNext + 0.1;
        } else {
            message(".", appendLF=FALSE);
        }
    }
    
    return(df.dat);
}

appendToIVDataset <- function(df.table, ma.length=50){
  
    # The table passed in has to be the output from buildDatasetForImpliedVol (from above).
    # Add columns for change in implied vol between forward and current iv, MA of
    # iv for N days (passed in), ratio of IV to MA, and change of ratio.
  
    # Change in iv
    new.df = cbind(df.table, df.table$fwdiv - df.table$iv);
    colnames(new.df)[ncol(new.df)] = "ivchg";

    # MA
    ma     = SMA(df.table$iv, n=ma.length);
    new.df = cbind(new.df, ma);
    new.df = cbind(new.df, (new.df$iv / new.df$ma) - 1);
    colnames(new.df)[ncol(new.df)] = "ivTOma";
    
    # ROC of the IV to MA ratio -- this is a little more difficult. On today, t, look back to 
    # last trade date, so you have the ratio from today and the ratio from then. Get difference.
    # To do this, iterate t to see if it exists in tnext. If yes, take ivTOma from that row
    # for calculation.
    nRows = nrow(new.df);
    fwd   = rep(NA, 1, nRows, 1);
    for(i in 1:nRows){
        now = new.df$t[i];
        chk = new.df[new.df$tnext >= now, ];
        if(nrow(chk) > 0){
            tmp    = new.df$ivTOma[i] - chk$ivTOma[1];
            if(!is.na(tmp)){ fwd[i] = tmp; }
        }
    }
    
    new.df = cbind(new.df, fwd);
    colnames(new.df)[ncol(new.df)] = "roc.ivTOma"
    
    return(new.df);
}

addFuturesMA <- function(iv.dataset, cmf, ma_length=50){
  
    cuml.cmf = cumprod(cmf+1) - 1;
    cmf.ma   = SMA(cuml.cmf, n=ma_length);
    cmf.ma[is.na(cmf.ma)] = cuml.cmf[is.na(cmf.ma)];
    cmf.ma   = cbind(cuml.cmf, cmf.ma);
    colnames(cmf.ma) = c("cmf.cuml", sprintf("%s.ma", ma_length));
    
    # Only take dates that match dataset
    dt.start = max(index(cuml.cmf[1,]), iv.dataset$t[1]);
    dt.end   = min(index(tail(cuml.cmf, 1)), tail(iv.dataset$t,1));
    new.df   = iv.dataset[iv.dataset$t >= dt.start & iv.dataset$t <= dt.end, ];
    
    # Error checking
    if(nrow(new.df) > nrow(cmf.ma[new.df$t,])){
        # Remove extra dates -- most likely holidays that shouldn't be there in the first place
        missing.dates = as.Date(setdiff(new.df$t, index(cmf.ma[new.df$t,])));
        new.df = new.df[!(new.df$t %in% missing.dates),];
    }
    new.df   = cbind(new.df, coredata(cmf.ma[new.df$t,]));
    
    return(new.df);
}