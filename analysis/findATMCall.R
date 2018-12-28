findATMCall <- function(theDate, sym, r=0.0185, product="crude", opt.inc=1){
  
    g_env   = globalenv();
    ret.obj = list(s0=NA, k=NA, cp=NA, dte=0);
    prod.futures.var = sprintf("%s.futures", product);
    
    # Sanity check: globally loaded data
    if(!exists(prod.futures.var, envir=g_env)){
        message("findATMCall: Unable to run properly because futures not loaded!");
        return(ret.obj);
    }
    prod.futures = get(prod.futures.var, envir=g_env);
    
    if(!exists(sym, envir=g_env)){
        message(cat("findATMCall: Unable to run properly because '", sym,"' not loaded!", sep=""));
        return(ret.obj);
    }
    
    # Get futures price
    fp = as.numeric(prod.futures[theDate, sym]);
    if(length(fp) == 0 || is.na(fp)){
        message(cat("findATMCall: Unable to load futures price for ", sym, sep=""));
        return(ret.obj);
    }
    
    # Need a precision keep numbers in proper alignment
    precision = 1;
    if(opt.inc < 1){
        chars = nchar(strsplit(as.character(opt.inc), "[.]")[[1]][2]);
        precision = 10^chars;
    }
    # Nearest strike
    mod    = fp %% opt.inc;
    k      = ifelse(mod < (opt.inc/2), fp - mod, fp + (opt.inc - mod));
    k      = round(k * precision) / precision;                    # Should get all.ks and k to same length
    dat    = get(sym, envir=g_env);
    all.ks = substr(colnames(dat), 7, max(nchar(colnames(dat))));
    all.ks = as.numeric(all.ks);
    all.ks[is.na(all.ks)] = 0;
#    idx    = grep(k, colnames(dat));
    idx    = which(all.ks == k);
    if(length(idx) == 0){

        # No strike found -- check if we need to go up or down
        nLimit = 25;
        old.fp = as.numeric(head(dat[dat$DTE<=120,1], 1));
        if(old.fp > fp){
          
            # Market has fallen (probably too far) so work way up
            nTries = 0;
            k.orig = k;
            k      = min(as.numeric(all.ks[all.ks > k & all.ks != 0]));
            if(is.infinite(k)){
                idx = which(abs(k.orig - all.ks) == min(abs(k.orig - all.ks)));
                k   = all.ks[idx];
                message("..fixing wrong direction..");
            } else {
                idx    = which(all.ks == k);
            }
#            while(nTries <= nLimit && length(idx) == 0){
#                k      = k + opt.inc;
#                idx    = grep(k, colnames(dat));
#                nTries = nTries + 1;
#            }
            if(abs(k - k.orig) > (opt.inc / 100)){
                message(sprintf("[%s]ATM should be %f, using %f", sym, k.orig, k));
                # Need to add puts from k to k.orig - 1
                #tmp.sym = sprintf("%s%sP", substr(sym, 1, 3), substr(sym, 5, 5));
                #for(z in k.orig:(k - 1)){
                #    new.opt = sprintf("%s %.1f Comdty", tmp.sym, z);
                #    if(!new.opt %in% df.requests[,sym]){
                #        nLast = which(df.requests[,sym] == "")[1];
                #        df.requests[nLast, sym] <<- new.opt;
                #    }
                #}
            }
        } else {
          
            #Market has risen, work way down
            nTries = 0;
            k.orig = k;
            k      = max(as.numeric(all.ks[all.ks < k & all.ks != 0]));
            if(is.infinite(k)){
              idx = which(abs(k.orig - all.ks) == min(abs(k.orig - all.ks)));
              k   = all.ks[idx];
              message("__fixing wrong direction__");
            } else {
              idx    = which(all.ks == k);
            }
#            while(nTries <= nLimit && length(idx) == 0){
#                k      = k - opt.inc;
#                idx    = grep(k, colnames(dat));
#                nTries = nTries + 1;
#            }
            if(abs(k - k.orig) > (opt.inc / 100)){
                message(sprintf("[%s]ATM should be %f, using %f", sym, k.orig, k));
                # Need to add calls from k.orig+1 to k
                #tmp.sym = sprintf("%s%sC", substr(sym, 1, 3), substr(sym, 5, 5));
                #for(z in (k+1):k.orig){
                #    new.opt = sprintf("%s %.1f Comdty", tmp.sym, z);
                #    if(!new.opt %in% df.requests[,sym]){
                #        nLast = which(df.requests[,sym] == "")[1];
                #        df.requests[nLast, sym] <<- new.opt;
                #    }
                #}
            }
        }
    } else if(length(idx) > 1){
      
        stop("Revisit this error!");
        #z = length(idx);
        #for(y in 1:z){
        #    tmp = colnames(dat)[idx[y]];
        #    tmpK = as.numeric(substr(tmp, 7, nchar(tmp)));
        #    if(abs(tmpK - k) < 0.000001) {
        #        idx = idx[y];
        #        break;
        #    }
        #}
    }
    strK   = colnames(dat)[idx];
    prc    = as.numeric(dat[theDate, strK]);
    isCall = substr(strK, 5, 5) == "C";
    t      = as.numeric(dat[theDate, 'DTE']) / 360;
    
    if(isCall == FALSE){
      
        # Use put call parity to convert put to call price
        # C + K / (1+r)^t = s0 + P
        # *** C + Ke^-rt = s0 + P
        # prc = fp + prc - (k / (1+r)^t);
        prc = fp + prc - k * exp(-r*t);
        
        if(!is.na(prc) && prc < 0){
            message(sprintf("findATMCall: %s strike=%f fp=%f generating negative call price",
                    theDate, k, fp));
            
            # Use precision to set smallest increment in value -- may not be exactly correct
            fp.decimals = nchar(strsplit(as.character(fp), "[.]")[[1]][2]);
            prc = 1 / (10 ^ fp.decimals);
            message(sprintf("\t setting price to %f", prc));
        }
    }
    
    ret.obj$s0  = fp;
    ret.obj$k   = k;
    ret.obj$cp  = prc;
    ret.obj$dte = t;
    return(ret.obj);
}