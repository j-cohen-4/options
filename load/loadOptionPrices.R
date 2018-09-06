loadOptionPrices <- function(sFile="data/crude/CLG8 Options.csv", expiration=NULL){
  
    dat = read.csv(sFile, header=TRUE, stringsAsFactors=FALSE, check.names=FALSE);
    dat[is.na(dat)] = "";
    
    # Input format: Colheaders = Symbol1, PX_LAST, Symbol2, PX_LAST, ...
    #               Data       = Date   , price  , date   , price ...
    # 2 columns for every option series
    nCols = ncol(dat);
    iter  = nCols / 2;
    syms  = colnames(dat)[seq(1,nCols,2)];
    syms  = c(syms, "DTE");
    remove = vector();
    
    for(i in 1:iter){
      
        dateCol = ((i - 1) * 2) + 1;
        prcCol  = dateCol + 1;
        syms[i] = gsub(' Comdty', "", syms[i]);
        
        theDates = dat[,dateCol];
        theDates = theDates[theDates != ""];

        # Check for empty strike
        if(length(theDates) == 0){
            remove[length(remove)+1] = syms[i];
            next;
        }
        theDates = as.Date(theDates, format("%m/%d/%Y"));
        xtsDat   = xts(as.numeric(dat[1:length(theDates), prcCol]), order.by=theDates);
        
        if(i == 1){
            opt_table = xtsDat;
        } else {
            opt_table = cbind(opt_table, xtsDat);
        }
    }
    
    # Add column for DTE
    if(is.null(expiration)){
      expiration = tail(index(opt_table), 1);
    }
    syms      = syms[! syms %in% remove];
    opt_table = cbind(opt_table, xts(as.numeric(expiration - index(opt_table)), order.by=index(opt_table)));
    colnames(opt_table) = syms[1:ncol(opt_table)];
    
    return(opt_table);
}