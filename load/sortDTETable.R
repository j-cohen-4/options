sortDTETable <- function(dte, symbolPrefix="CL"){
  
    # Start with any month, H
    sym      = paste(symbolPrefix, "H", sep="");
    all.syms = colnames(dte);
    jan      = sort(all.syms[grep(pattern=sym, all.syms)]);
    firstYr  = as.numeric(substr(jan[1], 4, 5));
    lastYr   = as.numeric(substr(tail(jan, 1), 4, 5));
    
    # Build list
    sorted   = vector();
    all.yrs  = seq(firstYr, lastYr, 1);
    nLen     = (lastYr - firstYr) + 1;
    for(i in 1:nLen){
        sorted = c(sorted, sort(all.syms[grep(pattern=all.yrs[i], all.syms)]));
    }
    return(dte[,sorted]);
}