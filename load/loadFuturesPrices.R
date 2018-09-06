loadFuturesPrices <- function(sFile='data/crude/CrudeFuturesPrices.csv'){
  
    # Data format starts as:
    # CLmy[y] Comdty, PX_LAST, CLmy[y] Comdty, PX_LAST...
    # Date, price, Date, price...
    # Some symbols have a 1 digit year (7 instead of 17) and needs to be changed to two digit
    # for uniformity (as well as dropping 'Comdty')
  
    dat  = read.csv(sFile, stringsAsFactors=FALSE, header=TRUE, check.names=FALSE);
    syms = colnames(dat)[seq(1, ncol(dat), 2)];
    nNum = length(syms);
    
    for(i in 1:nNum){
      
        nDateCol = ((i - 1) * 2) + 1;
        nPrcCol  = nDateCol + 1;
        sSym     = strsplit(syms[i], " ")[[1]][1];
        if(nchar(sSym) == 4){
           sSym     = sprintf("%s1%s", substr(sSym, 1, 3), substr(sSym, nchar(sSym), nchar(sSym)));
        }        
        # Get data
        dates    = dat[,nDateCol];
        dates    = dates[dates != ""];
        dates    = as.Date(dates, format="%m/%d/%Y");
        prices   = as.numeric(dat[1:length(dates),nPrcCol]);
        
        if(i == 1){
            sCols = sSym;
            tbl   = xts(prices, order.by=dates);
        } else {
            sCols = c(sCols, sSym);
            tbl   = cbind(tbl, xts(prices, order.by=dates));
        }
    }
    
    colnames(tbl) = sCols;
    return(tbl);
}