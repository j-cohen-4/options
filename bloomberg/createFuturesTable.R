createFuturesTable <- function(tickers, futures){
  
    # Regardless of how tickers come in, the go out with 2 digit years. So CLM8 becomes CLM18.
    nNum  = length(tickers);
    nCols = nNum * 2;
    df    = data.frame(matrix("", ncol=nCols, nrow=400), stringsAsFactors=FALSE);
    nMax  = 0;
    nLmt  = nNum;
    nSkip = 0;
    
    for(i in 1:nLmt){
        
        dtCol   = ((i - 1) * 2) + 1;
        prcCol  = dtCol + 1;
        fut.dat = futures[[i]];
        nRows   = nrow(fut.dat);
        nMax    = ifelse(nRows > nMax, nRows, nMax);
        
        # Don't handle empty rows
        if(nRows == 0){ 
            nSkip = nSkip + 1; 
            next; 
        }
        
        # Add data
        df[1:nRows,dtCol]  = format(fut.dat[,1], "%m/%d/%Y");
        df[1:nRows,prcCol] = fut.dat[,2];
        
        # Standardize symbol
        sym.parts = strsplit(tickers[i], " ")[[1]];
        sym       = tickers[i];
        if(length(sym.parts) == 2){
            # Format = SSMY[Y] Comdty
            if(nchar(sym.parts[1]) == 4){
                sym = paste(substr(sym.parts[1], 1, 3), "1", substr(sym.parts[1], 4, 4), " Comdty", sep="");
            } 
        } else if(length(sym.parts == 3)){
            # Format = S MY[Y] Comdty -- investigate middle part which is exp month + year
            if(nchar(sym.parts[2]) == 2){
                exp = paste(substr(sym.parts[2], 1, 1), "1", substr(sym.parts[2], 2, 2), sep="");
                sym = sprintf("%s %s Comdty", sym.parts[1], exp);
            }
        }

        
        if(i == 1){
            hdr = c(sym, "PX_LAST");
        } else {
            hdr = c(hdr, sym, "PX_LAST");
        }
    }

    # Pare df to what was filled out
    nLmt         = nLmt - nSkip;
    df           = df[1:nMax, 1:(nLmt * 2)];
    colnames(df) = hdr;

    return(df);
}