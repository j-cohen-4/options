generateOptionDatTbl <- function(xtsRow){
  
    # Take column headers (SYMmY.KK[K]) and build table where col header becomes row
    # header and the col info is Strike (numeric), IV, and Delta 
  
    nLen  = ncol(xtsRow) - 2;                      # Col 1 is futures price, Last col is DTE in days
    df    = data.frame(type=character(), strike=numeric(), price=numeric(), iv=numeric(), 
                       delta=numeric(), contract=character(), future=character(), 
                       stringsAsFactors=FALSE);
    
    # Get spot and dte
    spot  = as.numeric(xtsRow[1,1]);
    dte   = as.numeric(xtsRow[1,nLen+2]) / 360;
    sCols = colnames(xtsRow);
    nRow  = 1;
    
    for(i in 1:nLen){
  
        idx      = i+1;
        contract = sCols[idx];
        dStrike  = as.numeric(substr(contract, 7, nchar(contract)));
        type     = substr(contract, 5, 5);
        
        # Get proper function
        isPut    = type == "P";
        fnIV     = bscallimpvol;
        if(isPut){
            fnIV = bsputimpvol;
        }
        
        # Price data could be garbage, move on if it is
        if(is.na(xtsRow[1, idx])){
            next;
        }
        # Implied vol and delta
        prc      = as.numeric(xtsRow[1, idx]);
        iv       = fnIV(s=spot, k=dStrike, r=0, dte, 0, prc);
        dlt      = bsopt(s=spot, k=dStrike, v=iv, r=0, tt=dte, d=0);
        
        if(isPut){
            d = dlt$Put['Delta', 'bsput'];  
        } else {
            d = dlt$Call['Delta', 'bscall'];
        }
        
        # Fill out table
        df[nRow, 'type']     = type;
        df[nRow, 'strike']   = dStrike;
        df[nRow, 'price']    = prc;
        df[nRow, 'iv']       = iv;
        df[nRow, 'delta']    = d;
        df[nRow, 'contract'] = contract;
        df[nRow, 'future']   = sCols[1];
        
        # Move to next row
        nRow = nRow + 1;
    }
    
    df = df[order(df$strike),];    
    return(df);
}