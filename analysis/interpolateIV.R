interpolateIV <- function(opt1, opt2, pivot, r=0.0185){
  
    # Function interpolative implied vol between the 2 options passed in. Both opt1 and opt2 need
    # to be lists that are the output of findATMCall, so has the elements: s0, k, cp, dte
    elements = c("s0","k","cp","dte");
    val1     = sum(names(opt1) %in% elements);
    val2     = sum(names(opt2) %in% elements);
    if(val1 != 4 || val2 != 4){
        message("interpolateIV: opt1 or opt2 not valid parameters!");
        return(0);
    }
    
    # Error check for call price -- can't be less than intrinisic
    call1 = ifelse(opt1$cp == 0, 0.0001, round(opt1$cp, 13));
    call2 = ifelse(opt2$cp == 0, 0.0001, round(opt2$cp, 13));

    iv1  = bscallimpvol(opt1$s0, opt1$k, r, opt1$dte, 0, call1);
    iv2  = bscallimpvol(opt2$s0, opt2$k, r, opt2$dte, 0, call2);
    day1 = opt1$dte * 360;
    day2 = opt2$dte * 360;
    
    if(day1 < day2){
        wgt1 = 1 - (pivot - day1)/(day2 - day1);
    }else{
        wgt1 = 1 - (pivot - day2)/(day1 - day2);
    }
    
    return( list(iv=(iv1 * wgt1) + iv2 * (1-wgt1), iv1=iv1, iv2=iv2));
}