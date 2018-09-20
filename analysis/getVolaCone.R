getVolaCone <- function(returns, symbol="CLA", bPlot=TRUE){
  
    # 'returns' should be passed in as log returns (using rollsum)
  
    ret2   = returns ^ 2;
    
    # Calculate volatilies
    vol20  = sqrt(rollsum(ret2, 20) / 19) * sqrt(252);
    vol40  = sqrt(rollsum(ret2, 40) / 39) * sqrt(252);
    vol60  = sqrt(rollsum(ret2, 60) / 59) * sqrt(252);
    vol120 = sqrt(rollsum(ret2,120) / 119)* sqrt(252);
    vol240 = sqrt(rollsum(ret2,240) / 239)* sqrt(252);
    
    # Adjustment factor
    T      = nrow(returns);
    adj20  = calculateAdjustmentFactor(20, T);
    adj40  = calculateAdjustmentFactor(40, T);
    adj60  = calculateAdjustmentFactor(60, T);
    adj120 = calculateAdjustmentFactor(120, T);
    adj240 = calculateAdjustmentFactor(240, T);
    
    q20 = quantile(vol20, seq(0, 1, 0.25)) * adj20;
    q40 = quantile(vol40, seq(0, 1, 0.25)) * adj40;
    q60 = quantile(vol60, seq(0, 1, 0.25)) * adj60;
    q120= quantile(vol120,seq(0, 1, 0.25)) * adj120;
    q240= quantile(vol240,seq(0, 1, 0.25)) * adj240;
    
    # Pad vols with 0
    len    = length(vol20);
    vol40  = c(rep(0, len - length(vol40)), vol40);
    vol60  = c(rep(0, len - length(vol60)), vol60);
    vol120 = c(rep(0, len - length(vol120)), vol120);
    vol240 = c(rep(0, len - length(vol240)), vol240);
    
    # Create series
    cones = cbind(q20, q40, q60, q120, q240);
    x     = c(20,40,60,120,240);
    x     = cbind(x, x, x, x, x);
    y.rng = c(0, max(cones) * 1.05);
    
    if(bPlot==TRUE){
        matplot(x, t(cones), type='b', pch='o', 
                main=paste(symbol, " Volatility Cone"), xlab='Days', ylab='Vol', ylim=y.rng);
    }    
    
    return(list(x=x, cones=t(cones), vol=cbind(vol20, vol40, vol60, vol120, vol240), rng=y.rng));
}

calculateAdjustmentFactor <- function(h, T, vol=TRUE){
    n   = (T - h) - 1;
    adj = 1 / (1 - (h/n) + ((h^2-1) / (3*n^2)));
    
    if(vol){
        adj = sqrt(adj);
    }
    
    return(adj);
}