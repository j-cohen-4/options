getVolaCone2 <- function(returns, 
                         symbol       = "CLA", 
                         bPlot        = TRUE, 
                         cone.lengths = c(20, 40, 60, 120, 240), 
                         ret.type     = "log"){     # arith or log
  
  # 'returns' should be passed in as log returns (using rollsum)
  if(!(ret.type %in% c("log","arith"))){
      stop("getVolaCone2: Invalid 'ret.type', use 'log' or 'arith'");
  }
  
  # Need 5 buckets for cones
  if(length(cone.lengths) != 5){
      stop("getVolaCone2: There needs to be 5 buckets for 'cone.lengths'");
  }
  
  # Need to use log returns
  if(ret.type == "arith"){
      returns = log(1+returns);
  }
  ret2   = returns ^ 2;
  
  # Calculate volatilies
  vol20  = sqrt(rollsum(ret2, cone.lengths[1]) / (cone.lengths[1] - 1)) * sqrt(252);
  vol40  = sqrt(rollsum(ret2, cone.lengths[2]) / (cone.lengths[2] - 1)) * sqrt(252);
  vol60  = sqrt(rollsum(ret2, cone.lengths[3]) / (cone.lengths[3] - 1)) * sqrt(252);
  vol120 = sqrt(rollsum(ret2, cone.lengths[4]) / (cone.lengths[4] - 1)) * sqrt(252);
  vol240 = sqrt(rollsum(ret2, cone.lengths[5]) / (cone.lengths[5] - 1)) * sqrt(252);
  
  # Adjustment factor
  T      = nrow(returns);
  adj20  = calculateAdjustmentFactor(cone.lengths[1], T);
  adj40  = calculateAdjustmentFactor(cone.lengths[2], T);
  adj60  = calculateAdjustmentFactor(cone.lengths[3], T);
  adj120 = calculateAdjustmentFactor(cone.lengths[4], T);
  adj240 = calculateAdjustmentFactor(cone.lengths[5], T);
  
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
