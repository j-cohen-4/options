createConstantMaturityCrudeFuture <- function(nDays, product="crude"){
  
      g_env       = globalenv();
      dte.var     = sprintf("%s.dte.futures", product);
      futures.var = sprintf("%s.futures", product);
      
      # Create a single futures return stream with a constant 'nDays' to expiration
      if(!exists(dte.var, envir=g_env)){
          stop("createConstantMaturityCrudeFuture: Expirations not loaded!");
      }
      prod.dte.futures = get(dte.var, envir=g_env);
      
      if(!exists(futures.var, envir=globalenv())){
          stop("createConstantMaturityCrudeFuture: Prices not loaded!");
      }
      prod.futures  = get(futures.var, envir=g_env);
      
      # The returns
      prod.returns  = diff(log(prod.futures));
      prod.returns  = exp(prod.returns) - 1;
      prod.names    = colnames(prod.returns);

      nCount = nrow(prod.dte.futures);
      for(i in 1:nCount){
        
          # Get DTE
          idx.pre  = which(prod.dte.futures[i,] <= nDays);
          idx.post = which(prod.dte.futures[i,] > nDays);
          
          if(length(idx.pre) > 0 && length(idx.post) > 0){
              idx1   = tail(idx.pre, 1);
              idx2   = idx.post[1];
              dte1   = as.numeric(prod.dte.futures[i, idx1]);
              dte2   = as.numeric(prod.dte.futures[i, idx2]);
              bExact = (dte1 == nDays);
              bZero  = (dte1 == 0 || dte1 == -1);
              dt     = index(prod.dte.futures[i,]);
        
              if(bExact || bZero){
                  if(bExact == TRUE){
                      r = as.numeric(prod.returns[dt, idx1]);
                  } else {
                      r = as.numeric(prod.returns[dt, idx2]);
                  }
              } else {
                  
                  # Interpolate
                  r1 = as.numeric(prod.returns[dt, idx1]);
                  r2 = as.numeric(prod.returns[dt, idx2]);
                  
                  # If a problem, typically with second month
                  if(is.na(r2)){
                      message(sprintf("Second month[%s] has NA for return on %s, using front month[%s] at %d DTE", 
                                      prod.names[idx2], dt, prod.names[idx1], dte1));
                      r  = r1;
                  } else {
                      w1 = (dte2 - nDays) / (dte2 - dte1);
                      w2 = 1 - w1;
                      r  = (r1 * w1) + (r2 * w2);
                  }
              }
              
              if(exists("const.mat")){
                  const.mat = rbind(const.mat, xts(r, order.by=dt));
              } else {
                  const.mat = xts(r, order.by=dt);
              }
          }
      }
      
      return(const.mat);
}