library(Matrix)


f_delta_stat <- function(x){ # yields[c, var_est_c, CI95]
  
  # by Jim Ruzicka
  # calculate mean, variance, and 95# CI of a delta-lognormal distribution.
  # takes:
  #     x (vector of observations; can contain NaNs)
  # returns:
  #     c (mean)
  #     d (variance)
  #     CI95 (+/-95# CI statistics)
  # calls:
  #     g_function (internal sub-function at bottom of this file)
  # From: Pennington 1996. Estimating the mean and varince from
  # highly skewed marine data. Fishery Bulletin 94:498-505.
  # revision date: 9-3-2010
  # 9-3-2010 (switched to using var_est_c instead of d for variance)

# -----------------------------------------------------------
library(Matrix)
  
#function g = g_function(m, t)
g_function <- function(m,t){
  
  # the g function from Pennington 2003 (eq. 3)
  
  a = 0;
  #for j = 2:21;
  for(j in 2:21){ # matlab factorial function only good for n <= 21, accurate to 15 places
    #k = 2:1:j;
    k <- seq(2,j,1)
    l = (m + 2*k - 3);
    b1 = ((m-1)^(2*j-1)) * (t^j);
    b2 = m^j * prod(l) * factorial(j);
    b = b1 / b2;
    a = a + b;
    #end;
  }
  
  g = 1 + ((m-1)/m)*t + a;
  
  return(g)
  
}
# end g-function ---------------------------------------------

#x(isnan(x)) = []; # remove NaNs
x <- na.omit(x)
#n = length(x);
n <- length(x)

#nonzero_x =  nonzeros(x);
nonzero_x <- x[x > 0]

#nonzero_n = length(nonzero_x);
nonzero_n <- length(nonzero_x)

#if (n == 0) && (nonzero_n == 0)
if((n == 0) & (nonzero_n == 0)){

c = NaN; d = NaN; var_est_c = NaN; CI95 = NaN;

#elseif (n > 0) && (nonzero_n == 0)
} else if((n > 0) & (nonzero_n == 0)){
  
  c = 0; d = 0; var_est_c = NaN; CI95 = NaN;

#elseif (n > 0) && (nonzero_n > 0)
} else if((n > 0) & (nonzero_n >0)){

# x_bar = mean(x); #KR - not sure what these are used for as not called elsewhere in code
# x_var = var(x);

y = log(nonzero_x); # logged non-zero values of x
y_bar = mean(y, na.rm = T);
y_var = var(y,na.rm = T);

#m = nnz(x);
m = nnzero(x);

}

# calculate c (mean of delta-distribution; eq. 1)
if(m > 1){
  t = (y_var/2);
  g = g_function(m, t);
  c = (m/n) * exp(y_bar) * g;
} else if(m == 1){
  #nonzero = find(~isequal(x, 0));
  nonzero <- (x != 0)
  #c = nonzeros(x)/n;#
  c = x[nonzero]/n;
} else { # m = 0
  c = 0;
}

# calculate d (variance of delta-distribution; eq. 2)
if(m > 1){
  t1 = (y_var*2);
  t2 = ((m-2)/(m-1)) * y_var;
  g1 = g_function(m, t1);
  g2 = g_function(m, t2);
  d = (m/n) * exp(2*y_bar) * (g1 - ((m-1)/(n-1)) * g2);
} else if(m == 1){
  #nonzero = find(~isequal(x, 0));
  nonzero <- (x != 0)
  #d = (nonzeros(x)^2)/n;
  d = (x[nonzero])^2/n;
} else { # m = 0
  d = 0;
  #end;
}

# calculate estimated variance of c and 95# CI (eq. 4)
if(m > 1){
  t1 = (y_var/2);
  t2 = ((m-2)/(m-1)) * y_var;
  g1 = g_function(m, t1);
  g2 = g_function(m, t2);
  var_est_c = (m/n) * exp(2*y_bar) * ((m/n) * g1^2 - ((m-1)/(n-1)) * g2);
} else if (m == 1){
  #nonzero = find(~isequal(x, 0));
  nonzero <- (x != 0)
  #var_est_c = (nonzeros(x)/n)^2;
  var_est_c = (x[nonzero]/n)^2;
  
} else {# m = 0
  var_est_c = 0;
  #end;
}

CI95 = 2 * sqrt(var_est_c);

delta_stats <- list(mean = c, variance = var_est_c, CI95 = CI95)
return(delta_stats)

}

  # end R-script**************************************************