data {
  int N1; // length of low frequency series
  int N2; // length of high frequency series
  int freq; // every freq-th observation of the high frequency series we get an observation of the low frequency one
  vector[N1] y;
  vector[N2] z1;
  vector[N2] z2;
}
parameters {
  real<lower = 0> sigma_epsilon;
  real<lower = 0> sigma_eta;
  vector[2] gamma;
  vector[N2] x;
}
model {
  int count;
  // priors
  sigma_epsilon ~ cauchy(0,1);
  sigma_eta ~ cauchy(0,1);
  gamma ~ normal(0,1);
  increment_log_prob(normal_log(x[1], 0, 1));
  
  // likelihood
  count <- 0;
  for(i in 2:N2){
    increment_log_prob(normal_log(x[i], x[i-1] + z1[i]*gamma[1] + z2[i]*gamma[2], sigma_eta));
    if(i%freq==0){
      count <- count + 1;
      increment_log_prob(normal_log(y[count], x[i], sigma_epsilon));
    }
  }
}