data {
  int<lower=1> T;                   
  int<lower=1> number_mode;         
  int<lower=1> number_occasion;     
  int<lower=1> K_max;               
  int<lower=1> N_obs;               
  array[N_obs] int<lower=1> obs_time;
  array[N_obs] int<lower=1, upper=number_mode> obs_mode;
  array[N_obs] int<lower=1, upper=number_occasion> obs_occasion;
  vector[N_obs] speed_obs;
  vector[N_obs] Z_obs;  
}

parameters {
  real<lower=0> a;                   
  real<upper=0> b;                   
  vector[K_max] c;                    
  real<lower=0> s_h_mode;
  real<lower=0> s_h_occasion;
  vector[T] x_raw;                   
  vector[number_mode] r_raw_mode;    
  vector[number_occasion] r_raw_occasion; 
  real<lower=0> s_w;                 
  real<lower=0.2> s_v;               
  real<lower=0> s_r_mode;            
  real<lower=0> s_r_occasion;        
}

transformed parameters {
  vector<lower=-100, upper=100>[T] x;   // Constraint added
  vector<lower=-10, upper=10>[number_mode] r_mode;
  vector<lower=-10, upper=10>[number_occasion] r_occasion;

  x[1] = x_raw[1];  
  for (t in 2:T) {
    x[t] = x[t-1] + s_w * x_raw[t];  
  }
  
  r_mode = s_r_mode * r_raw_mode;  
  r_occasion = s_r_occasion * r_raw_occasion;  
}

model {
  a ~ normal(10, 5);   
  b ~ normal(-1, 1);  
  c ~ normal(-0.5, 1);  
  s_w ~ student_t(3, 0, 10);
  s_v ~ student_t(3, 0, 10);
  s_r_mode ~ student_t(3, 0, 10);
  s_r_occasion ~ student_t(3, 0, 10);

  x_raw ~ normal(0,1);
  r_raw_mode ~ normal(0, s_h_mode);
  r_raw_occasion ~ normal(0, s_h_occasion);

  for (n in 1:N_obs) {
    real lagged_effect = 0;
    for (k in 1:K_max) {
      if (n - k > 0) {  
        lagged_effect += c[k] * log10(fmax(Z_obs[max(1, n - k)], 1e-6));  
      }
    }
    speed_obs[n] ~ normal(
      a + b * log10(fmax(Z_obs[n], 1e-6)) + lagged_effect +  
      x[min(obs_time[n], T)] +  
      r_mode[obs_mode[n]] + 
      r_occasion[obs_occasion[n]],
      s_v
    );
  }
}


generated quantities {
  array[N_obs] real log_lik;  
  array[N_obs] real estimated_speed;
  
  for (n in 1:N_obs) {
    real lagged_effect;
    // real lagged_effect = 0;
    for (k in 1:K_max) {
      if (n - k > 0) {
        lagged_effect += c[k] * log10(Z_obs[max(1, n - k)]);
      }
    }
    
    // Print debugging info to detect -inf values
    // print("n: ", n, 
    //       " | log10(Z_obs): ", log10(Z_obs[n]),
    //       " | lagged_effect: ", lagged_effect,
    //       " | x: ", x[min(obs_time[n], T)],
    //       " | r_mode: ", r_mode[obs_mode[n]],
    //       " | r_occasion: ", r_occasion[obs_occasion[n]]);

    log_lik[n] = normal_lpdf(speed_obs[n] | 
      a + b * log10(Z_obs[n]) + lagged_effect +  
      x[min(obs_time[n], T)] +  
      r_mode[obs_mode[n]] + 
      r_occasion[obs_occasion[n]],
      s_v
    );

    estimated_speed[n] = normal_rng(
      a + b * log10(Z_obs[n]) + lagged_effect +  
      x[min(obs_time[n], T)] +  
      r_mode[obs_mode[n]] + 
      r_occasion[obs_occasion[n]], 
      s_v
    );
  }
}

