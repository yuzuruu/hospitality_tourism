// hierarchical model 

data {
  int<lower=1> T;                   // Length of time series
  int<lower=1> number_mode;         // Number of modes
  int<lower=1> number_occasion;     // Number of occasions
  int<lower=1> N_obs;               // Number of observed data points
  array[N_obs] int<lower=1, upper=T> obs_time;   // Time indices for observed values
  array[N_obs] int<lower=1, upper=number_mode> obs_mode; // Mode indices for observed values
  array[N_obs] int<lower=1, upper=number_occasion> obs_occasion; // Occasion indices for observed values
  vector[N_obs] speed_obs;          // Observed values
}

parameters {
  vector[T] x_raw;                  // Latent state (non-centered)
  vector[T] m_raw;                  // Mode-level mean process (non-centered)
  vector[T] h_raw;                  // Occasion-level mean process (non-centered)
  vector[number_mode] r_raw_mode;   // Mode-level random effects (non-centered)
  vector[number_occasion] r_raw_occasion;   // Occasion-level random effects (non-centered)
  real<lower=0> s_w;                // Process noise standard deviation
  real<lower=0> s_m;                // Mean process noise standard deviation
  real<lower=0> s_h;                // Occasion process noise standard deviation
  real<lower=0.2> s_s;              // Observation noise standard deviation
  real<lower=0> s_r_mode;           // Random effect standard deviation for modes
  real<lower=0> s_r_occasion;       // Random effect standard deviation for occasions
}

transformed parameters {
  vector[T] x;                      // Latent state (centered)
  vector[T] m;                      // Mode-level process (centered)
  vector[T] h;                      // Occasion-level process (centered)
  vector[number_mode] r_mode;        // Mode-level random effects (centered)
  vector[number_occasion] r_occasion;// Occasion-level random effects (centered)
  
  // Non-centered parameterization for better sampling
  x[1] = x_raw[1];  // Set initial state
  for (t in 2:T) {
    x[t] = x[t-1] + s_w * x_raw[t];  // Latent state evolution
  }
  
  r_mode = s_r_mode * r_raw_mode;  // Scale random effects
  r_occasion = s_r_occasion * r_raw_occasion;  // Scale random effects
  
  m[1] = x[1] + r_mode[obs_mode[1]];
  for (t in 2:T) {
    m[t] = m[t-1] + s_m * m_raw[t] + r_mode[obs_mode[t]]; // Mode-level mean process evolution
  }
  
  h[1] = m[1] + r_occasion[obs_occasion[1]];
  for (t in 2:T) {
    h[t] = h[t-1] + s_h * h_raw[t] + r_occasion[obs_occasion[t]]; // Occasion-level mean process evolution
  }
}

model {
  // Priors
  s_w ~ student_t(3, 0, 10);
  s_m ~ student_t(3, 0, 10);
  s_h ~ student_t(3, 0, 10);
  s_s ~ student_t(3, 0, 10);
  s_r_mode ~ student_t(3, 0, 10);
  s_r_occasion ~ student_t(3, 0, 10);

  // Latent state: Non-centered parameterization improves convergence
  x_raw ~ normal(0,1);
  m_raw ~ normal(0,1);
  h_raw ~ normal(0,1);
  
  // Random effects: Non-centered parameterization
  r_raw_mode ~ normal(0,1);
  r_raw_occasion ~ normal(0,1);

  // Hierarchical normal model for speed observations
  speed_obs ~ normal(h[obs_time], s_s);
}

generated quantities {
  vector[N_obs] log_lik;  // Log-likelihood for WAIC computation
  vector[T] estimated_speed;
  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(speed_obs[n] | h[obs_time[n]], s_s);
  }
  
  for (t in 1:T) {
    estimated_speed[t] = normal_rng(h[t], s_s);  // Estimated speed for the full time range
  }
}

