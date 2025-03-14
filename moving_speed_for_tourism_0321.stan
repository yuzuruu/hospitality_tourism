data {
  int<lower=1> T;                   // Length of time series
  int<lower=1> number_mode;         // Number of modes
  int<lower=1> number_occasion;     // Number of occasions
  int<lower=1> N_obs;               // Number of observed data points
  array[N_obs] int<lower=1, upper=T> obs_time;   // Time indices for observed values
  array[N_obs] int<lower=1, upper=number_mode> obs_mode; // Mode indices for observed values
  array[N_obs] int<lower=1, upper=number_occasion> obs_occasion; // Occasion indices for observed values
  vector[N_obs] speed_obs;          // Observed speed values
  vector[N_obs] Z_obs;              // Observed predictor variable
}

parameters {
  real<lower=0> a;                   // Intercept (must be > 0)
  real<upper=0> b;                   // Slope for log(Z_obs) (must be < 0)
  real<lower=0> s_h_mode;
  real<lower=0> s_h_occasion;
  vector[T] x_raw;                   // Latent state (non-centered)
  vector[number_mode] r_raw_mode;    // Mode-level random effects (non-centered)
  vector[number_occasion] r_raw_occasion; // Occasion-level random effects (non-centered)
  real<lower=0> s_w;                 // Process noise standard deviation
  real<lower=0.2> s_v;               // Observation noise standard deviation
  real<lower=0> s_r_mode;            // Random effect standard deviation for modes
  real<lower=0> s_r_occasion;        // Random effect standard deviation for occasions
}

transformed parameters {
  vector[T] x;                      // Latent state (centered)
  vector[number_mode] r_mode;        // Mode-level random effects (centered)
  vector[number_occasion] r_occasion;// Occasion-level random effects (centered)

  // Non-centered parameterization for better sampling
  x[1] = x_raw[1];  // Set initial state
  for (t in 2:T) {
    x[t] = x[t-1] + s_w * x_raw[t];  // Reparameterized random walk
  }
  
  r_mode = s_r_mode * r_raw_mode;  // Scale random effects
  r_occasion = s_r_occasion * r_raw_occasion;  // Scale random effects
}

model {
  // Priors
  a ~ normal(10, 5);   // Prior ensures a > 0
  b ~ normal(-1, 1);   // Prior ensures b < 0
  s_w ~ student_t(3, 0, 10);
  s_v ~ student_t(3, 0, 10);
  s_r_mode ~ student_t(3, 0, 10);
  s_r_occasion ~ student_t(3, 0, 10);

  // Latent state: Non-centered parameterization improves convergence
  x_raw ~ normal(0,1);
  
  // Random effects: Non-centered parameterization
  r_raw_mode ~ normal(0, s_h_mode);
  r_raw_occasion ~ normal(0, s_h_occasion);

  // Normal model for speed observations with log-transformed Z_obs
  speed_obs ~ normal(a + b * log(Z_obs) + x[obs_time] + r_mode[obs_mode] + r_occasion[obs_occasion], s_v);
}

generated quantities {
  vector[N_obs] log_lik;  // Log-likelihood for WAIC computation
  vector[T] estimated_speed;
  for (n in 1:N_obs) {
    log_lik[n] = normal_lpdf(speed_obs[n] | a + b * log(Z_obs[n]) + x[obs_time[n]] + r_mode[obs_mode[n]] + r_occasion[obs_occasion[n]], s_v);
  }
  
  for (t in 1:T) {
    estimated_speed[t] = normal_rng(a + b * log(Z_obs[t]) + x[t] + r_mode[obs_mode[t]] + r_occasion[obs_occasion[t]], s_v);  // Estimated speed for the full time range
  }
}

