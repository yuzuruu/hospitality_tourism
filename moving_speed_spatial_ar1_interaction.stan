// accessible transportation
// 5th. September 2025
// by Yuzuru Utsunomiya, Ph.D.
// (Faculty of Economics, Nagasaki University)

// data
data {
  int<lower=1> T;
  int<lower=1> N_obs;
  int<lower=1> N_miss;

  vector[N_obs] speed_obs;
  array[N_obs] int<lower=1> obs_idx;
  array[N_miss] int<lower=1> miss_idx;

  int<lower=1> N_obs_lon;
  int<lower=1> N_miss_lon;
  array[N_obs_lon] int<lower=1> obs_idx_lon;
  array[N_miss_lon] int<lower=1> miss_idx_lon;
  vector[N_obs_lon] lon_obs;

  int<lower=1> N_obs_lat;
  int<lower=1> N_miss_lat;
  array[N_obs_lat] int<lower=1> obs_idx_lat;
  array[N_miss_lat] int<lower=1> miss_idx_lat;
  vector[N_obs_lat] lat_obs;

  vector[T] N_car;
  vector[T] N_person;
  vector[T] N_motorcycle;

  array[T] int<lower=1> mode_id;
  array[T] int<lower=1> occasion_id;

  int<lower=1> J_mode;
  int<lower=1> J_occasion;
}

// parameter
parameters {
  vector[N_miss] speed_miss;
  vector[N_miss_lon] lon_miss;
  vector[N_miss_lat] lat_miss;

  real a;

  vector[J_mode] beta_car_by_mode;
  real beta_person;
  real beta_motorcycle;

  vector[J_mode] alpha_mode;
  vector[J_occasion] alpha_occasion;

  vector[2] z_beta;
  vector<lower=0>[2] lambda_beta;
  real<lower=0> tau_beta;

  real<lower=0> sigma;
  real<lower=0> sigma_x;
  real<lower=-1,upper=1> phi1;

  vector[T] x_raw;
}

// transformed parameter
transformed parameters {
  vector[T] speed;
  vector[T] lon;
  vector[T] lat;
  vector[T] x;
  vector[2] beta;

  for (n in 1:N_obs)
    speed[obs_idx[n]] = speed_obs[n];
  for (n in 1:N_miss)
    speed[miss_idx[n]] = speed_miss[n];

  for (n in 1:N_obs_lon)
    lon[obs_idx_lon[n]] = lon_obs[n];
  for (n in 1:N_miss_lon)
    lon[miss_idx_lon[n]] = lon_miss[n];

  for (n in 1:N_obs_lat)
    lat[obs_idx_lat[n]] = lat_obs[n];
  for (n in 1:N_miss_lat)
    lat[miss_idx_lat[n]] = lat_miss[n];

  beta = tau_beta * lambda_beta .* z_beta;

  x[1] = x_raw[1] * sigma_x;
  for (t in 2:T)
    x[t] = phi1 * x[t - 1] + x_raw[t] * sigma_x;
}

// model
model {
  a ~ normal(2, 1);

  z_beta ~ normal(0, 1);
  lambda_beta ~ cauchy(0, 1);
  tau_beta ~ cauchy(0, 1);

  beta_car_by_mode ~ normal(0, 1);
  beta_person ~ normal(0, 1);
  beta_motorcycle ~ normal(0, 1);

  alpha_mode ~ normal(0, 1);
  alpha_occasion ~ normal(0, 1);

  lon_miss ~ normal(0, 1);
  lat_miss ~ normal(0, 1);

  sigma ~ normal(0, 1);
  sigma_x ~ normal(0, 1);
  phi1 ~ normal(0, 0.5);
  x_raw ~ normal(0, 1);

  for (t in 1:T) {
    real log_car = log(fmax(N_car[t], 1e-3));
    real log_person = log(fmax(N_person[t], 1e-3));
    real log_motorcycle = log(fmax(N_motorcycle[t], 1e-3));

    target += student_t_lpdf(speed[t] | 4,
      a + x[t]
      + beta_car_by_mode[mode_id[t]] * log_car
      + beta_person * log_person
      + beta_motorcycle * log_motorcycle
      + beta[1] * lon[t]
      + beta[2] * lat[t]
      + alpha_mode[mode_id[t]]
      + alpha_occasion[occasion_id[t]],
      sigma);
  }
}

// generated quantities
generated quantities {
  vector[N_obs] log_lik;
  for (n in 1:N_obs) {
    real log_car = log(fmax(N_car[obs_idx[n]], 1e-3));
    real log_person = log(fmax(N_person[obs_idx[n]], 1e-3));
    real log_motorcycle = log(fmax(N_motorcycle[obs_idx[n]], 1e-3));

    log_lik[n] = student_t_lpdf(speed_obs[n] | 4,
      a + x[obs_idx[n]]
      + beta_car_by_mode[mode_id[obs_idx[n]]] * log_car
      + beta_person * log_person
      + beta_motorcycle * log_motorcycle
      + beta[1] * lon[obs_idx[n]]
      + beta[2] * lat[obs_idx[n]]
      + alpha_mode[mode_id[obs_idx[n]]]
      + alpha_occasion[occasion_id[obs_idx[n]]],
      sigma);
  }
}
