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

parameters {
  vector[N_miss] speed_miss;
  vector[N_miss_lon] lon_miss;
  vector[N_miss_lat] lat_miss;

  real<lower=0> a;
  vector[T] beta_lon_raw;
  vector[T] beta_lat_raw;

  matrix[J_mode, T] beta_car_by_mode_raw;
  vector[T] beta_person_raw;
  vector[T] beta_motorcycle_raw;

  vector[J_mode] alpha_mode;
  vector[J_occasion] alpha_occasion;

  real<lower=0> sigma;
  real<lower=0> sigma_mode;
  real<lower=0> sigma_occasion;

  real<lower=0> sigma_beta;  // controls smoothness of time-varying effects
}

transformed parameters {
  vector[T] speed;
  vector[T] lon;
  vector[T] lat;

  matrix[J_mode, T] beta_car_by_mode;
  vector[T] beta_person;
  vector[T] beta_motorcycle;
  vector[T] beta_lon;
  vector[T] beta_lat;

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

  beta_car_by_mode = beta_car_by_mode_raw;
  beta_person = beta_person_raw;
  beta_motorcycle = beta_motorcycle_raw;
  beta_lon = beta_lon_raw;
  beta_lat = beta_lat_raw;
}

model {
  a ~ normal(2, 1);
  sigma ~ normal(1, 0.5);
  sigma_mode ~ normal(0, 1);
  sigma_occasion ~ normal(0, 1);
  sigma_beta ~ normal(0, 1);

  lon_miss ~ normal(0, 1);
  lat_miss ~ normal(0, 1);

  alpha_mode ~ normal(0, sigma_mode);
  alpha_occasion ~ normal(0, sigma_occasion);

  beta_car_by_mode_raw[,1] ~ normal(1, 0.5);
  beta_person_raw[1] ~ normal(1, 0.5);
  beta_motorcycle_raw[1] ~ normal(1, 0.5);
  beta_lon_raw[1] ~ normal(0, 1);
  beta_lat_raw[1] ~ normal(0, 1);

  for (t in 2:T) {
    beta_car_by_mode_raw[,t] ~ normal(beta_car_by_mode_raw[,t-1], sigma_beta);
    beta_person_raw[t] ~ normal(beta_person_raw[t-1], sigma_beta);
    beta_motorcycle_raw[t] ~ normal(beta_motorcycle_raw[t-1], sigma_beta);
    beta_lon_raw[t] ~ normal(beta_lon_raw[t-1], sigma_beta);
    beta_lat_raw[t] ~ normal(beta_lat_raw[t-1], sigma_beta);
  }

  for (t in 1:T) {
    real log_car = log(fmax(N_car[t], 1e-3));
    real log_person = log(fmax(N_person[t], 1e-3));
    real log_motorcycle = log(fmax(N_motorcycle[t], 1e-3));

    speed[t] ~ normal(
      a
      - beta_car_by_mode[mode_id[t], t] * log_car
      - beta_person[t] * log_person
      - beta_motorcycle[t] * log_motorcycle
      + beta_lon[t] * lon[t]
      + beta_lat[t] * lat[t]
      + alpha_mode[mode_id[t]]
      + alpha_occasion[occasion_id[t]],
      sigma
    );
  }
}

generated quantities {
  vector[N_obs] log_lik;
  for (n in 1:N_obs) {
    real log_car = log(fmax(N_car[obs_idx[n]], 1e-3));
    real log_person = log(fmax(N_person[obs_idx[n]], 1e-3));
    real log_motorcycle = log(fmax(N_motorcycle[obs_idx[n]], 1e-3));

    log_lik[n] = normal_lpdf(speed_obs[n] |
      a
      - beta_car_by_mode[mode_id[obs_idx[n]], obs_idx[n]] * log_car
      - beta_person[obs_idx[n]] * log_person
      - beta_motorcycle[obs_idx[n]] * log_motorcycle
      + beta_lon[obs_idx[n]] * lon[obs_idx[n]]
      + beta_lat[obs_idx[n]] * lat[obs_idx[n]]
      + alpha_mode[mode_id[obs_idx[n]]]
      + alpha_occasion[occasion_id[obs_idx[n]]],
      sigma
    );
  }
}

