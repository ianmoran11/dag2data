rm(list = ls())

# Constant parameters ----------------------------------------------------------

b_epsilon.t1_y.t1   = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Error term
b_i.t0_x.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # investment to capital
b_l.t1_y.t1         = 0.30 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Return on labour
b_omega.t0_i.t1     = 0.10 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # increased investement from productivity
b_omega.t0_omega.t1 = 0.80 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # productivity persistence
b_omega.t1_y.t1     = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Prodcutviity to output
b_x.t0_x.t1         = 0.90 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Depreciation
b_x.t1_i.t1         = 0.10 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Investement rate (of capital)
b_x.t1_l.t1         = 1.20 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Capital to labour ratio
b_x.t1_y.t1         = 0.30 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Return on capital 
b_xi.t1_l.t1        = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # increase labour from xi shock
b_xi.t1_omega.t1    = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00) # Increase productivity from capital

omega.t0            = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.10) # productivity distribution
x.t0                = 100000*rgamma(n = 1.00,shape = 1.018, rate = 0.0051) # Firm size (employment distribution)
i.t0                = .10*x.t0 %>% rnorm(n = 1.00, mean = . ,sd = 0.025)

# Each period - Exogenous ------------------------------------------------------

epsilon.t1          = 0.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.15)
xi.t1               = 0.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.15)

# Each period - Endogenous -----------------------------------------------------

x.t1     = b_x.t0_x.t1*x.t0              + b_i.t0_x.t1*i.t0                                                                             %>%  rnorm(n = 1.00, mean = ., sd = 0.10)
i.t1     = b_x.t1_i.t1*x.t1              + b_omega.t0_i.t1*omega.t0                                                                     %>%  rnorm(n = 1.00, mean = ., sd = 0.10)
l.t1     = b_xi.t1_l.t1*xi.t1            + b_x.t1_l.t1*x.t1                                                                             %>%  rnorm(n = 1.00, mean = ., sd = 0.10)
omega.t1 = b_omega.t0_omega.t1*omega.t0  + b_xi.t1_omega.t1*xi.t1                                                                       %>%  rnorm(n = 1.00, mean = ., sd = 0.10)
y.t1     = b_l.t1_y.t1*l.t1              + b_x.t1_y.t1*x.t1              + b_omega.t1_y.t1*omega.t1      + b_epsilon.t1_y.t1*epsilon.t1 %>%  rnorm(n = 1.00, mean = ., sd = 0.10)

# Transition to next period -----------------------------------------------------

i.t0 = i.t1
omega.t0 = omega.t1
x.t0 = x.t1
