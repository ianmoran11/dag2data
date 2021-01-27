rm(list = ls())

# Constant parameters ----------------------------------------------------------

b_epsilon.t1_y.t1   = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_i.t0_x.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_l.t1_y.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_omega.t0_i.t1     = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_omega.t0_omega.t1 = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_omega.t1_y.t1     = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_x.t0_x.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_x.t1_i.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_x.t1_l.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_x.t1_y.t1         = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_xi.t1_l.t1        = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
b_xi.t1_omega.t1    = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
i.t0                = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
omega.t0            = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
x.t0                = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)

# Each period - Exogenous ------------------------------------------------------

epsilon.t1          = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)
xi.t1               = 1.00 %>%  rnorm(n = 1.00, mean = . ,sd = 0.00)

# Each period - Endogenous -----------------------------------------------------

x.t1     = b_x.t0_x.t1*x.t0              + b_i.t0_x.t1*i.t0                                                                             %>%  rnorm(n = 1.00, mean = ., sd = 0.00)
i.t1     = b_x.t1_i.t1*x.t1              + b_omega.t0_i.t1*omega.t0                                                                     %>%  rnorm(n = 1.00, mean = ., sd = 0.00)
l.t1     = b_xi.t1_l.t1*xi.t1            + b_x.t1_l.t1*x.t1                                                                             %>%  rnorm(n = 1.00, mean = ., sd = 0.00)
omega.t1 = b_omega.t0_omega.t1*omega.t0  + b_xi.t1_omega.t1*xi.t1                                                                       %>%  rnorm(n = 1.00, mean = ., sd = 0.00)
y.t1     = b_l.t1_y.t1*l.t1              + b_x.t1_y.t1*x.t1              + b_omega.t1_y.t1*omega.t1      + b_epsilon.t1_y.t1*epsilon.t1 %>%  rnorm(n = 1.00, mean = ., sd = 0.00)

# Transition to next period -----------------------------------------------------

i.t0 = i.t1
omega.t0 = omega.t1
x.t0 = x.t1
