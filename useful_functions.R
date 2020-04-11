e_sat <- function(T){
  
  T0    = 273.15
  Lv    = 2.5e6
  Rv    = 461
  
  e_sat = 611.2*exp(Lv/Rv*(1/T0-1/T))
  
}


q_sat <- function(p,T){
  
  T0    = 273.15
  Lv    = 2.5e6
  Rv    = 461
  Rd    = 287
  eps   = Rd/Rv
  
  e_sat = 611.2*exp(Lv/Rv*(1/T0-1/T))
  q_sat = eps*e_sat/(p-(1-eps)*e_sat)
  
}


dqsat_dT <- function(p,T){
  
  T0       = 273.15
  Lv       = 2.5e6
  Rv       = 461
  Rd       = 287
  eps      = Rd/Rv
  
  e_sat    = 611.2*exp(Lv/Rv*(1/T0-1/T))
  
  dqsat_dT = (L_v*e_sat/R_v/T^2)*eps*p/(p-(1-eps)*e_sat)^2
  
}

# find alpha
a_eq <- function(n_iso, Temp) {
if (n_iso == 1) {          # % Deuterium
  a1 = 24884
  a2 = -76.248
  a3 = 0.052612
  }
if (n_iso == 2) {          # % 180
  a1 = 1137
  a2 = -0.4156
  a3 = -0.0020667
  }
  a_eq = exp((a1 + Temp * (a2 + Temp * a3))/(Temp*Temp))
}
