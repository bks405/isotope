## UROP Hawai'i Isotope Analysis Program
## Authors: Britt Seifert and Eleanore Law
## Latest Update: April 1, 2020

## working directory
setwd("~/Documents/UROP")

## packages

## constants
g       = 9.81    # m/s^2
R_d     = 287     # J/kgK
R_v     = 461     # J/kgK
gamma_d = 0.0098  # K/m
e       = 0.0014  # kg/kg
L_v     = 2.5e6   # J/kg
C_p     = 1005    # J/kgK

# input values
given_q = 0.014   # kg/kg
given_z = 1500    # m

# define variables as vectors
z       = array(0,100)
Temp    = array(0,100)
P       = array(0,100)
q_v     = array(0,100)
q_l     = array(0,100)
e_star  = array(0,100)
r_star  = array(0,100)
q_star  = array(0,100)

## define starting values
z[1]    = 10      # m
dz      = 20      # m
Temp[1] = 300     # K
P[1]    = 101500  # Pa
q_v[1]  = 0.014   # g/kg
q_l[1]  = 0       # g/kg
Tmp     = 0

# loop
for (i in 1:99)
{
  z[i+1]    = z[i] + dz
  Temp[i+1] = Temp[i] - gamma_d * dz
  P[i+1]    = P[i] - (P[i] / (R_d * Temp[i])) * g * dz
  q_v[i+1]  = q_v[i]
  
  e_star_new = 611.2*exp((17.67*(Temp[i+1]-273.15))/((Temp[i+1]-273.15)+243.5))
  r_star_new = (R_d/R_v)*(e_star_new/(P[i+1]-e_star_new))
  q_star_new = r_star_new/(1+r_star_new)
  
  if (q_v[i+1] > q_star_new) 
    {
      Temp_old = Temp[i+1]
      q_star_old = q_star_new
      dq_l_old = q_v[i+1] - q_star_new
      dTemp_old = 0
      q_l_old = 0 + dq_l_old
  
      dTemp_new = 1000  # to start while loop
    
      while (abs(dTemp_new) > 0.01 & Tmp < 100)
      {
        Tmp = Tmp + 1
        
        dTemp_new = (L_v/C_p) * dq_l_old
        Temp_new = Temp_old + dTemp_new
    
        e_star_new = 611.2*exp((17.67*(Temp_new-273.15))/((Temp_new-273.15)+243.5))
        r_star_new = (R_d/R_v)*(e_star_new/(P[i+1]-e_star_new))
        q_star_new = r_star_new/(1+r_star_new)
    
        dq_l_new = q_star_old - q_star_new
        q_l_new = q_l_old - dq_l_new
    
        print(q_star_new)
        
        Temp_old = Temp_new
        dTemp_old = dTemp_new
        q_star_old = q_star_new
        dq_l_old = dq_l_new
      }
      
      Temp[i+1] = Temp_new
      q_l[i+1]  = q_l_new
      q_v[i+1]  = q_v[i+1] - q_l[i+1]
      
    }
}
