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
  
  Temp_old = Temp[i]
  q_star_old = q_star[i]
  dq_l_old = q_l
  dTemp_old = 0
  
  dTemp_new = 1000  # to start while loop
  
  while (abs(dTemp_new) > 0.01)
  {
    dTemp_new = (L_v/C_p) * dq_l_old
    Temp_new = Temp_old + dTemp_new
    
    e_star_new = 611.2*exp((17.67*(Temp_new-273.15))/((Temp_new-273.15)+243.5))
    r_star_new = (R_d/R_v)*(e_star_new/(P[i]-e_star_new))
    q_star_new = r_star_new/(1+r_star_new)
    
    dq_l_new = q_star_old - q_star_new
    
    Temp_old = Temp_new
    dTemp_old = dTemp_new
    q_star_old = q_star_new
    dq_l_old = dq_l_new
  }
  
}

i = 100
e_star[i] = 611.2*exp((17.67*(Temp[i]-273.15))/((Temp[i]-273.15)+243.5))
r_star[i] = (R_d/R_v)*(e_star[i]/(P[i]-e_star[i]))
q_star[i] = r_star[i]/(1+r_star[i])

# finds q_star altitude for given q
q = 0
i = 100   # index value reset
while (q < given_q)
{
  q = q_star[i]
  i = i - 1
}
z_star   = z[i]      # m altitude on curve of given_q

# finds curve_q for given z
i        = 1   # index value reset
curve_z  = 0   # m holder value
curve_q  = 0   # g/kg
while (curve_z < given_z)
{
  curve_z = z[i]
  i = i + 1
}

curve_q = q_star[i]  # point on curve for given altitude

# partition
q_l = 0
q_v = 0

if (given_z < z_star) 
{
  q_l = 0
  q_v = given_q 
} else 
    { 
      q_l = given_q - curve_q
      q_v = given_q - q_l
    }

# fun plots
plot(P, z, xlim=c(1000, 100000))
lines(e_star, z, type = "p")

plot(P,z)

plot(e_star,z)
plot(r_star,z)
plot(q_star,z, type = "l")
