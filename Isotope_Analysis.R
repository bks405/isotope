## UROP Hawai'i Isotope Analysis Program
## Authors: Britt Seifert and Eleanore Law
## Latest Update: March 9, 2020

## working directory
setwd("~/Documents/UROP")

## packages

## constants
g       = 9.81    # m/s^2
R_d     = 287     # J/kgK
R_v     = 461     # J/kgK
gamma_d = 0.0098  # K/m
e       = 0.0014  # kg/kg

# input values
given_q = 0.014   # kg/kg
given_z = 1500    # m

# define variables as vectors
z       = array(0,100)
Temp    = array(0,100)
P       = array(0,100)
e_star  = array(0,100)
r_star  = array(0,100)
q_star  = array(0,100)

## define starting values
z[1]    = 10      # m
dz      = 20      # m
Temp[1] = 300     # K
P[1]    = 101500  # Pa

# loop
for (i in 1:99)
{
  z[i+1]    = z[i] + dz
  Temp[i+1] = Temp[i] - gamma_d * dz
  P[i+1]    = P[i] - (P[i] / (R_d * Temp[i])) * g * dz
  
  e_star[i] = 611.2*exp((17.67*(Temp[i]-273.15))/((Temp[i]-273.15)+243.5))
  r_star[i] = (R_d/R_v)*(e_star[i]/(P[i]-e_star[i]))
  q_star[i] = r_star[i]/(1+r_star[i])
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
curve_q  = 0   # units??
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

## CURRENTLY IN PROGRESS ##
# convergence to find q_l and q_v after temp fluctuations
i = 1       # dummy variable? not sure if need to use i or k
dq_l = 0    # condensation rate
dTemp = 0   # temperature change due to condensation

dq_l[i] = q_v[i] - q_star[i]

while (abs(dTemp) > 0.01)
{
  dTemp = (L_v/C_p) * dq_l[i]
  Temp_new[i] = Temp[i] + dTemp
  q_star[Temp[i]] = q_star[Temp_new[i]]
  dq_l_new = q_v[i] - q_star[Temp[i]]
}

# fun plots
plot(P, z, xlim=c(1000, 100000))
lines(e_star, z, type = "p")

plot(P,z)

plot(e_star,z)
plot(r_star,z)
plot(q_star,z, type = "l")
