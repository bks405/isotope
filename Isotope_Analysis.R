## UROP Hawai'i Isotope Analysis Program
## Authors: Britt Seifert and Eleanore Law
## Latest Update: April 10, 2020

## working directory
## setwd("~/Documents/UROP")

## packages

## constants
Nz      = 200
g       = 9.81    # m/s^2
R_d     = 287     # J/kgK
R_v     = 461     # J/kgK
eps     = R_d/R_v
gamma_d = 0.0098  # K/m
e       = 0.0014  # kg/kg
L_v     = 2.5e6   # J/kg
C_p     = 1005    # J/kgK


## define variables as vectors
z       = array(0,Nz)
Temp    = array(0,Nz)
P       = array(0,Nz)
qv      = array(0,Nz)
ql      = array(0,Nz)


## define starting values
z[1]    = 10      # m
dz      = 20      # m
Temp[1] = 300     # K
P[1]    = 101500  # Pa
qv[1]   = 0.014   # kg/kg
ql[1]   = 0.00    # kg/kg
tmp     = 0


## loop
for(i in 1:(Nz-1)){
	
	z[i+1]    = z[i]+dz
	Temp[i+1] = Temp[i]-gamma_d*dz
	P[i+1]    = P[i]-(P[i]/(R_d*Temp[i+1]))*g*dz
	qs_i      = q_sat(P[i+1],Temp[i+1])
	qv[i+1]   = qv[i]
	
	if (qv[i+1] > qs_i) {
    	
    	tmp  = 0
    	Tp_i = Temp[i+1] 		   
    	dTp  = 100
    	
    	while(abs(dTp) > 1e-6 && tmp < 100){
    		   		
    		tmp  = tmp + 1
    		
			fun  = Tp_i - Temp[i+1] - L_v/C_p*(qv[i+1]-q_sat(P[i+1],Tp_i))
			der  = 1 + L_v/C_p*dqsat_dT(P[i+1],Tp_i)
			 
			dTp  = -fun/der
			Tp_i = Tp_i+dTp
    		
    	}
    
    	Temp[i+1] = Tp_i
     	ql[i+1]   = qv[i+1]-q_sat(P[i+1],Temp[i+1]) 	
    	qv[i+1]   = q_sat(P[i+1],Temp[i+1]) 
    
    }
  
}

