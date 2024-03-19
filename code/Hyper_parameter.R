
## Default alpha 
alpha=0.5

## theoretical value psychometric function
# Default value
##  sd| delta | lapsing rates (pl) | guessing rates (pg) | mu( log(theta,10)  )
PFpar = c(0.5, 0, 0,-1.5) 



## theoretical value of response confidence parameter
## default value
## sigma cutoff 1~4 
## confidence size
ConfidentPar=c(0.1,0.2,0.4,0.6,0.8)



## theoretical value of response time parameter
## default value

## shifted parameter | Pieron law K |
#  ratio of RT sd mean | Pieron law beta | right censoring 
RTpar = c(150, 350, 0.22, 0.3,1)


# option for visualization

# Title name
# range of the displayed trials
# upper bound and lower bound of the plot
Tbook <- c(
  r"( size: 0.5 initial: $\theta-3\sigma$ )",
  r"(size:  0.5 initial: $\theta-2\sigma$)",
  r"(size: 0.5 initial: $\theta-1\sigma$)",
  r"(size: 0.5 initial: $\theta+1\sigma$)",
  r"(size: 0.5 initial: $\theta+2\sigma$)",
  r"(size: 0.5 initial: $\theta+3\sigma$)",
  r"(size: 1  initial: $\theta-3\sigma$)",
  r"(size: 1 initial: $\theta-2\sigma$)",
  r"(size: 1 initial: $\theta-1\sigma$)",
  r"(size: 1 initial: $\theta+1\sigma$)",
  r"(size: 1 initial: $\theta+2\sigma$)",
  r"(size: 1  initial: $\theta+3\sigma$)",
  r"(size: 1.5 initial: $\theta-3\sigma$)",
  r"(size: 1.5 initial: $\theta-2\sigma$)",
  r"(size: 1.5 initial: $\theta-1\sigma$)",
  r"(size: 1.5 initial: $\theta+1\sigma$)",
  r"(size: 1.5 initial: $\theta+2\sigma$)",
  r"(size: 1.5 initial: $\theta+3\sigma$)"
)
# trials
T_set <- c(2:15)
# plot limits
B_L <- -0.01
B_U <- 0.01
S_L <- 0
S_U <- 0.06
M_L <- 0
M_U <- 0.007
