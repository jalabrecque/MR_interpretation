
# 
# lifetime_effect_at_50 <- 2
# GbyE_shape <- "unif"
# exp_window_shape <- "recent"


MR_longitudinal_sim_plot <- function(GbyE_shape, exp_window_shape) {
  
  age <- seq(0, 50, length.out = 1001)

  lifetime_effect_at_50=2
  
GbyE_fn <- function(age, GbyE_shape) {
  
  if (GbyE_shape=="unif") {
    return(rep(0.55, times = length(age)))
  } else if (GbyE_shape=="FTO") {
    return(dnorm(x = age, mean = 25, sd = 10)*13.1 + 0.2538) # last number used to be 0.2538
  } else if (GbyE_shape=="incr") {
    return(0.1+0.5*age/50)
  } else if (GbyE_shape=="decr") {
    return(0.6-0.5*age/50)
  } else {
    stop("Shape argument for GbyE_fn must be either unif, FTO, incr or decr")
  }
  
}

lapply(c("unif","FTO","incr","decr"), FUN=function(x) {
  return(c(trapz(age, GbyE_fn(age = age, GbyE_shape = x)),
           mean(GbyE_fn(age = age, GbyE_shape = x))))
})

exp_window_fn <- function(age, exp_window_shape) {
  
  if (exp_window_shape=="unif") {
    return(rep(1, times = length(age)))
  } else if (exp_window_shape=="recent") {
    return(dnorm(x = age, mean = 50, sd = 10))
  } else if (exp_window_shape=="critical") {
    return(dnorm(x = age, mean = 14, sd = 2)) # mean used to be 25
  } else if (exp_window_shape=="incr") {
    return(age/50)
  } else {
    stop("Shape argument for exp_window_fn must be either unif, recent, critical or incr")
  }
  
}

# Create age variable
age <- seq(from = 0 , to = 50, length.out = 1001)

# Create BMI difference
BMI_diff <- GbyE_fn(age = age, GbyE_shape = GbyE_shape)

# Create exposure window
exp_window_wts_unscaled <- exp_window_fn(age = age, exp_window_shape = exp_window_shape)

par()
plot(0, type = "n", xlim = c(0,50), ylim = c(0,1))
if (exp_window_shape=="recent") lines((age-20)[age-20>=0], (exp_window_wts_unscaled/ifelse(exp_window_shape=="unif",2,max(exp_window_wts_unscaled)))[age>=20], col = "gray", lwd=2, lty=3)
lines(age, exp_window_wts_unscaled/ifelse(exp_window_shape=="unif",2,max(exp_window_wts_unscaled)), col = "black", lwd=2, lty=3)
lines(age, BMI_diff, col = "black", lwd = 2)



}










