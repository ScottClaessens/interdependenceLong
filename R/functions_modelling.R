# custom functions

# model without controls
fitRICLPM1 <- function(d, constrained = FALSE) {
  # model code for riclpm
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*PFIsharedfateNeigh.1 + 1*PFIsharedfateNeigh.2 + 1*PFIsharedfateNeigh.3 + 1*PFIsharedfateNeigh.4 + 1*PFIsharedfateNeigh.5 + 1*PFIsharedfateNeigh.6 + 1*PFIsharedfateNeigh.7 + 1*PFIsharedfateNeigh.8 + 1*PFIsharedfateNeigh.9 + 1*PFIsharedfateNeigh.10 + 1*PFIsharedfateNeigh.11 + 1*PFIsharedfateNeigh.12 + 1*PFIsharedfateNeigh.13 + 1*PFIsharedfateNeigh.15 +1*PFIsharedfateNeigh.18
            ri2 =~ 1*HelpGiven.1 + 1*HelpGiven.2 + 1*HelpGiven.3 + 1*HelpGiven.4 + 1*HelpGiven.5 + 1*HelpGiven.6 + 1*HelpGiven.7 + 1*HelpGiven.8 + 1*HelpGiven.9 + 1*HelpGiven.10 + 1*HelpGiven.11 + 1*HelpGiven.12 + 1*HelpGiven.13 + 1*HelpGiven.15 +1*HelpGiven.18
            
            # Create within-person centered variables
            w1_01 =~ 1*PFIsharedfateNeigh.1
            w1_02 =~ 1*PFIsharedfateNeigh.2
            w1_03 =~ 1*PFIsharedfateNeigh.3
            w1_04 =~ 1*PFIsharedfateNeigh.4
            w1_05 =~ 1*PFIsharedfateNeigh.5
            w1_06 =~ 1*PFIsharedfateNeigh.6
            w1_07 =~ 1*PFIsharedfateNeigh.7
            w1_08 =~ 1*PFIsharedfateNeigh.8
            w1_09 =~ 1*PFIsharedfateNeigh.9
            w1_10 =~ 1*PFIsharedfateNeigh.10
            w1_11 =~ 1*PFIsharedfateNeigh.11
            w1_12 =~ 1*PFIsharedfateNeigh.12
            w1_13 =~ 1*PFIsharedfateNeigh.13
            w1_15 =~ 1*PFIsharedfateNeigh.15
            w1_18 =~ 1*PFIsharedfateNeigh.18
            
            w2_01 =~ 1*HelpGiven.1
            w2_02 =~ 1*HelpGiven.2
            w2_03 =~ 1*HelpGiven.3
            w2_04 =~ 1*HelpGiven.4
            w2_05 =~ 1*HelpGiven.5
            w2_06 =~ 1*HelpGiven.6
            w2_07 =~ 1*HelpGiven.7
            w2_08 =~ 1*HelpGiven.8
            w2_09 =~ 1*HelpGiven.9
            w2_10 =~ 1*HelpGiven.10
            w2_11 =~ 1*HelpGiven.11
            w2_12 =~ 1*HelpGiven.12
            w2_13 =~ 1*HelpGiven.13
            w2_15 =~ 1*HelpGiven.15
            w2_18 =~ 1*HelpGiven.18
            
            # Estimate the lagged effects between the within-person centered variables
            w1_02 ~ b11*w1_01 + b12*w2_01
            w1_03 ~ b11*w1_02 + b12*w2_02
            w1_04 ~ b11*w1_03 + b12*w2_03
            w1_05 ~ b11*w1_04 + b12*w2_04
            w1_06 ~ b11*w1_05 + b12*w2_05
            w1_07 ~ b11*w1_06 + b12*w2_06
            w1_08 ~ b11*w1_07 + b12*w2_07
            w1_09 ~ b11*w1_08 + b12*w2_08
            w1_10 ~ b11*w1_09 + b12*w2_09
            w1_11 ~ b11*w1_10 + b12*w2_10
            w1_12 ~ b11*w1_11 + b12*w2_11
            w1_13 ~ b11*w1_12 + b12*w2_12
            w1_15 ~ b11*w1_13 + b12*w2_13
            w1_18 ~ b11*w1_15 + b12*w2_15
            
            w2_02 ~ b21*w1_01 + b22*w2_01
            w2_03 ~ b21*w1_02 + b22*w2_02
            w2_04 ~ b21*w1_03 + b22*w2_03
            w2_05 ~ b21*w1_04 + b22*w2_04
            w2_06 ~ b21*w1_05 + b22*w2_05
            w2_07 ~ b21*w1_06 + b22*w2_06
            w2_08 ~ b21*w1_07 + b22*w2_07
            w2_09 ~ b21*w1_08 + b22*w2_08
            w2_10 ~ b21*w1_09 + b22*w2_09
            w2_11 ~ b21*w1_10 + b22*w2_10
            w2_12 ~ b21*w1_11 + b22*w2_11
            w2_13 ~ b21*w1_12 + b22*w2_12
            w2_15 ~ b21*w1_13 + b22*w2_13
            w2_18 ~ b21*w1_15 + b22*w2_15
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_01 ~~ w2_01
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_02 ~~ cov12*w2_02
            w1_03 ~~ cov12*w2_03
            w1_04 ~~ cov12*w2_04
            w1_05 ~~ cov12*w2_05
            w1_06 ~~ cov12*w2_06
            w1_07 ~~ cov12*w2_07
            w1_08 ~~ cov12*w2_08
            w1_09 ~~ cov12*w2_09
            w1_10 ~~ cov12*w2_10
            w1_11 ~~ cov12*w2_11
            w1_12 ~~ cov12*w2_12
            w1_13 ~~ cov12*w2_13
            w1_15 ~~ cov12*w2_15
            w1_18 ~~ cov12*w2_18
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri1 ~~ ri2
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_01 ~~ w1_01
            w1_02 ~~ var1*w1_02
            w1_03 ~~ var1*w1_03
            w1_04 ~~ var1*w1_04
            w1_05 ~~ var1*w1_05
            w1_06 ~~ var1*w1_06
            w1_07 ~~ var1*w1_07
            w1_08 ~~ var1*w1_08
            w1_09 ~~ var1*w1_09
            w1_10 ~~ var1*w1_10
            w1_11 ~~ var1*w1_11
            w1_12 ~~ var1*w1_12
            w1_13 ~~ var1*w1_13
            w1_15 ~~ var1*w1_15
            w1_18 ~~ var1*w1_18
            
            w2_01 ~~ w2_01
            w2_02 ~~ var2*w2_02
            w2_03 ~~ var2*w2_03
            w2_04 ~~ var2*w2_04
            w2_05 ~~ var2*w2_05
            w2_06 ~~ var2*w2_06
            w2_07 ~~ var2*w2_07
            w2_08 ~~ var2*w2_08
            w2_09 ~~ var2*w2_09
            w2_10 ~~ var2*w2_10
            w2_11 ~~ var2*w2_11
            w2_12 ~~ var2*w2_12
            w2_13 ~~ var2*w2_13
            w2_15 ~~ var2*w2_15
            w2_18 ~~ var2*w2_18
  
            # Estimate the means
            PFIsharedfateNeigh.1 + PFIsharedfateNeigh.2 + PFIsharedfateNeigh.3 + PFIsharedfateNeigh.4 + PFIsharedfateNeigh.5 + PFIsharedfateNeigh.6 + PFIsharedfateNeigh.7 + PFIsharedfateNeigh.8 + PFIsharedfateNeigh.9 + PFIsharedfateNeigh.10 + PFIsharedfateNeigh.11 + PFIsharedfateNeigh.12 + PFIsharedfateNeigh.13 + PFIsharedfateNeigh.15 + PFIsharedfateNeigh.18 ~ 1
            HelpGiven.1 + HelpGiven.2 + HelpGiven.3 + HelpGiven.4 + HelpGiven.5 + HelpGiven.6 + HelpGiven.7 + HelpGiven.8 + HelpGiven.9 + HelpGiven.10 + HelpGiven.11 + HelpGiven.12 + HelpGiven.13 + HelpGiven.15 + HelpGiven.18 ~ 1'
  # remove constraints for unconstrained model
  if (!constrained) {
    constraints <- c("b11","b12","b21","b22","cov12","var1","var2")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}

# model with controls
fitRICLPM2 <- function(d, constrained = FALSE) {
  # model code for riclpm
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*PFIsharedfateNeigh.1 + 1*PFIsharedfateNeigh.2 + 1*PFIsharedfateNeigh.3 + 1*PFIsharedfateNeigh.4 + 1*PFIsharedfateNeigh.5 + 1*PFIsharedfateNeigh.6 + 1*PFIsharedfateNeigh.7 + 1*PFIsharedfateNeigh.8 + 1*PFIsharedfateNeigh.9 + 1*PFIsharedfateNeigh.10 + 1*PFIsharedfateNeigh.11 + 1*PFIsharedfateNeigh.12 + 1*PFIsharedfateNeigh.13 + 1*PFIsharedfateNeigh.15 +1*PFIsharedfateNeigh.18
            ri2 =~ 1*HelpGiven.1 + 1*HelpGiven.2 + 1*HelpGiven.3 + 1*HelpGiven.4 + 1*HelpGiven.5 + 1*HelpGiven.6 + 1*HelpGiven.7 + 1*HelpGiven.8 + 1*HelpGiven.9 + 1*HelpGiven.10 + 1*HelpGiven.11 + 1*HelpGiven.12 + 1*HelpGiven.13 + 1*HelpGiven.15 +1*HelpGiven.18
            ri3 =~ 1*HelpReceived.1 + 1*HelpReceived.2 + 1*HelpReceived.3 + 1*HelpReceived.4 + 1*HelpReceived.5 + 1*HelpReceived.6 + 1*HelpReceived.7 + 1*HelpReceived.8 + 1*HelpReceived.9 + 1*HelpReceived.10 + 1*HelpReceived.11 + 1*HelpReceived.12 + 1*HelpReceived.13 + 1*HelpReceived.15 +1*HelpReceived.18
            
            # Create within-person centered variables
            w1_01 =~ 1*PFIsharedfateNeigh.1
            w1_02 =~ 1*PFIsharedfateNeigh.2
            w1_03 =~ 1*PFIsharedfateNeigh.3
            w1_04 =~ 1*PFIsharedfateNeigh.4
            w1_05 =~ 1*PFIsharedfateNeigh.5
            w1_06 =~ 1*PFIsharedfateNeigh.6
            w1_07 =~ 1*PFIsharedfateNeigh.7
            w1_08 =~ 1*PFIsharedfateNeigh.8
            w1_09 =~ 1*PFIsharedfateNeigh.9
            w1_10 =~ 1*PFIsharedfateNeigh.10
            w1_11 =~ 1*PFIsharedfateNeigh.11
            w1_12 =~ 1*PFIsharedfateNeigh.12
            w1_13 =~ 1*PFIsharedfateNeigh.13
            w1_15 =~ 1*PFIsharedfateNeigh.15
            w1_18 =~ 1*PFIsharedfateNeigh.18
            
            w2_01 =~ 1*HelpGiven.1
            w2_02 =~ 1*HelpGiven.2
            w2_03 =~ 1*HelpGiven.3
            w2_04 =~ 1*HelpGiven.4
            w2_05 =~ 1*HelpGiven.5
            w2_06 =~ 1*HelpGiven.6
            w2_07 =~ 1*HelpGiven.7
            w2_08 =~ 1*HelpGiven.8
            w2_09 =~ 1*HelpGiven.9
            w2_10 =~ 1*HelpGiven.10
            w2_11 =~ 1*HelpGiven.11
            w2_12 =~ 1*HelpGiven.12
            w2_13 =~ 1*HelpGiven.13
            w2_15 =~ 1*HelpGiven.15
            w2_18 =~ 1*HelpGiven.18
            
            w3_01 =~ 1*HelpReceived.1
            w3_02 =~ 1*HelpReceived.2
            w3_03 =~ 1*HelpReceived.3
            w3_04 =~ 1*HelpReceived.4
            w3_05 =~ 1*HelpReceived.5
            w3_06 =~ 1*HelpReceived.6
            w3_07 =~ 1*HelpReceived.7
            w3_08 =~ 1*HelpReceived.8
            w3_09 =~ 1*HelpReceived.9
            w3_10 =~ 1*HelpReceived.10
            w3_11 =~ 1*HelpReceived.11
            w3_12 =~ 1*HelpReceived.12
            w3_13 =~ 1*HelpReceived.13
            w3_15 =~ 1*HelpReceived.15
            w3_18 =~ 1*HelpReceived.18
            
            # Regression of observed variables on time-invariant controls
            PFIsharedfateNeigh.1 + PFIsharedfateNeigh.2 + PFIsharedfateNeigh.3 + PFIsharedfateNeigh.4 + PFIsharedfateNeigh.5 + PFIsharedfateNeigh.6 + PFIsharedfateNeigh.7 + PFIsharedfateNeigh.8 + PFIsharedfateNeigh.9 + PFIsharedfateNeigh.10 + PFIsharedfateNeigh.11 + PFIsharedfateNeigh.12 + PFIsharedfateNeigh.13 + PFIsharedfateNeigh.15 + PFIsharedfateNeigh.18 ~ Wealth.1 + TraitEmpathicConcern.1
            HelpGiven.1 + HelpGiven.2 + HelpGiven.3 + HelpGiven.4 + HelpGiven.5 + HelpGiven.6 + HelpGiven.7 + HelpGiven.8 + HelpGiven.9 + HelpGiven.10 + HelpGiven.11 + HelpGiven.12 + HelpGiven.13 + HelpGiven.15 + HelpGiven.18 ~ Wealth.1 + TraitEmpathicConcern.1
            HelpReceived.1 + HelpReceived.2 + HelpReceived.3 + HelpReceived.4 + HelpReceived.5 + HelpReceived.6 + HelpReceived.7 + HelpReceived.8 + HelpReceived.9 + HelpReceived.10 + HelpReceived.11 + HelpReceived.12 + HelpReceived.13 + HelpReceived.15 + HelpReceived.18 ~ Wealth.1 + TraitEmpathicConcern.1
            
            # Estimate the lagged effects between the within-person centered variables
            w1_02 ~ b11*w1_01 + b12*w2_01 + b13*w3_01
            w1_03 ~ b11*w1_02 + b12*w2_02 + b13*w3_02
            w1_04 ~ b11*w1_03 + b12*w2_03 + b13*w3_03
            w1_05 ~ b11*w1_04 + b12*w2_04 + b13*w3_04
            w1_06 ~ b11*w1_05 + b12*w2_05 + b13*w3_05
            w1_07 ~ b11*w1_06 + b12*w2_06 + b13*w3_06
            w1_08 ~ b11*w1_07 + b12*w2_07 + b13*w3_07
            w1_09 ~ b11*w1_08 + b12*w2_08 + b13*w3_08
            w1_10 ~ b11*w1_09 + b12*w2_09 + b13*w3_09
            w1_11 ~ b11*w1_10 + b12*w2_10 + b13*w3_10
            w1_12 ~ b11*w1_11 + b12*w2_11 + b13*w3_11
            w1_13 ~ b11*w1_12 + b12*w2_12 + b13*w3_12
            w1_15 ~ b11*w1_13 + b12*w2_13 + b13*w3_13
            w1_18 ~ b11*w1_15 + b12*w2_15 + b13*w3_15
            
            w2_02 ~ b21*w1_01 + b22*w2_01 + b23*w3_01
            w2_03 ~ b21*w1_02 + b22*w2_02 + b23*w3_02
            w2_04 ~ b21*w1_03 + b22*w2_03 + b23*w3_03
            w2_05 ~ b21*w1_04 + b22*w2_04 + b23*w3_04
            w2_06 ~ b21*w1_05 + b22*w2_05 + b23*w3_05
            w2_07 ~ b21*w1_06 + b22*w2_06 + b23*w3_06
            w2_08 ~ b21*w1_07 + b22*w2_07 + b23*w3_07
            w2_09 ~ b21*w1_08 + b22*w2_08 + b23*w3_08
            w2_10 ~ b21*w1_09 + b22*w2_09 + b23*w3_09
            w2_11 ~ b21*w1_10 + b22*w2_10 + b23*w3_10
            w2_12 ~ b21*w1_11 + b22*w2_11 + b23*w3_11
            w2_13 ~ b21*w1_12 + b22*w2_12 + b23*w3_12
            w2_15 ~ b21*w1_13 + b22*w2_13 + b23*w3_13
            w2_18 ~ b21*w1_15 + b22*w2_15 + b23*w3_15
            
            w3_02 ~ b31*w1_01 + b32*w2_01 + b33*w3_01
            w3_03 ~ b31*w1_02 + b32*w2_02 + b33*w3_02
            w3_04 ~ b31*w1_03 + b32*w2_03 + b33*w3_03
            w3_05 ~ b31*w1_04 + b32*w2_04 + b33*w3_04
            w3_06 ~ b31*w1_05 + b32*w2_05 + b33*w3_05
            w3_07 ~ b31*w1_06 + b32*w2_06 + b33*w3_06
            w3_08 ~ b31*w1_07 + b32*w2_07 + b33*w3_07
            w3_09 ~ b31*w1_08 + b32*w2_08 + b33*w3_08
            w3_10 ~ b31*w1_09 + b32*w2_09 + b33*w3_09
            w3_11 ~ b31*w1_10 + b32*w2_10 + b33*w3_10
            w3_12 ~ b31*w1_11 + b32*w2_11 + b33*w3_11
            w3_13 ~ b31*w1_12 + b32*w2_12 + b33*w3_12
            w3_15 ~ b31*w1_13 + b32*w2_13 + b33*w3_13
            w3_18 ~ b31*w1_15 + b32*w2_15 + b33*w3_15
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_01 ~~ w2_01
            w1_01 ~~ w3_01
            w2_01 ~~ w3_01
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_02 ~~ cov12*w2_02
            w1_03 ~~ cov12*w2_03
            w1_04 ~~ cov12*w2_04
            w1_05 ~~ cov12*w2_05
            w1_06 ~~ cov12*w2_06
            w1_07 ~~ cov12*w2_07
            w1_08 ~~ cov12*w2_08
            w1_09 ~~ cov12*w2_09
            w1_10 ~~ cov12*w2_10
            w1_11 ~~ cov12*w2_11
            w1_12 ~~ cov12*w2_12
            w1_13 ~~ cov12*w2_13
            w1_15 ~~ cov12*w2_15
            w1_18 ~~ cov12*w2_18
            
            w1_02 ~~ cov13*w3_02
            w1_03 ~~ cov13*w3_03
            w1_04 ~~ cov13*w3_04
            w1_05 ~~ cov13*w3_05
            w1_06 ~~ cov13*w3_06
            w1_07 ~~ cov13*w3_07
            w1_08 ~~ cov13*w3_08
            w1_09 ~~ cov13*w3_09
            w1_10 ~~ cov13*w3_10
            w1_11 ~~ cov13*w3_11
            w1_12 ~~ cov13*w3_12
            w1_13 ~~ cov13*w3_13
            w1_15 ~~ cov13*w3_15
            w1_18 ~~ cov13*w3_18
            
            w2_02 ~~ cov23*w3_02
            w2_03 ~~ cov23*w3_03
            w2_04 ~~ cov23*w3_04
            w2_05 ~~ cov23*w3_05
            w2_06 ~~ cov23*w3_06
            w2_07 ~~ cov23*w3_07
            w2_08 ~~ cov23*w3_08
            w2_09 ~~ cov23*w3_09
            w2_10 ~~ cov23*w3_10
            w2_11 ~~ cov23*w3_11
            w2_12 ~~ cov23*w3_12
            w2_13 ~~ cov23*w3_13
            w2_15 ~~ cov23*w3_15
            w2_18 ~~ cov23*w3_18
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri3 ~~ ri3
            
            ri1 ~~ ri2
            ri1 ~~ ri3
            ri2 ~~ ri3
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_01 ~~ w1_01
            w1_02 ~~ var1*w1_02
            w1_03 ~~ var1*w1_03
            w1_04 ~~ var1*w1_04
            w1_05 ~~ var1*w1_05
            w1_06 ~~ var1*w1_06
            w1_07 ~~ var1*w1_07
            w1_08 ~~ var1*w1_08
            w1_09 ~~ var1*w1_09
            w1_10 ~~ var1*w1_10
            w1_11 ~~ var1*w1_11
            w1_12 ~~ var1*w1_12
            w1_13 ~~ var1*w1_13
            w1_15 ~~ var1*w1_15
            w1_18 ~~ var1*w1_18
            
            w2_01 ~~ w2_01
            w2_02 ~~ var2*w2_02
            w2_03 ~~ var2*w2_03
            w2_04 ~~ var2*w2_04
            w2_05 ~~ var2*w2_05
            w2_06 ~~ var2*w2_06
            w2_07 ~~ var2*w2_07
            w2_08 ~~ var2*w2_08
            w2_09 ~~ var2*w2_09
            w2_10 ~~ var2*w2_10
            w2_11 ~~ var2*w2_11
            w2_12 ~~ var2*w2_12
            w2_13 ~~ var2*w2_13
            w2_15 ~~ var2*w2_15
            w2_18 ~~ var2*w2_18
            
            w3_01 ~~ w3_01
            w3_02 ~~ var3*w3_02
            w3_03 ~~ var3*w3_03
            w3_04 ~~ var3*w3_04
            w3_05 ~~ var3*w3_05
            w3_06 ~~ var3*w3_06
            w3_07 ~~ var3*w3_07
            w3_08 ~~ var3*w3_08
            w3_09 ~~ var3*w3_09
            w3_10 ~~ var3*w3_10
            w3_11 ~~ var3*w3_11
            w3_12 ~~ var3*w3_12
            w3_13 ~~ var3*w3_13
            w3_15 ~~ var3*w3_15
            w3_18 ~~ var3*w3_18
  
            # Estimate the means
            PFIsharedfateNeigh.1 + PFIsharedfateNeigh.2 + PFIsharedfateNeigh.3 + PFIsharedfateNeigh.4 + PFIsharedfateNeigh.5 + PFIsharedfateNeigh.6 + PFIsharedfateNeigh.7 + PFIsharedfateNeigh.8 + PFIsharedfateNeigh.9 + PFIsharedfateNeigh.10 + PFIsharedfateNeigh.11 + PFIsharedfateNeigh.12 + PFIsharedfateNeigh.13 + PFIsharedfateNeigh.15 + PFIsharedfateNeigh.18 ~ 1
            HelpGiven.1 + HelpGiven.2 + HelpGiven.3 + HelpGiven.4 + HelpGiven.5 + HelpGiven.6 + HelpGiven.7 + HelpGiven.8 + HelpGiven.9 + HelpGiven.10 + HelpGiven.11 + HelpGiven.12 + HelpGiven.13 + HelpGiven.15 + HelpGiven.18 ~ 1
            HelpReceived.1 + HelpReceived.2 + HelpReceived.3 + HelpReceived.4 + HelpReceived.5 + HelpReceived.6 + HelpReceived.7 + HelpReceived.8 + HelpReceived.9 + HelpReceived.10 + HelpReceived.11 + HelpReceived.12 + HelpReceived.13 + HelpReceived.15 + HelpReceived.18 ~ 1'
  # remove constraints for unconstrained model
  if (!constrained) {
    constraints <- c("b11","b12","b13","b21","b22","b23","b31","b32","b33",
                     "cov12","cov13","cov23","var1","var2","var3")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}