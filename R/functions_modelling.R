# custom functions

fitRICLPM <- function(d, constrained = FALSE) {
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
    constraints <- c("b11","b12","b13",
                     "b21","b22","b23",
                     "b31","b32","b33",
                     "cov12","cov13","cov23",
                     "var1","var2","var3")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}