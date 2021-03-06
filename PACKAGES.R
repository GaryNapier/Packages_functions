# PACKAGES


# INSTALL
# install.packages("dplyr")
# install.packages("rlist")
# install.packages("data.table")
# install.packages("rlist")
# install.packages("RCurl")
# install.packages("glmnet")
# install.packages("arm")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("psych")
# install.packages("aod")
# install.packages("QuantPsyc")
# install.packages("R.matlab")
# install.packages("swirl")
# install.packages("XML")
# install.packages("lubridate")
# install.packages("zoo")
# install.packages("xlsx")
# install.packages("rJava")
# install.packages("gdata")
# install.packages("combinat")
# install.packages("ISLR")
# install.packages("caret")
# install.packages("stringi")
# install.packages("scrapeR")
# install.packages("httr")
# install.packages("rvest")
# install.packages("GGally")
# install.packages("kernlab")
# install.packages("randomForest")
# install.packages("e1071")
# install.packages("pROC")
# install.packages("jpeg")
# install.packages("ggROC")
# install.packages("mlbench")
# install.packages("pwr")
# install.packages("ROCR")
# install.packages("quantmod")
# install.packages("fields")
# install.packages("C50")
# install.packages("AppliedPredictiveModeling")
# install.packages("class")
# install.packages("boot")
# install.packages("gbm")
# install.packages("moments")
# install.packages("Hmisc")
# install.packages("pracma")
# install.packages("klaR")
# install.packages("rpart")
# install.packages("ellipse")
# install.packages("ipred")
# install.packages("MASS")
# install.packages("RWeka")
# install.packages("penalizedLDA")
# install.packages("Amelia")
# install.packages("corrgram")
# install.packages("R.matlab")
# install.packages("lsr")
# install.packages("ez")
# install.packages("NCmisc")
# install.packages("gmodels")
# install.packages("Sweave")
# install.packages("plotly")
# install.packages("forecast")
# install.packages("Rmisc") 
# install.packages("tidyr")
# install.packages("miscTools")
# install.packages("plotrix")
# install.packages("car")
# install.packages("coin")
# install.packages("lsmeans")
# install.packages("dunn.test")
# install.packages("LogicReg")
# install.packages("Sweave")
# install.packages("stargazer")
# install.packages("htmlTable")
# install.packages("memisc")
# install.packages("texreg")
# install.packages("R.matlab")
# install.packages("multcomp")
# install.packages("sna")
# install.packages("knitr")
# install.packages("xtable")
# install.packages("pander")
# install.packages("gdata")
# install.packages("formattable")
# install.packages("ezknitr")
# install.packages("broom")
# install.packages("DMwR")
# install.packages("tsModel")
# install.packages("cowplot")
# install.packages("vcd")
# install.packages("doParallel")
# install.packages("stepPlr")



 
# install.packages("swirl")

 # library(swirl)
# # 
# # install_from_swirl("Getting and Cleaning Data")
# install_from_swirl("Regression Models")
# # install_from_swirl("Exploratory Data Analysis")
# # install_from_swirl("Mathematical Biostatistics BootCamp")
# install_from_swirl("Statistical Inference")
# swirl()

# LIBRARY
# Load_packages <- function(){
  # tryCatch({
library(dplyr)
# library(rlist)
library(data.table)
# library(rlist)
# library(RCurl)
library(glmnet)
library(arm)
# library(XML)
library(ggplot2)
# library(stringr)
# library(lubridate)
# library(zoo)
# library(xlsx)
# library(rJava)
# library(gdata)
# library(combinat)
# library(ISLR)
library(caret)
# library(stringi)
# library(scrapeR)
# library(httr)
# library(rvest)
# library(GGally)
# library(kernlab)
library(randomForest)
library(e1071)
# library(pROC)
# library(ggROC)
# library(mlbench)
# library(pwr)
# library(ROCR)
# library(jpeg)
# library(quantmod)
# library(fields)
library(C50)
# library(AppliedPredictiveModeling)
# library(class)
library(LogicReg)
# library(boot)
# library(moments) 
library(Hmisc)
library(pracma)
# library(klaR)
library(rpart)
library(plyr)
# library(ellipse)
library(ipred)
library(MASS)
# library(penalizedLDA)
# library(Amelia)
# library(corrgram)
library(lsr)
library(multcomp)
library(gridExtra)
# library(ez)
# library(NCmisc)
# library(gmodels)
# library(Sweave)
# library(plotly)
# library(forecast)
library(Rmisc)
library(tidyr)
library(miscTools)
library(plotrix)
library(car)
library(coin)
# library(lsmeans)
library(dunn.test)
library(knitr)
# library(stats)
library(xtable)
# library(stargazer)
library(htmlTable)
# library(memisc)
# library(texreg)
library(R.matlab)
# library(sna)
# library(pander)
# library(gdata)
# library(formattable)
library(broom)
library(DMwR)
# library(tsModel)
library(gbm)
library(survival)
library(splines)
library(parallel)
library(cowplot)
library(vcd)
library(latticeExtra)
library(doParallel)
library(stepPlr)
    
  # }, error = function(e){})
# }
# Load_packages()

#------------------------------------------------------------------------------------------------------------------------
# Custom functions
#------------------------------------------------------------------------------------------------------------------------

# List of functions:

# Split_into - Split vector V into X number of pieces which are a multiple of V

# set_plot - Set dimensions of multiple plot layout
# reset_plot - reset multiple plot layout to default - i.e. 1x1

# ggplotColours - Colour function - not sure what this does, but results in better colour for GGplot

# lapply_mod - Modified lapply allowing looping by index

# Shapiro_NA - allows NA values when using shapiro.test

# g_legend - Extract legend

# Split_into - For splitting vector into x pieces as a multiple of vector

# Tapas_sgm  - Converts to sigmoid  - Gets the right predictions - adapted from Tapas files 

# Interleave_cols - Interleave two matrix columns assuming matrices are same size

# Eta_sq_nonpara - Effect size for non-parametric ANOVAs

# summarySE - summarises data giving mean, and s.e. - must use melt() to format

# is.even - Tests even/odd

# Mw_effect - Get effect size for non-parametric t-test (Mann-Whitney)

# Add_html - Add html to make tables go horizontally across

# T_frame - Function for putting core t-test output into table (t, df, p, effect size)

# Match_mod - Gets index of columns of x containing any one contained in 'pattern' and cleans up

# ROC_sens_spec - Function to get max ROC, max sens, and max spec for each model 

# Get_CV_results - Function to get mean ROC, sens, spec from CV results

# Conf_matrices - Function to get the confusion matrices for each model

# Get_sig_diffs - Get sig diffs of ANOVA models via TukeyHSD


# Functions:

# Split vector V into X number of pieces which are a multiple of V
Split_into <- function(Vector, Pieces){
  # 'Pieces' must be multiple of Vector
  stopifnot((length(Vector)/Pieces)%%1==0)
  split(Vector, gl(length(Vector)/(length(Vector)/Pieces), 
                   length(Vector)/(length(Vector)/(length(Vector)/Pieces))))
}
# 
# 
# # Set and reset plots
reset_plot <- function(){
  par(mfrow = c(1, 1))
}
set_plot <- function(x,y){
  par(mfrow = c(x,y))
}

# # Colour function - not sure what this does, but results in better colour for GGplot
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

# Modified lapply allowing looping by index
lapply_mod <- function(X, FUN, ...){
  lapply(seq_along(X), FUN, ...)
}
# 
All_same <- function(x){
  length(unique(x)) == 1
}

Shapiro_NA <- function(Data){
  if (All_same(Data)){
    NA
  }else{
    shapiro.test(Data)$p.value
  }
}

# Extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Custom function for splitting vector into x pieces as a multiple of vector
Split_into <- function(Vector, Pieces){
  # 'Pieces' must be multiple of Vector
  stopifnot((length(Vector)/Pieces)%%1==0)
  split(Vector, gl(length(Vector)/(length(Vector)/Pieces), 
                   length(Vector)/(length(Vector)/(length(Vector)/Pieces))))
}

# Not sure what this is for, but it gets the right predictions - adapted from Tapas files 
Tapas_sgm <- function(x, a){ # NB 'a' is always 1 in original Matlab plotting script
  Predictions <- a/(1+exp(-x))
  return(Predictions)
}

# Interleave two matrix columns assuming matrices are same size
Interleave_cols <- function(Matrix_1, Matrix_2){
  Interleave_list <- lapply(seq(ncol(Matrix_1)), function(i) cbind(Matrix_1[,i], Matrix_2[,i]))
  Interleave_matrix <- signif(do.call("cbind", Interleave_list), digits = 3)
  return(Interleave_matrix)
}

Eta_sq_nonpara <- function(Chi_sq, N){
  tryCatch({Eta_sq <- Chi_sq/(N-1)}, error = function(e){Eta_sq <- NA})
  return(Eta_sq)
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Tests even/odd
is.even <- function(x) x %% 2 == 0 

# Get effect size for non-parametric t-test (Mann-Whitney)
Mw_effect <- function(z, n){
  Effect <- abs(z)/sqrt(n)
  return(Effect)
}


# Add html to make tables go horizontally across
Add_html <- function(Tables){
  # Input must be list(Tables)
  Tables <- unlist(Tables)
  Tables <- cat(cat("<table class='container'><tr>"),
                for(i in seq(Tables)){
                  cat("<td>")
                  cat(Tables[i])
                  cat("</td>")
                  cat("<td>")
                },
                cat("</tr></table>"))
  return(cat(Tables))
}

# Function for putting core t-test output into table (t, df, p, effect size)
T_frame <- function(Vector, Binary_var){
  Out <- tidy(t.test(Vector ~ Binary_var, equal.var=F))
  Effect_size <- cohensD(Vector ~ Binary_var, method="unequal")
  Out <- round(c(Out$statistic, Out$parameter, Out$p.value, Effect_size), digits=3)
  names(Out) <- c("t", "df", "p", "Effect size (d)")
  Out <- t(data.frame(Out))
  rownames(Out) <- NULL
  return(Out)
}

# Gets index of columns of x containing any one contained in 'pattern' and cleans up
Match_mod <- function(Pattern, x){
  Out <- match(Pattern, x)
  Out <- Out[!is.na(Out)]
  return(Out)
}

# Function to get max ROC, max sens, and max spec for each model 
ROC_sens_spec <- function(...){
  library(htmlTable)
  Args <- list(...)
  Model_names <<- as.list(sapply(substitute({...})[-1], deparse))
  # Function for getting max sensitivity
  Max_measures <- function(df, colname = "results"){
    df <- df[[colname]]
    new_df <- df[c(which.max(df$ROC), which.max(df$Sens), which.max(df$Spec)), ]
    x <- sapply(new_df, is.numeric)
    new_df[, x] <- round(new_df[, x], 2)
    new_df
  }
  # Find max Sens for each model
  Max_out <- lapply(Args, Max_measures)
  names(Max_out) <- Model_names
  
  Max_out <- lapply(seq(Max_out), 
                    function(i) htmlTable(Max_out[[i]], 
                                          caption=sprintf("Max ROC, sens, spec %s", Model_names[[i]]),
                                          align=c("c","|"), align.header =c("c","|"), col.columns = c("none",Blue)))
  return(Max_out)
}

# Function to get mean ROC, sens, spec from CV results
Get_CV_results <- function(...){
  Args <- list(...)
  Model_names <<- as.list(sapply(substitute({...})[-1], deparse))
  
  Results <- summary(resamples(Args))
  Results <- data.frame(rbind(Results$statistics$ROC[,"Mean"],
                              Results$statistics$Sens[,"Mean"], 
                              Results$statistics$Spec[,"Mean"]))
  rownames(Results) <- c("ROC", "Sens", "Spec")
  colnames(Results) <- Model_names
  
  Table <- htmlTable(Results, align=c("c","|"), align.header =c("c","|"), col.columns = c("none",Blue), 
                     caption = "Mean ROC, sens, spec for models")
  return(Table)
}

# Function to get the confusion matrices for each model
Conf_matrices <- function(..., Original_data, Y){
  Args <- list(...)
  Model_names <<- as.list(sapply(substitute({...})[-1], deparse))
  
  # Get confusion matrices using predict() for each model
  set.seed(Seed)
  Predict_out <- lapply(Args, function(x) predict(x, Original_data))
  Conf_matrices <- lapply(Predict_out, function(x) confusionMatrix(x, Y))
  names(Conf_matrices) <- Model_names
  Conf_matrices <- sapply(Conf_matrices, function(x) rbind(x$table))
  rownames(Conf_matrices) <- Contingencies
  
  return(Conf_matrices)
}



# Get sig diffs of models 
Get_sig_diffs <- function(...){
  Args <- list(...)
  Model_names <- as.list(sapply(substitute({...})[-1], deparse))
  names(Args) <- Model_names
  
  x <- summary(resamples(Args))
  
  Sig_diffs <- list(melt(x$values[,seq(1, ncol(x$values), 3)]), 
                    melt(x$values[,seq(2, ncol(x$values), 3)]),
                    melt(x$values[,seq(3, ncol(x$values), 3)]))
  
  Sig_diffs <- lapply(Sig_diffs, function(x) 
    TukeyHSD(aov(value~variable, data=x))$variable[, c(1, 4)])
  
  Sig_diffs <- lapply(seq(Sig_diffs), function(i){
    tryCatch({
      txtRound(Sig_diffs[[i]], 3)
    }, error = function(e) {Sig_diffs[[i]]})
  })
  
  Tables <- lapply(seq(Sig_diffs), function(i) htmlTable(Sig_diffs[[i]], 
                                                         align=c("c","|"), 
                                                         align.header =c("c","|"), 
                                                         col.columns = c("none",Blue), 
                                                         caption = sprintf("Significant differences %s", Measures[i])))
  
  Tables <- Add_html(Tables)
}



# Input <- 1:40
# Take_out_vector <- 5
# From <- 1
# Knockout <- function(Input, Take_out_vector, From=1){
#   
#   
#   x <- seq(From, max(Input), Take_out_vector*2)
#   
#   
#   for (i in seq(x)){
#     
#     x[i] <- x[i]:(x[i]+(Take_out_vector-1))
#     
#   }
#   return(x)
# }
# 
# Knockout(1:40, 5, From=1)

# Proportions_table <- function(Risk_factors_train){
#   
#   Risk_factors_train_list <- sapply(Risk_factors_train, list)
#   
#  Test <-  sapply(Risk_factors_train_list, function(x){
#     table(x)/max(table(x))
#   })
# }

#------------------------------------------------------------------------------------------------------------------------
# Plotly
#------------------------------------------------------------------------------------------------------------------------

# hzsble7tht
# 
# Sys.setenv("GaryNapier"="GaryNapier")
# Sys.setenv("hzsble7tht"="hzsble7tht")

# Publish your graphs to plotly with plotly_POST
# library(plotly)
# p <- plot_ly(midwest, x = percollege, color = state, type = "box")
# plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)


































