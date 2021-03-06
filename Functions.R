#------------------------------------------------------------------------------------------------------------------------
# Custom functions
#------------------------------------------------------------------------------------------------------------------------

# List of functions:

# heaD - Always mis-typing head() as heaD(), so does the same thing

# h_col - do head() on just the first ten columns

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

# N_clusters - Get number of clusters when specifying the height of a tree. 
# Tree can be hclust or dendrogram object

# No_outliers - Take out outliers from each group (lineage in this case) -
# adapt for your group of interest

# Sub_group - Subset rows of data by group (sub_lineage in this case),
# first proportional to amount of data in the group vs other groups, then
# retaining x% of rows (Retain arg - set to 0-1). Group arg - srtring of group col names


# Functions:

heaD <- function(x,...){
  head(x, ...)
}

h_col <- function(x, nrow = 6){
  head(x[, 1:10], nrow) 
}

# Split vector V into X number of pieces which are a multiple of V
Split_into <- function(Vector, Pieces){
  # 'Pieces' must be multiple of Vector
  stopifnot((length(Vector)/Pieces)%%1==0)
  split(Vector, gl(length(Vector)/(length(Vector)/Pieces), 
                   length(Vector)/(length(Vector)/(length(Vector)/Pieces))))
}

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

N_clusters <- function(Tree_in, Height, Min_clust_sz){
  length(unique(cutreeDynamic(as.hclust(Tree_in), 
                              cutHeight = Height,
                              minClusterSize=Min_clust_sz
  )))
}

# Function to take out outliers from each group (lineage in this case) -
# adapt for your group of interest
No_outliers <- function(Data, SDs){
  # SDs <- 3.5 # How many standard devs from mean to take out
  Out <- function(Data, SDs){
    Vals <-c(mean(Data)+(sd(Data)*SDs),mean(Data)-(sd(Data)*SDs))
    # abs(Data - abs(mean(Data) - Val)) > Val
    Data > max(Vals) | Data < min(Vals)
  }
  
  # Need to order or indexing won't work
  Data <- Data[order(Data$main_lineage, Data$sub_lineage), ] 
  Outliers <- unique(c(
    as.vector(which(unlist(sapply(split(Data$V1, as.character(Data$sub_lineage)),
                                  function(x){Out(x, SDs)})))), 
    as.vector(which(unlist(sapply(split(Data$V2, as.character(Data$sub_lineage)),
                                  function(x){Out(x, SDs)}))))
  )) # unique(c(
  
  Data <- Data[-(Outliers), ]
}

# Function to subset rows of data by group (sub_lineage in this case),
# first proportional to amount of data in the group vs other groups, then
# retaining x% of rows (Retain arg - set to 0-1). Group arg - srtring of group col names
Sub_group <- function(x, Group, Retain){
  N_each_group <- plyr::count(x, eval(substitute(Group))) # Nb need library(dplyr)
  N_each_group$retain <- round(N_each_group$freq - 
                                 (N_each_group$freq*
                                    (N_each_group$freq/sum(N_each_group$freq)) ))
  
  N_each_group$retain <- round(N_each_group$retain*Retain)
  
  x_split <- split(x, as.character(x[,Group]))
  do.call("rbind", lapply(seq(x_split), function(i){
    x_split[[i]][sample(1:nrow(x_split[[i]]), 
                        N_each_group[N_each_group[, Group] == N_each_group[i,Group],
                                     "retain"]) , ]
  }))
}
