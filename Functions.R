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

hs <- function(x, ...){
  # Print both head() and str() so don't have to print separately
  print(head(x, ...))
  print("---")
  str(x, ...)
}

round_if <- function(x, round_place = 3){
  # For a numeric column of a dataframe, display as a regular decimal to 3 places if > 0.01
  # If less then display as scientific e.g. 1e-03
  # n.b. converts whole vector to string
  # > x <- data.frame(ID = LETTERS[1:5], a = c(0.1, 0.01, 0.001, 0.0001, 0.00001), b = 1:5)
  # > round_if(x$a)
  # [1] "0.1"   "0.01"  "1e-03" "1e-04" "1e-05"
  # > x$a <- round_if(x$a)
  # > x
  #   ID     a b
  # 1  A   0.1 1
  # 2  B  0.01 2
  # 3  C 1e-03 3
  # 4  D 1e-04 4
  # 5  E 1e-05 5
  x_new <- vector()
  for(i in seq(x)){
    num <- as.numeric(x[i])
    if(is.na(num)){
      x_new[i] <- x[i]
    }else if(num < 0.01){
      x_new[i] <- scales::scientific(num)
    }else{
      x_new[i] <- round(num, round_place)
    }
  }
  x_new
}

num_cols <- function(x){
  # Returns only the numeric columns of a dataframe
  x[, sapply(x, is.numeric)]
}

group_sum <- function(df, col, r_names){
  # Sum all the numeric columns by the row values of another (string, probably 'ID') column
  # Creates a new concatenated value for the ID column
  # Needs the num_cols() function above
  # > x <- data.frame(ID = LETTERS[1:10], x = 1:10, y = 21:30)
  # > group_sum(x, "ID", c("A", "B", "C"))
  #        ID x  y
  # 1 A, B, C 6 66
  col <- as.character(col)
  x <- df[df[, col] %in% r_names, ]
  num_df <- data.frame(as.list(colSums(num_cols(x))))
  cat_df <- setNames(data.frame(paste0(x[, col], collapse = ", ")), col) 
  cbind(cat_df, num_df)
}

fmt <- function(x, ...){
  # > fmt(1000)
  # [1] "1,000"
  # > fmt(10000)
  # [1] "10,000"
  # Add comma to numbers. Converts to string.
  format(x, big.mark=",",scientific=FALSE, ...)
}

len_str <- function(string){
  # Get length of string or vector of strings
  # > len_str("bananas")
  # [1] 7
  # > len_str(c("bananas", "apples", "grapes"))
  # [1] 19
  length(unlist(strsplit(string, split = "")))
}

to_table <- function(x, pc_dir = "row"){
  # Add percentages and totals to tables. 
  # Requires dplyr and janitor packages
  # pc_dir = the direction to use for calculating percentages. One of "row", "col", or "all".
  # x <- data.frame(ID = LETTERS[1:10], x = 1:10, y = 21:30)
  # > to_table(x, pc_dir = "row")
  # ID           x            y         Total
  # A  1  (4.55%)  21 (95.45%)  22 (100.00%)
  # B  2  (8.33%)  22 (91.67%)  24 (100.00%)
  # C  3 (11.54%)  23 (88.46%)  26 (100.00%)
  # D  4 (14.29%)  24 (85.71%)  28 (100.00%)
  # E  5 (16.67%)  25 (83.33%)  30 (100.00%)
  # F  6 (18.75%)  26 (81.25%)  32 (100.00%)
  # G  7 (20.59%)  27 (79.41%)  34 (100.00%)
  # H  8 (22.22%)  28 (77.78%)  36 (100.00%)
  # I  9 (23.68%)  29 (76.32%)  38 (100.00%)
  # J 10 (25.00%)  30 (75.00%)  40 (100.00%)
  # Total 55 (17.74%) 255 (82.26%) 310 (100.00%)
  
  
  if(!require(dplyr)){
    install.packages("dplyr")
  }
  if(!require(janitor)){
    install.packages("janitor")
  }
  x <- x %>% janitor::adorn_totals(c("row", "col")) %>%
    janitor::adorn_percentages(c(pc_dir)) %>%
    janitor::adorn_pct_formatting(digits = 2)
  formatted_ns <- attr(x, "core") %>% # extract the tabyl's underlying Ns
    janitor::adorn_totals(c("row", "col")) %>% # to match the data.frame we're appending to
    dplyr::mutate_if(is.numeric, format, big.mark = ",")
  x %>% adorn_ns(position = "front", ns = formatted_ns)
}

log10_ceiling <- function(x) {
  # Round up to nearest 10, 100, 1000 etc depending on size of number
  # > log10_ceiling(10)
  # [1] 10
  # > log10_ceiling(11)
  # [1] 100
  # > log10_ceiling(99)
  # [1] 100
  # > log10_ceiling(101)
  # [1] 1000
  # > log10_ceiling(9999)
  # [1] 10000
  # > log10_ceiling(10001)
  # [1] 1e+05
  10^(ceiling(log10(x)))
}

tab2df <- function(tab){
  # Convert table object to dataframe
  df <- data.frame(x = matrix(tab, ncol = length(tab)))
  names(df) <- names(tab)
  df
}

print_vect <- function(x){
  # Print vector with commas and spaces separating
  # x <- 1:10
  # > print_vect(x)
  # [1] "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"
  # > x <- c("bananas", "pears", "apples")
  # > print_vect(x)
  # [1] "bananas, pears, apples"
  gsub(".{1}$", "", paste0(x, sep = ",", collapse = " "))
}

drop_cols <- function(x, cnames){
  # Drop columns by column names
  x[!(names(x) %in% cnames)]
}

# Get T/F for which columns are numeric instead of using sapply
# > head(df)
# gene1       pos1 gene2          pos2        chisq          lrt       lor        se        waldp
# katG p.Ser315Thr  ahpC      c.-48G>A 6.365049e-03 6.838877e-03 0.6484617 0.2306100 4.924359e-03
# katG p.Ser315Thr  ahpC      c.-51G>A 1.336244e-03 1.772518e-03 1.0247058 0.3149814 1.141025e-03
# katG p.Ser315Thr  ahpC c.-47_-46insT 1.726763e-22 2.161040e-18 2.7396624 0.3700432 1.325123e-13
# > is_numeric(df)
# gene1  pos1 gene2  pos2 chisq   lrt   lor    se waldp 
# FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
# df[, is_numeric(df)] <- <code>
is_numeric <- function(df){
  sapply(df, is.numeric)
}

# Round columns, keeping scientific notation if already in scientific notation(round_if function), and convert back to numeric
# > head(df)
# gene1       pos1 gene2          pos2        chisq          lrt       lor        se        waldp
# katG p.Ser315Thr  ahpC      c.-48G>A 6.365049e-03 6.838877e-03 0.6484617 0.2306100 4.924359e-03
# katG p.Ser315Thr  ahpC      c.-51G>A 1.336244e-03 1.772518e-03 1.0247058 0.3149814 1.141025e-03
# katG p.Ser315Thr  ahpC c.-47_-46insT 1.726763e-22 2.161040e-18 2.7396624 0.3700432 1.325123e-13
# df <- round_cols(df)
# > head(katg)
# gene1       pos1 gene2          pos2    chisq      lrt   lor    se    waldp
# katG p.Ser315Thr  ahpC      c.-48G>A 6.37e-03 6.84e-03 0.648 0.231 4.92e-03
# katG p.Ser315Thr  ahpC      c.-51G>A 1.34e-03 1.77e-03 1.025 0.315 1.14e-03
# katG p.Ser315Thr  ahpC c.-47_-46insT 1.73e-22 2.16e-18 2.740 0.370 1.33e-13
round_cols <- function(df){
  df[, is_numeric(df)] <- apply(apply(num_cols(df), 2, round_if), 2, as.numeric)
  df
}

# Sort a whole dataframe with each column decreasing by default
# df <- data.frame(x = c(2, 2, 3, 3, 1, 1), y = c('f', 'c', 'b', 'a', 'z', 'w'))
# odr(df)
# x y
# 1 w
# 1 z
# 2 c
# 2 f
# 3 a
# 3 b
odr <- function(x, decreasing = F){
  x[do.call(order, c(x, list(decreasing=decreasing))),]
}


uniq_col <- function(x, split = "; "){
  unique(unlist(strsplit(x, split)))
}

col_pc <- function(df, col){
  round((df[, col] / sum(df[, col]))*100, 3)
}

len_uniq <- function(x){
  length(unique(x))
}

rbind_force <- function(df1, df2){
  rbind(df1, setNames(df2, names(df1)))
}

# Make quick pivot table summarising total unique values by column
# my_formula arg must be call to the formula() function: formula(<col> ~ 'n')
# Result:
#             PRM            n
#  katG-c.2223A>G  3   (4.05%)
# katG-p.Arg484His  5   (6.76%)
# ...
# katG-p.Tyr413Cys  8  (10.81%)
#           Total 74 (100.00%)
pivot <- function(x, my_formula, value_var = "wgs_id"){
  drop_cols(to_table(reshape2::dcast(x, my_formula, value.var = value_var, fun.aggregate = len_uniq),
                     pc_dir = 'col'), 
            'Total')
}

duped <- function(x, col){
  dups <- x[duplicated(x[col]), col]
  x[x[, col] %in% dups, ]
}

# Swap first and second parts of string, splitting on a charachter (or substring)
# e.g. 
# x <- "katG-p.Ile335Val; katG-p.Ser315Thr"
# swap_str(x, "; ")
# "katG-p.Ser315Thr; katG-p.Ile335Val"
swap_str <- function(x, split_chr = "; "){
  paste0(unlist(strsplit(x, split_chr))[c(2, 1)], collapse = split_chr)
}


# Print each element of vector vertically instead of across the screen
# e.g. 
# > x <- c("a", "b", "c")
# > x
# [1] "a" "b" "c"
# > print_vert(x)
# a
# b
# c
print_vert <- function(x){
  cat(paste0(x, '\n'))
}

# Remove change from second gene-change pair
# e.g.
# x <- "katG-p.Ser315Thr; fabG1-c.-15C>T"
# > drop_change(x, "-")
# [1] "katG-p.Ser315Thr; fabG1"
drop_change <- function(string, split_on = "-"){
  unlist(lapply(strsplit(string, split_on), function(x){paste0(x[c(1, 2)], collapse = split_on)}))
}

clean_binary_table <- function(x){
  x <- data.frame(apply(x, 2, function(x){gsub("\\[\\]", NA, as.character(x))}))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\[||\\]", "", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\), \\(", "; ", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\', \\'", "-", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\(||\\)", "", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\'", "", as.character(x)) }))
  x
}

sort_pos <- function(x, col){
  
  x$pos <- as.numeric(stringr::str_extract(x[, col], "[0-9]+"))
  x <- x[order(x$pos), ]
  drop_cols(x, 'pos')
  
}

uniq_col <- function(x, split = "; "){
  unique(unlist(strsplit(x, split)))
}

col_pc <- function(df, col){
  round((df[, col] / sum(df[, col]))*100, 3)
}

len_uniq <- function(x){
  length(unique(x))
}

rbind_force <- function(df1, df2){
  rbind(df1, setNames(df2, names(df1)))
}

# Make quick pivot table summarising total unique values by column
# my_formula arg must be call to the formula() function: formula(<col> ~ 'n')
# Result:
#             PRM            n
#  katG-c.2223A>G  3   (4.05%)
# katG-p.Arg484His  5   (6.76%)
# ...
# katG-p.Tyr413Cys  8  (10.81%)
#           Total 74 (100.00%)
pivot <- function(x, my_formula, value_var = "wgs_id"){
  drop_cols(to_table(reshape2::dcast(x, my_formula, value.var = value_var, fun.aggregate = len_uniq),
                     pc_dir = 'col'), 
            'Total')
}

pivot_num <- function(x, my_formula,value_var = "wgs_id", ...){
  reshape2::dcast(x, my_formula, value.var = value_var, fun.aggregate = len_uniq, ...)
}

split_pivot <- function(df, my_formula, leave_col, value_var = "wgs_id", split_on_col = "Drug"){
  
  to_table_split_pivot <- function(x, col){
    x[[col]] <- as.character(x[[col]])
    x <- drop_cols(to_table(x, "col"), "Total")
    x[[col]] <- gsub("\\s*\\([^\\)]+\\)", "", x[[col]])
    x
  }
  
  df_pivot <- reshape2::dcast(df, my_formula, value.var = 'wgs_id', fun.aggregate = len_uniq)
  df_split <- split(df_pivot, df_pivot[, split_on_col])
  
  do.call('rbind', lapply(df_split, to_table_split_pivot, leave_col))
  
}

duped <- function(x, col){
  dups <- x[duplicated(x[col]), col]
  x[x[, col] %in% dups, ]
}

# Swap first and second parts of string, splitting on a charachter (or substring)
# e.g. 
# x <- "katG-p.Ile335Val; katG-p.Ser315Thr"
# swap_str(x, "; ")
# "katG-p.Ser315Thr; katG-p.Ile335Val"
swap_str <- function(x, split_chr = "; "){
  paste0(unlist(strsplit(x, split_chr))[c(2, 1)], collapse = split_chr)
}


# Print each element of vector vertically instead of across the screen
# e.g. 
# > x <- c("a", "b", "c")
# > x
# [1] "a" "b" "c"
# > print_vert(x)
# a
# b
# c
print_vert <- function(x){
  cat(paste0(x, '\n'))
}

# Remove change from second gene-change pair
# e.g.
# x <- "katG-p.Ser315Thr; fabG1-c.-15C>T"
# > drop_change(x, "-")
# [1] "katG-p.Ser315Thr; fabG1"
drop_change <- function(string, split_on = "-"){
  unlist(lapply(strsplit(string, split_on), function(x){paste0(x[c(1, 2)], collapse = split_on)}))
}

clean_binary_table <- function(x){
  x <- data.frame(apply(x, 2, function(x){gsub("\\[\\]", NA, as.character(x))}))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\[||\\]", "", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\), \\(", "; ", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\', \\'", "-", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\(||\\)", "", as.character(x)) }))
  x <- data.frame(apply(x, 2, function(x){ gsub("\\'", "", as.character(x)) }))
  x
}

sort_pos <- function(x, col){
  x$pos <- as.numeric(stringr::str_extract(x[, col], "[0-9]+"))
  x <- x[order(x$pos), ]
  drop_cols(x, 'pos')
}

lapply_nms <- function(x, fun, nms){
  res <- lapply(x, fun)
  names(res) <- nms
  res
}

drug_col_front <- function(df_list){
  lapply(df_list, function(x){ select(x, Drug, everything()) })
}

add_drug_col <- function(df_list, drugs){
  drug_col_front(lapply_mod(df_list, function(i){df_list[[i]]$Drug <- rep(drugs[i], nrow(df_list[[i]])); df_list[[i]]}))
}

find_pos <- function(x){
  sub("\\-", "", gsub("[^0-9\\-]+", "", x))
}

fmt_pc <- function(x, rnd = 2, ...){
  paste0(fmt(round(x*100, rnd), ...), "%")
}

trimws_df <- function(x){
  data.frame(sapply(x, trimws))
}

tonum <- function(x){
  as.numeric(gsub(",", "", strsplit(x, " ")[[1]][1]))
}

uniq_vars <- function(x){
  unique(unlist(strsplit(x, "; ")))
}

rm_null <- function(x){
  x[!sapply(x,is.null)] 
}

rm_norow <- function(x){
  x[sapply(x, nrow) > 0]
}

rm_na <- function(x){
  x[!is.na(x)]
}

pivot_numeric <- function(x, my_formula, value_var = "wgs_id"){
  reshape2::dcast(x, my_formula, value.var = value_var, fun.aggregate = len_uniq)
}

print_chisq <- function(chisq_result, scientific = T, rnd = 3){
  paste0("X-sq = ", round(chisq_result$statistic, rnd), 
         "; df = ", round(chisq_result$parameter, rnd), 
         "; p = ", format(chisq_result$p.value, scientific = scientific))
}

clean_del_ins_dup <- function(x){
  gsub("(del|ins|dup).*", "\\1", x)
}

drop_col_name <- function(x, name){
  # Return col names except named col names
  names(x)[!(names(x) %in% name)]
}

rbind_mod <- function(df1, df2, names){
  rbind(setNames(df1, names), setNames(df2, names))
}


split_df_on_haplotype <- function(df, col){
  # Duplicate sample row if other_vars has more than one var (i.e. on the "; " character/string)
  data.frame(df %>% mutate(col = strsplit(as.character(col), "; ")) %>% unnest(col))
}

strsplit_mod <- function(x,
                         split,
                         type = "remove",
                         perl = FALSE,
                         ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

get_gene <- function(x, split_on = "-"){
  unlist(lapply(strsplit(x, split_on), function(x){x[1]}))
}

drop_gene <- function(x, split_on = "-"){
  unlist(lapply(strsplit_mod(x, split_on, "before"), function(x){
    if(length(x) < 3){
      x <- paste0(x[2:length(x)], collapse = "")
      x <- gsub("-p.", "", x)
      gsub("-c.", "", x)
    }else{
      x <- paste0(x[3:length(x)], collapse = "")
      x <- gsub("-p.", "", x)
      gsub("-c.", "", x)
    }
  }))
}

try_read_csv <- function(x){
  tryCatch({read.csv(x)}, error = function(e){})
}

add_pc_col <- function(x, col, divide_by, rnd = 2){
  x$pc <- fmt_pc(x[, col]/divide_by, rnd)
  x[col] <- fmt(x[, col])
  x
}

dark_to_light <- function(lin_names, first_col){
  if(length(lin_names) == 1){
    names(first_col) <- lin_names
    return(first_col)
  }else{
    lighten_to_col <- lighten(first_col, 1-(1/(length(lin_names))))
    fc <- colorRampPalette(c(first_col, lighten_to_col))
    lin_all_cols <- fc(length(lin_names))
    scales::show_col(lin_all_cols)
    names(lin_all_cols) <- lin_names
    return(lin_all_cols)
  }
}

non_num_cols <- function(x){
  # Returns only the non-numeric columns of a dataframe
  x[!(sapply(x, is.numeric))]
}

expand_hierarchy <- function(df, group_by_col_name, hierarchy_to_expand_col_name){
  # Takes df like this:
  #   ID      Group
  # 1 samp_1  4.2.1.1
  # 2 samp_2  1.2.1.2.1
  
  # And makes this:
  #   ID    lin_level_1 lin_level_2 lin_level_3 lin_level_4 lin_level_5   max_lin
  # 1 samp_1          4         4.2       4.2.1     4.2.1.1        <NA>   4.2.1.1
  # 2 samp_2          1         1.2       1.2.1     1.2.1.2   1.2.1.2.1 1.2.1.2.1
  
  split_lins <- stringr::str_split(df[[hierarchy_to_expand_col_name]], "\\.")
  max_lin_len <- max(sapply(split_lins, length))
  mat <- matrix(nrow = length(df[[group_by_col_name]]), ncol = max_lin_len+1)
  mat[, 1] <- df[[group_by_col_name]]
  for(i in 1:nrow(mat)){
    for(lin_level in 1:max_lin_len){
      
      len_lin <- length(split_lins[[i]])
      
      if(lin_level > len_lin){
        mat[i, lin_level+1] <- NA
      }else{
        mat[i, lin_level+1] <- paste0(split_lins[[i]][1:lin_level], collapse = ".")
      }
    }
  }
  
  max_lin <- vector()
  for(i in seq(nrow(mat))){
    max_lin[i] <- mat[i, which.max(sapply(mat[i, -1], len_str))+1]
  }
  mat <- data.frame(cbind(mat, max_lin), stringsAsFactors = F)
  names(mat) <- c("id", paste0("lin_level_", 1:(ncol(mat)-2) ), "max_lin")
  return(mat)
}

expand_hierarchy_fill <- function(df, group_by_col_name, hierarchy_to_expand_col_name){
  # Takes df like this:
  #   ID      Group
  # 1 samp_1  4.2.1.1
  # 2 samp_2  1.2.1.2.1
  # 3 samp_3  4
  
  # And makes this:
  #   ID    lin_level_1 lin_level_2 lin_level_3 lin_level_4 lin_level_5   max_lin
  # 1 samp_1          4         4.2       4.2.1     4.2.1.1     4.2.1.1   4.2.1.1
  # 2 samp_2          1         1.2       1.2.1     1.2.1.2   1.2.1.2.1 1.2.1.2.1
  # 3 samp_3          4           4           4           4           4         4
  
  # To fill hierarchy with NAs use function expand_hierarchy()
  #   ID    lin_level_1 lin_level_2 lin_level_3 lin_level_4 lin_level_5   max_lin
  # 1 samp_1          4         4.2       4.2.1     4.2.1.1        <NA>   4.2.1.1
  # 2 samp_2          1         1.2       1.2.1     1.2.1.2   1.2.1.2.1 1.2.1.2.1
  # 3 samp_3          4        <NA>        <NA>        <NA>        <NA>      <NA>
  
  split_lins <- stringr::str_split(df[[hierarchy_to_expand_col_name]], "\\.")
  max_lin_len <- max(sapply(split_lins, length))
  mat <- matrix(nrow = length(df[[group_by_col_name]]), ncol = max_lin_len+1)
  mat[, 1] <- df[[group_by_col_name]]
  
  for(i in 1:nrow(mat)){
    for(lin_level in 1:max_lin_len){
      
      len_lin <- length(split_lins[[i]])
      
      if(lin_level > len_lin){
        # mat[i, lin_level+1] <- NA
        mat[i, lin_level+1] <- mat[i, lin_level]
      }else{
        mat[i, lin_level+1] <- paste0(split_lins[[i]][1:lin_level], collapse = ".")
      }
    }
  }
  
  max_lin <- vector()
  for(i in seq(nrow(mat))){
    max_lin[i] <- mat[i, which.max(sapply(mat[i, -1], len_str))+1]
  }
  mat <- data.frame(cbind(mat, max_lin), stringsAsFactors = F)
  names(mat) <- c("id", paste0("lin_level_", 1:(ncol(mat)-2) ), "max_lin")
  return(mat)
}

top_spol_freq <- function(x, col){
  x %>% 
    group_by_(.dots = lazyeval::lazy(col)) %>% 
    dplyr::count(.dots = lazyeval::lazy(col), spoligotype) %>% 
    dplyr::arrange(desc(n), .by_group = TRUE) %>%
    dplyr::top_n(10, n) %>% 
    data.frame()
}

tab2df <- function(df){
  as.data.frame.matrix(table(df))
}

row_probs <- function(x){
  # Dataframe:
  # fruit   X1 X2 X3
  # mangoes 90 10 20
  # bananas 10 90 80
  # Get the row-wise proportions for each number:
  # fruit   X1     X2     X3
  # mangoes 0.7500 0.0833 0.167
  # bananas 0.0556 0.5000 0.444
  # i.e. mangoes-X1 = 90/(90+10+20) = 0.75
  
  # Strip off the non-numeric col(s)
  x_non_num <- non_num_cols(x)
  # Strip off numeric cols for calculation 
  x <- num_cols(x)
  # Pre-define an empty matrix
  mat <- matrix(nrow = nrow(x), ncol = ncol(x))
  # Loop over rows 
  for(row in seq(nrow(x))){
    # Loop over cols in row
    for(col in seq(ncol(x[row, ]))){
      # Divide the number by the total in the row
      mat[row, col] <- x[row, col]/sum(x[row, ])
    }
  }
  # Put table back together
  cbind(x_non_num, setNames(data.frame(mat), names(x)))
}

col_probs <- function(x){
  # Dataframe:
  # fruit   X1 X2 X3
  # mangoes 90 10 20
  # bananas 10 90 80
  # Get the col-wise proportions for each number:
  # col_probs(x)
  # fruit   L1  L2  L3
  # mangoes 0.9 0.1 0.2
  # bananas 0.1 0.9 0.8
  # i.e. mangoes-X1 = 90/(90+10) = 0.9
  
  # Strip off the non-numeric col(s)
  x_non_num <- non_num_cols(x)
  # Strip off numeric cols for calculation 
  x <- num_cols(x)
  # Pre-define an empty matrix
  mat <- matrix(nrow = nrow(x), ncol = ncol(x))
  # Loop over rows 
  for(row in seq(nrow(x))){
    # Loop over cols in row
    for(col in seq(ncol(x[row, ]))){
      # Divide the number by the total in the row
      mat[row, col] <- x[row, col]/sum(x[ ,col])
    }
  }
  # Put table back together
  cbind(x_non_num, setNames(data.frame(mat), names(x)))
}

sort_df_by_col_name <- function(df, col, decreasing = T){
  df[order(df[, col],  decreasing = decreasing),]
}

rm_dup_group <- function(df, col){
  # If rows of groups are repeated in a col, replace repeats with blank ("")
  # For final display of tables
  # e.g. 
  # in
  # lineage value
  # L1      10
  # L1      20
  # L1      25
  # L2      100
  # L2      3
  # L2      21
  # out
  # lineage value
  # L1      10
  #         20
  #         25
  # L2      100
  #         3
  #         21
  df[which(duplicated(df[col])), col] <- ""
  df
}



