# install.packages("factoextra")
# install.packages("FKF")
# install.packages("clusterSim")
# install.packages("clValid")
#install.packages("patchwork")
library(patchwork)
library(readr)
library(dplyr)
library(lubridate)
library(cluster)
library(factoextra)
library(clusterSim)
library(clValid)
library(tidyverse)
library(FKF)


################################################################################
################################# problem no. 1 ################################


# function avrghours
#         input: data frame from "Haushalte mit Waermepumpe OHNE PV.zip"
#         output: list with [1] averaged values per hour and
#                           [2] timestamps where values are missing
avrghours <- function(x){
  # Convert index column to datetime
  x <- x %>%
    mutate(index = ymd_hms(index))
  # Columns: index and HAUSHALT_TOT 
  x <- x[,c(1,3)]
  # add Index for every hour
  x <- x %>% mutate(date = as_date(index), hour = hour(index))
  # hour same as index (for loops)
  x$hour <- x$hour + 1
  # remove index
  x <- x[,-1]
  # all days of 2019
  days <- seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = "days")
  # matrix for averaged hours
  avho <- matrix(nrow = 365, ncol = 24)
  # loop for averaging hours
  for(i in  1:365){
    part_data <- x[x$date == days[i],]
    avho[i,] <- sapply(1:24, function(y)
      mean(part_data[part_data$hour == y,][,1]))
  } 
  # determining missing hours
  missho <- t(apply(avho, 1, function(y) sapply(1:24, function(z) if(is.na(y[z] > 0)){
    y[z] <- NA} else{
      y[z] <- z
    })))
  
  return(list("Avhours" = avho, "Misshours" = missho))
} 

# function smoothing
#         input: data frame from "Haushalte mit Waermepumpe OHNE PV.zip" (named "SFH...")
#         output: data frame with number of household, day, weekday and smoothed values
#                 for every day of 2019
smoothing <- function(x, k = 3, hno){
  M <- avrghours(x)
  # k range
  q <- (k-1)/2 
  # Start and end
  Q <- q+1
  # Averaged hours as vector
  vecv <- as.vector(t(M$Avhours))
  # Vector for smoothed values 
  smoothv <- as.vector(t(M$Avhours))
  smoothv[which(smoothv == "NaN")] <- NA
  # Missing hours as vector
  vecm <- as.vector(t(M$Misshours))
  # Loop for smoothing
  for(j in Q:(length(vecv)-q)){
    # Condition must be satisfied for smoothing
    if((!is.na(vecm[j+q]) & !is.na(vecm[j-q]) & !is.na(vecm[j])) == TRUE){
      # Moving averages
      smoothv[j] <- mean(vecv[(j-q):(j+q)])
    }
  }
  M$Avhours <- matrix(smoothv, nrow = 365, byrow = TRUE)
  a <- sin((((1:365) - 31 - 28 - 21)/365)*2*pi)
  # Full data frame
  output <- data.frame(M$Avhours, 
                       "Weekday" = rep(c("Tuesday", "Wednesday", "Thursday", "Friday", 
                               "Saturday", "Sunday",
                               "Monday"), length.out = 365), 
             "Household_No" = rep(hno, 365), 
             "Day" = 1:365,
             "Season" = a)
  names(output)[1:24] <- c("0h","1h","2h","3h","4h","5h","6h","7h","8h","9h","10h",
                           "11h","12h","13h","14h","15h","16h","17h","18h","19h",
                           "20h","21h","22h","23h")
  return(output)
}


# function smoothing weekday
smooth_same_weekday <- function(df, k = 5) {
  q <- (k - 1) / 2  # Half-window size
  result_df <- df  # Copy to store results
  
  # Loop through each weekday
  for (wd in unique(df$Weekday)) {
    group_df <- df %>% filter(Weekday == wd)
    num_rows <- nrow(group_df)

    for (i in 1:num_rows) {
      if (i > q && (i + q) <= num_rows) {
        # Extract the window
        window <- group_df[(i - q):(i + q), 1:24]
        
        # Check if all window rows have no NAs in critical hourly columns
        if (!any(is.na(window))) {
          # Apply column-wise mean only if all relevant rows are complete enough
          result_df[result_df$Weekday == wd, 1:24][i, ] <- colMeans(window, 
                                                                    na.rm = TRUE)
        }
        # else: leave it unchanged (as in original data)
      }
    }
  }
  
  return(result_df)
}

M <- data.frame(matrix(nrow = 365*33, ncol = 52))
x <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 18, 19, 20, 21, 22, 23, 25, 27, 
  28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 40)
for(i in x){
  path = paste("E:/case_study/project_1/case_studies/project_1/data/OHNE PV/SFH", i, ".csv", sep = "")
  df <- read.csv(path)
  df_new <- smoothing(df, hno = i)
  p <- which(x == i)
  M[((p-1)*365+1):(p*365),c(1:24, 49, 50, 51, 52)] <- df_new
  M[((p-1)*365+1):(p*365),25:48] <- smooth_same_weekday(df_new)[,1:24]
}

names(M) <- c(paste(c("0h","1h","2h","3h","4h","5h","6h","7h","8h","9h","10h",
              "11h","12h","13h","14h","15h","16h","17h","18h","19h",
              "20h","21h","22h","23h"), "a", sep = ""),
              paste(c("0h","1h","2h","3h","4h","5h","6h","7h","8h","9h","10h",
                      "11h","12h","13h","14h","15h","16h","17h","18h","19h",
                      "20h","21h","22h","23h"), "b", sep = ""), 
              "Weekday", "Household", "Day", "Season")
save(M, file = "E:/case_study/project_1/case_studies/smoothed_data.RData")
load("E:/case_study/project_1/case_studies/smoothed_data.RData")

## factorize weekday and household
M$Weekday <- as.factor(M$Weekday)
M$Household <- as.factor(M$Household)


################################################################################
################################# problem no. 2 ################################


# choose randomly 30 households
set.seed(69)
rs <- sample(x, 30)

sM <- M[which(M$Household %in% rs),]
save(sM, file = "E:/case_study/project_1/case_studies/sampled_data.RData")

load(file = "E:/case_study/project_1/case_studies/sampled_data.RData")
# fit the models (interaction, no interaction), use manova()

# no interaction Weekday

no_interact <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[1:24], "`"), 
                        collapse = ", "), ") ~ Household + Weekday + Season")
)

no_interact_res <- manova(no_interact, data = sM)

# interaction Weekday

interact <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[1:24], "`"), 
                        collapse = ", "), ") ~ Household * Weekday * Season")
)

interact_res <- manova(interact, data = sM)
summary(interact_res, test = "Wilks")
summary(no_interact_res, test = "Wilks")

# reduce the model with interactions
# no need to reduce

sM$Weektype <- ifelse(sM$Weekday %in% c("Monday", "Tuesday", 
                                        "Wednesday", "Thursday", "Friday"), 1, 2)
sM$Weektype <- as.factor(sM$Weektype)

# no interaction Weektype

no_interact_2 <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[1:24], "`"), 
                        collapse = ", "), ") ~ Household + Weektype + Season")
)

no_interact_res_2 <- manova(no_interact_2, data = sM)

# interaction Weektype

interact_2 <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[1:24], "`"), 
                        collapse = ", "), ") ~ Household * Weektype * Season")
)

interact_res_2 <- manova(interact_2, data = sM)


no_interact_2_b <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[25:48], "`"), 
                        collapse = ", "), ") ~ Household + Weektype + Season")
)

no_interact_2_b_res <- manova(no_interact_2_b, data = sM)

interact_2_b <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[25:48], "`"), 
                        collapse = ", "), ") ~ Household * Weektype * Season")
)

interact_2_b_res <- manova(interact_2_b, data = sM)

# computation of AIC

aik <- function(l){
  res <- l$residuals
  n <- nrow(l$residuals)
  d <- 24
  kov <- (n-1)/n * cov(l$residuals)
  ende <- sapply(1:n, function(x) t(res[x,])%*%solve(kov, tol = 1e-20)%*%res[x,])
  logli <- -n*d*log(2*pi)/2 - n*log(det(kov))/2 - 1/2 * sum(ende)
  q <- length(summary(l)$row.names) - 1
  k <- q*d + d*(d+1)/2
  aiki <- 2*k - 2*logli
  return(aiki)
}

# comparison of AIC values for interaction and no interaction models (smoothed
# values in a)
aik(no_interact_res_2)
aik(interact_res_2)
# 2793157 versus 2778913 >> interaction model is better (lower AIC)

# comparison of AIC values for interaction and no interaction models (smoothed
# values in b)
aik(no_interact_2_b_res)
aik(interact_2_b_res)
# 2557623 versus 2522469 >> interaction model is better (lower AIC)


################################################################################
################################# problem no. 3 ################################


# for a)
interact_h <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[1:24], "`"), 
                        collapse = ", "), ") ~ Weektype * Season")
)
no_interact_h <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[1:24], "`"), 
                        collapse = ", "), ") ~ Weektype + Season")
)

model_inter_a <- list(1)
for(i in rs){
  model_inter_a[[which(i == sort(rs))]] <- manova(interact_h, 
                                                  data = sM[which(sM$Household == i),])
}

model_nointer_a <- list(1)
for(i in rs){
  model_nointer_a[[which(i == sort(rs))]] <- manova(no_interact_h, 
                                                    data = sM[which(sM$Household == i),])
}

# for b)
interact_h_b <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[25:48], "`"), 
                        collapse = ", "), ") ~ Weektype * Season")
)

no_interact_h_b <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM)[25:48], "`"), 
                        collapse = ", "), ") ~ Weektype + Season")
)

model_inter_b <- list(1)
for(i in rs){
  model_inter_b[[which(i == sort(rs))]] <- manova(interact_h_b, 
                                                  data = sM[which(sM$Household == i),])
}

model_nointer_b <- list(1)
for(i in rs){
  model_nointer_b[[which(i == sort(rs))]] <- manova(no_interact_h_b, 
                                                    data = sM[which(sM$Household == i),])
}

# AIC values for each household separately, smoothed values from a), interaction
aic_inter_a <- rep(1, 30)
for(i in 1:30){
  aic_inter_a[i] <- aik(model_inter_a[[i]])
}
# AIC values for each household separately, smoothed values from a), no interaction
aic_nointer_a <- rep(1,30)
for(i in 1:30){
  aic_nointer_a[i] <- aik(model_nointer_a[[i]])
}

# AIC values for each household separately, smoothed values from b), interaction
aic_inter_b <- rep(1,30)
for(i in 1:30){
  aic_inter_b[i] <- aik(model_inter_b[[i]])
}
# AIC values for each household separately, smoothed values from b), no interaction
aic_nointer_b <- rep(1,30)
for(i in 1:30){
  aic_nointer_b[i] <- aik(model_nointer_b[[i]])
}

# difference in the AIC values (smoothed values from a))
sum(aic_inter_a - aic_nointer_a > 0)
# 15 models where no interaction is better

# difference in the AIC values (smoothed values from b))
sum(aic_inter_b - aic_nointer_b < 0)
# 29 models where interaction is better


################################################################################
################################# problem no. 4 ################################


coeff_matrix_a_nointer <- matrix(rep(0,1440), nrow = 30)
for(i in 1:30){
  coeff_matrix_a_nointer[i,] <- c(as.numeric(model_nointer_a[[i]]$coefficients[2,]), 
                                  as.numeric(model_nointer_a[[i]]$coefficients[3,]))
}

fviz_nbclust(coeff_matrix_a_nointer, kmeans, method = "silhouette")
# k = 3

# nstart = 25 is the standard recommendation
clust_a_nointer <- kmeans(coeff_matrix_a_nointer, centers = 3, nstart = 25)
# visualization of the clustering
fviz_cluster(clust_a_nointer, data = coeff_matrix_a_nointer, pallette = "jco", 
             ggtheme = theme_minimal())

index.DB(coeff_matrix_a_nointer, clust_a_nointer$cluster)$DB
# 1.070489
dunn(D = dist(coeff_matrix_a_nointer), clusters = clust_a_nointer$cluster)
# 0.1595234

coeff_matrix_a_inter <- matrix(rep(0,2160), nrow = 30)
for(i in 1:30){
  coeff_matrix_a_inter[i,] <- c(as.numeric(model_inter_a[[i]]$coefficients[2,]), 
                                as.numeric(model_inter_a[[i]]$coefficients[3,]),
                                as.numeric(model_inter_a[[i]]$coefficients[4,]))
}

fviz_nbclust(coeff_matrix_a_inter, kmeans, method = "silhouette")
# k = 2

# nstart = 25 is the standard recommendation
clust_a_inter <- kmeans(coeff_matrix_a_inter, centers = 2, nstart = 25)
# visualization of the clustering
fviz_cluster(clust_a_inter, data = coeff_matrix_a_inter, pallette = "jco", 
             ggtheme = theme_minimal())

index.DB(coeff_matrix_a_inter, clust_a_inter$cluster)$DB
# 1.760764
dunn(D = dist(coeff_matrix_a_inter), clusters = clust_a_inter$cluster)
# 0.1670974 (better)

rm(clust_a_nointer, coeff_matrix_a_nointer)

######################################### for the smoothed values in b)
coeff_matrix_b_nointer <- matrix(rep(0,1440), nrow = 30)
for(i in 1:30){
  coeff_matrix_b_nointer[i,] <- c(as.numeric(model_nointer_b[[i]]$coefficients[2,]), 
                                  as.numeric(model_nointer_b[[i]]$coefficients[3,]))
}

fviz_nbclust(coeff_matrix_b_nointer, kmeans, method = "silhouette")
# k = 2

# nstart = 25 is the standard recommendation
clust_b_nointer <- kmeans(coeff_matrix_b_nointer, centers = 2, nstart = 25)
# visualization of the clustering
fviz_cluster(clust_b_nointer, data = coeff_matrix_b_nointer, pallette = "jco", 
             ggtheme = theme_minimal())

index.DB(coeff_matrix_b_nointer, clust_b_nointer$cluster)$DB
# 1.738703
dunn(D = dist(coeff_matrix_b_nointer), clusters = clust_b_nointer$cluster)
# 0.1148332

coeff_matrix_b_inter <- matrix(rep(0,2160), nrow = 30)
for(i in 1:30){
  coeff_matrix_b_inter[i,] <- c(as.numeric(model_inter_b[[i]]$coefficients[2,]), 
                                as.numeric(model_inter_b[[i]]$coefficients[3,]),
                                as.numeric(model_inter_b[[i]]$coefficients[4,]))
}

fviz_nbclust(coeff_matrix_b_inter, kmeans, method = "silhouette")
# k = 2

# nstart = 25 is the standard recommendation
clust_b_inter <- kmeans(coeff_matrix_b_inter, centers = 2, nstart = 25)
# visualization of the clustering
fviz_cluster(clust_b_inter, data = coeff_matrix_b_inter, pallette = "jco", 
             ggtheme = theme_minimal())

index.DB(coeff_matrix_b_inter, clust_b_inter$cluster)$DB
# 1.762704 (worse)
dunn(D = dist(coeff_matrix_b_inter), clusters = clust_b_inter$cluster)
# 0.1559164 (worse)

rm(clust_b_inter, coeff_matrix_b_inter)

###################### check for weekday #######################################
sM_nonweekend <- sM[which(sM$Weektype == 1),]
sM_weekend <- sM[which(sM$Weektype == 2),]
# smoothed values of a)
non_weekend <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM_nonweekend)[1:24], "`"), 
                        collapse = ", "), ") ~ Season")
)
weekend <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM_weekend)[1:24], "`"), 
                        collapse = ", "), ") ~ Season")
)

model_nonweekend_a <- list(1)
for(i in rs){
  model_nonweekend_a[[which(i == sort(rs))]] <- manova(
    non_weekend, 
    data = sM_nonweekend[which(sM_nonweekend$Household == i),])
}


coeff_matrix_nonweekend_a <- matrix(rep(0,720), nrow = 30)
for(i in 1:30){
  coeff_matrix_nonweekend_a[i,] <- as.numeric(model_nonweekend_a[[i]]$coefficients[2,])
}

fviz_nbclust(coeff_matrix_nonweekend_a, kmeans, method = "silhouette")
# k = 2

# nstart = 25 is the standard recommendation
clust_nonweekend_a <- kmeans(coeff_matrix_nonweekend_a, centers = 2, nstart = 25)

fviz_cluster(clust_nonweekend_a, data = coeff_matrix_nonweekend_a, pallette = "jco", 
             ggtheme = theme_minimal())

model_weekend_a <- list(1)
for(i in rs){
  model_weekend_a[[which(i == sort(rs))]] <- manova(
    weekend, data = sM_weekend[which(sM_weekend$Household == i),])
}

coeff_matrix_weekend_a <- matrix(rep(0,720), nrow = 30)
for(i in 1:30){
  coeff_matrix_weekend_a[i,] <- as.numeric(model_weekend_a[[i]]$coefficients[2,])
}

fviz_nbclust(coeff_matrix_weekend_a, kmeans, method = "silhouette")
# k = 3

# nstart = 25 is the standard recommendation
clust_weekend_a <- kmeans(coeff_matrix_weekend_a, centers = 3, nstart = 25)

fviz_cluster(clust_weekend_a, data = coeff_matrix_weekend_a, pallette = "jco", 
             ggtheme = theme_minimal())

# db index and dunn index
index.DB(coeff_matrix_nonweekend_a, clust_nonweekend_a$cluster)$DB
# 0.1597573
index.DB(coeff_matrix_weekend_a, clust_weekend_a$cluster)$DB
# 0.4851963
dunn(D = dist(coeff_matrix_nonweekend_a), clusters = clust_nonweekend_a$cluster)
# 2.332575 
dunn(D = dist(coeff_matrix_weekend_a), clusters = clust_weekend_a$cluster)
# 0.827543 

# smoothed values of b)
non_weekend_b <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM_nonweekend)[25:48], "`"), 
                        collapse = ", "), ") ~ Season")
)
weekend_b <- as.formula(
  paste("cbind(", paste(paste0("`", names(sM_weekend)[25:48], "`"), 
                        collapse = ", "), ") ~ Season")
)

model_nonweekend_b <- list(1)
for(i in rs){
  model_nonweekend_b[[which(i == sort(rs))]] <- manova(
    non_weekend_b, data = sM_nonweekend[which(sM_nonweekend$Household == i),])
}

coeff_matrix_nonweekend_b <- matrix(rep(0,720), nrow = 30)
for(i in 1:30){
  coeff_matrix_nonweekend_b[i,] <- as.numeric(model_nonweekend_b[[i]]$coefficients[2,])
}

fviz_nbclust(coeff_matrix_nonweekend_b, kmeans, method = "silhouette")
# k = 2

# nstart = 25 is the standard recommendation
clust_nonweekend_b <- kmeans(coeff_matrix_nonweekend_b, centers = 2, nstart = 25)

fviz_cluster(clust_nonweekend_b, data = coeff_matrix_nonweekend_b, pallette = "jco", 
             ggtheme = theme_minimal())

model_weekend_b <- list(1)
for(i in rs){
  model_weekend_b[[which(i == sort(rs))]] <- manova(
    weekend_b, data = sM_weekend[which(sM_weekend$Household == i),])
}

coeff_matrix_weekend_b <- matrix(rep(0,720), nrow = 30)
for(i in 1:30){
  coeff_matrix_weekend_b[i,] <- as.numeric(model_weekend_b[[i]]$coefficients[2,])
}

fviz_nbclust(coeff_matrix_weekend_b, kmeans, method = "silhouette")
# k = 3

# nstart = 25 is the standard recommendation
clust_weekend_b <- kmeans(coeff_matrix_weekend_b, centers = 3, nstart = 25)

fviz_cluster(clust_weekend_b, data = coeff_matrix_weekend_b, pallette = "jco", 
             ggtheme = theme_minimal())
index.DB(coeff_matrix_nonweekend_b, clust_nonweekend_b$cluster)$DB
# 0.1562603
index.DB(coeff_matrix_weekend_b, clust_weekend_b$cluster)$DB
# 1.134467
dunn(D = dist(coeff_matrix_nonweekend_b), clusters = clust_nonweekend_b$cluster)
# 2.476568 (good)
dunn(D = dist(coeff_matrix_weekend_b), clusters = clust_weekend_b$cluster)
# 0.127087 (bad)


################################################################################
########################### weekday clustering #################################


# visualization of AIC

# Create data frame for bar graph
aic_df <- data.frame(
  Model = factor(c("No Interaction (A)", "Interaction (A)", 
                   "No Interaction (B)", "Interaction (B)"),
                 levels = c("No Interaction (A)", "Interaction (A)", 
                            "No Interaction (B)", "Interaction (B)")),
  AIC = c(aik(no_interact_res_2), 
          aik(interact_res_2), 
          aik(no_interact_2_b_res), 
          aik(interact_2_b_res))
)

# Horizontal gradient bar plot
ggplot(aic_df, aes(x = Model, y = AIC, fill = AIC)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(y = AIC / 2, label = round(AIC, 0)), 
            color = "white", size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "AIC Comparison Across Models",
       x = NULL, y = "AIC Value") +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.text = element_text(color = "black", face = "bold"),
    axis.title = element_text(color = "black", face = "bold"),
    panel.grid.major = element_line(color = "gray40", linetype = "dotted"),
    panel.grid.minor = element_line(color = "gray60", linetype = "dotted")
  )


################################################################################
################################# problem no. 5 ################################


xx <- x[-which(x %in% rs)]
# 3 23 40

nsM <- M[which(M$Household %in% xx),]
nsM$Weektype <- ifelse(nsM$Weekday %in% c("Monday", "Tuesday", "Wednesday", 
                                          "Thursday", "Friday"), 1, 2)
nsM$Weektype <- as.factor(nsM$Weektype)

nsM_nonweekend <- nsM[which(nsM$Weektype == 1),]
nsM_weekend <- nsM[which(nsM$Weektype == 2),]
# smoothed values of a)
non_weekend_3 <- as.formula(
  paste("cbind(", paste(paste0("`", names(nsM_nonweekend)[1:24], "`"), 
                        collapse = ", "), ") ~ Season")
)
weekend_3 <- as.formula(
  paste("cbind(", paste(paste0("`", names(nsM_weekend)[1:24], "`"), 
                        collapse = ", "), ") ~ Season")
)

model_nonweekend_a_3 <- list(1)
for(i in xx){
  model_nonweekend_a_3[[which(i == xx)]] <- manova(
    non_weekend_3, data = nsM_nonweekend[which(nsM_nonweekend$Household == i),])
}

coeff_matrix_nonweekend_a_3 <- matrix(rep(0,72), nrow = 3)
for(i in 1:3){
  coeff_matrix_nonweekend_a_3[i,] <- as.numeric(model_nonweekend_a_3[[i]]$coefficients[2,])
}

# distance to centers for household no. 3 (non-weekend)
sum((coeff_matrix_nonweekend_a_3[1,] - clust_nonweekend_a$centers[1,])^2)
# 2724541
sum((coeff_matrix_nonweekend_a_3[1,] - clust_nonweekend_a$centers[2,])^2)
# 41198.93 cluster 2

# distance to the centers for household no. 23 (non-weekend)
sum((coeff_matrix_nonweekend_a_3[2,] - clust_nonweekend_a$centers[1,])^2)
# 3068516
sum((coeff_matrix_nonweekend_a_3[2,] - clust_nonweekend_a$centers[2,])^2)
# 16915.39 cluster 2

# distance to the centers for household no. 40 (non-weekend)
sum((coeff_matrix_nonweekend_a_3[3,] - clust_nonweekend_a$centers[1,])^2)
# 3434373
sum((coeff_matrix_nonweekend_a_3[3,] - clust_nonweekend_a$centers[2,])^2)
# 52988.85 cluster 2

model_weekend_a_3 <- list(1)
for(i in xx){
  model_weekend_a_3[[which(i == xx)]] <- manova(
    weekend_3, data = nsM_weekend[which(nsM_weekend$Household == i),])
}

coeff_matrix_weekend_a_3 <- matrix(rep(0,72), nrow = 3)
for(i in 1:3){
  coeff_matrix_weekend_a_3[i,] <- as.numeric(model_weekend_a_3[[i]]$coefficients[2,])
}

# distance to centers for household no. 3 (weekend)
sum((coeff_matrix_weekend_a_3[1,] - clust_weekend_a$centers[1,])^2)
# 635355.8
sum((coeff_matrix_weekend_a_3[1,] - clust_weekend_a$centers[2,])^2)
# 37838.69 cluster 2
sum((coeff_matrix_weekend_a_3[1,] - clust_weekend_a$centers[3,])^2)
# 2597032

# distance to the centers for household no. 23 (weekend)
sum((coeff_matrix_weekend_a_3[2,] - clust_weekend_a$centers[1,])^2)
# 707836.1
sum((coeff_matrix_weekend_a_3[2,] - clust_weekend_a$centers[2,])^2)
# 9321.251 cluster 2
sum((coeff_matrix_weekend_a_3[2,] - clust_weekend_a$centers[3,])^2)
# 2290884

# distance to the centers for household no. 40 (weekend)
sum((coeff_matrix_weekend_a_3[3,] - clust_weekend_a$centers[1,])^2)
# 256180.4 cluster 1
sum((coeff_matrix_weekend_a_3[3,] - clust_weekend_a$centers[2,])^2)
# 303932.2
sum((coeff_matrix_weekend_a_3[3,] - clust_weekend_a$centers[3,])^2)
# 3747364

# smoothed values of b)
non_weekend_b_3 <- as.formula(
  paste("cbind(", paste(paste0("`", names(nsM_nonweekend)[25:48], "`"), 
                        collapse = ", "), ") ~ Season")
)
weekend_b_3 <- as.formula(
  paste("cbind(", paste(paste0("`", names(nsM_weekend)[25:48], "`"), 
                        collapse = ", "), ") ~ Season")
)

model_nonweekend_b_3 <- list(1)
for(i in xx){
  model_nonweekend_b_3[[which(i == xx)]] <- manova(
    non_weekend_b_3, data = nsM_nonweekend[which(nsM_nonweekend$Household == i),])
}

coeff_matrix_nonweekend_b_3 <- matrix(rep(0,72), nrow = 3)
for(i in 1:3){
  coeff_matrix_nonweekend_b_3[i,] <- as.numeric(model_nonweekend_b_3[[i]]$coefficients[2,])
}

# distance to centers for household no. 3 (non-weekend)
sum((coeff_matrix_nonweekend_b_3[1,] - clust_nonweekend_b$centers[1,])^2)
# 39043.45 cluster 1
sum((coeff_matrix_nonweekend_b_3[1,] - clust_nonweekend_b$centers[2,])^2)
# 2639325

# distance to the centers for household no. 23 (non-weekend)
sum((coeff_matrix_nonweekend_b_3[2,] - clust_nonweekend_b$centers[1,])^2)
# 14317.47 cluster 1
sum((coeff_matrix_nonweekend_b_3[2,] - clust_nonweekend_b$centers[2,])^2)
# 3014395

# distance to the centers for household no. 40  (non-weekend)
sum((coeff_matrix_nonweekend_b_3[3,] - clust_nonweekend_b$centers[1,])^2)
# 51814.09 cluster 1
sum((coeff_matrix_nonweekend_b_3[3,] - clust_nonweekend_b$centers[2,])^2)
# 3347935

model_weekend_b_3 <- list(1)
for(i in xx){
  model_weekend_b_3[[which(i == xx)]] <- manova(
    weekend_b_3, data = nsM_weekend[which(nsM_weekend$Household == i),])
}

coeff_matrix_weekend_b_3 <- matrix(rep(0,72), nrow = 3)
for(i in 1:3){
  coeff_matrix_weekend_b_3[i,] <- as.numeric(model_weekend_b_3[[i]]$coefficients[2,])
}

# distance to centers for household no. 3 (weekend)
sum((coeff_matrix_weekend_b_3[1,] - clust_weekend_b$centers[1,])^2)
# 71899.3 
sum((coeff_matrix_weekend_b_3[1,] - clust_weekend_b$centers[2,])^2)
# 57773.59 cluster 2
sum((coeff_matrix_weekend_b_3[1,] - clust_weekend_b$centers[3,])^2)
# 2452643

# distance to the centers for household no. 23 (weekend)
sum((coeff_matrix_weekend_b_3[2,] - clust_weekend_b$centers[1,])^2)
# 81775.67
sum((coeff_matrix_weekend_b_3[2,] - clust_weekend_b$centers[2,])^2)
# 15367.59 cluster 2
sum((coeff_matrix_weekend_b_3[2,] - clust_weekend_b$centers[3,])^2)
# 2210444

# distance to the centers for household no. 40 (weekend)
sum((coeff_matrix_weekend_b_3[3,] - clust_weekend_b$centers[1,])^2)
# 104655.8 cluster 1
sum((coeff_matrix_weekend_b_3[3,] - clust_weekend_b$centers[2,])^2)
# 387031.9
sum((coeff_matrix_weekend_b_3[3,] - clust_weekend_b$centers[3,])^2)
# 3544498

clustering <- matrix(c(2,2,2, 2,2,1, 1,1,1, 2,2,1),nrow = 3, ncol = 4)
colnames(clustering) <- c("nonweekend a", "weekend a", "nonweekend b", "weekend b")
rownames(clustering) <- c("hh 3", "hh 23", "hh 40")


## visualization of remaining households in our clusters
plot_cluster_projection <- function(trained_coeff, 
                                    new_coeff, 
                                    trained_kmeans, 
                                    trained_ids, 
                                    new_ids, 
                                    title = "Cluster Projection") {
  # Combine coefficient matrices
  all_coeff <- rbind(trained_coeff, new_coeff)
  
  # PCA on trained data
  pca_res <- prcomp(trained_coeff, scale. = TRUE)
  
  # Project both trained and new into PCA space
  pca_proj_all <- predict(pca_res, newdata = all_coeff)[, 1:2]
  df_plot <- as.data.frame(pca_proj_all)
  colnames(df_plot) <- c("PC1", "PC2")
  
  # Assign clusters for new data based on min distance to centers
  assign_cluster <- function(new_row, centers) {
    which.min(apply(centers, 1, function(center) sum((new_row - center)^2)))
  }
  new_clusters <- apply(new_coeff, 1, assign_cluster, centers = trained_kmeans$centers)
  
  # Build metadata
  df_plot$Cluster <- factor(c(trained_kmeans$cluster, new_clusters))
  # Compute Dunn Index on the combined coefficient matrix
  dunn_index <- dunn(D = dist(all_coeff), clusters = as.numeric(df_plot$Cluster))
  # Format Dunn Index with color
  dunn_val <- round(dunn_index, 3)
  dunn_color <- ifelse(dunn_val < 1, "red", "green")
  
  # Create HTML-formatted subtitle
  subtitle_text <- paste0(
    "Projection of New Households in Cluster Space<br>",
    "<b style='color:", dunn_color, "'>Dunn Index = ", dunn_val, "</b>"
  )
  df_plot$Type <- factor(c(rep("Trained", nrow(trained_coeff)), rep("New", nrow(new_coeff))))
  df_plot$Household <- c(trained_ids, new_ids)
  
  # Determine cluster sizes
  cluster_sizes <- table(df_plot$Cluster)
  
  # Identify singleton clusters
  singleton_clusters <- names(cluster_sizes[cluster_sizes == 1])
  
  # Set base colors
  cluster_levels <- levels(df_plot$Cluster)
  default_colors <- c("#191970", "#228B22", "#B22222", "#9932CC")
  names(default_colors) <- cluster_levels
  
  # Override singleton cluster colors with orangered
  default_colors[singleton_clusters] <- "orangered1"
  
  # Plot
  ggplot(df_plot, aes(x = PC1, y = PC2, color = Cluster, shape = Type)) +
    geom_point(
      data = df_plot %>% filter(Type == "Trained"),
      size = 3,
      alpha = 0.8
    ) +
    geom_point(
      data = df_plot %>% filter(Type == "New"),
      aes(fill = Cluster),
      color = "red",         # red border
      size = 3,
      stroke = 1,
      shape = 24, # filled triangle with border
      show.legend = FALSE
    ) +
    # Labels for NEW households (bold)
    ggrepel::geom_text_repel(
      data = df_plot %>% filter(Type == "New"),
      aes(label = Household),
      size = 4.5,
      fontface = "bold",
      box.padding = 0.15,
      point.padding = 0.1,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.color = 'grey50',
      show.legend = FALSE
    ) +
    # Labels for TRAINED households (regular)
    ggrepel::geom_text_repel(
      data = df_plot %>% filter(Type == "Trained"),
      aes(label = Household),
      size = 3.5,
      fontface = "plain",
      box.padding = 0.15,
      point.padding = 0.1,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.color = 'grey50',
      show.legend = FALSE
    ) +
    scale_shape_manual(
      values = c("New" = 17, "Trained" = 1),
      name = "Type",
      labels = c("New", "Trained")
    ) +
    scale_color_manual(
      values = default_colors,
      name = "Cluster",
      labels = paste0("Cluster ", cluster_levels)
    ) +
    scale_fill_manual(
      values = default_colors
    ) +
    # Turn off shape guide for Cluster
    guides(
      fill = "none",  # hide fill legend
      color = guide_legend(
        override.aes = list(shape = 16, linetype = "blank")
      )
    ) +
    # Light dotted circle around Household 34
    ggforce::geom_circle(
      data = df_plot %>% filter(Household == 34),
      aes(x0 = PC1, y0 = PC2, r = 1),  # Adjust radius if needed
      color = "orangered1",
      linetype = "dotted",
      linewidth = 0.8,
      inherit.aes = FALSE,
      alpha = 0.1
    ) +
    labs(
      title = title,
      subtitle = subtitle_text,
      x = "Principal Component 1",
      y = "Principal Component 2"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = ggtext::element_markdown(face = "bold", size = 14),
      axis.title.x = element_text(face = "bold", size = 14),
      axis.title.y = element_text(face = "bold", size = 14),
      axis.text.x = element_text(face = "bold", size = 12),
      axis.text.y = element_text(face = "bold", size = 12),
      legend.title = element_text(face = "bold", size = 13),
      legend.text = element_text(face = "bold", size = 12),
      legend.key.size = unit(1, "cm"),
      legend.spacing.y = unit(0.5, "cm"),
      legend.position = c(0.8, 0.8),
      legend.justification = c("left", "top"),
      legend.box.margin = margin(10, 10, 10, 10),
      
      # Optional Add-on: Enhance grid, axis ticks and lines
      panel.grid.major = element_line(size = 0.4),
      panel.grid.minor = element_line(size = 0.2),
      axis.ticks = element_line(size = 0.6),
      axis.line = element_line(size = 0.8, colour = "black")
    )
}

plot_cluster_projection(coeff_matrix_nonweekend_a, 
                        coeff_matrix_nonweekend_a_3, 
                        clust_nonweekend_a, 
                        sort(rs), 
                        xx, 
                        title = "Part A – Weekday Cluster Projection")

plot_cluster_projection(coeff_matrix_weekend_a, 
                        coeff_matrix_weekend_a_3, 
                        clust_weekend_a, 
                        sort(rs), 
                        xx, 
                        title = "Part A – Weekend Cluster Projection")

plot_cluster_projection(coeff_matrix_nonweekend_b, 
                        coeff_matrix_nonweekend_b_3,
                        clust_nonweekend_b, 
                        sort(rs), 
                        xx, 
                        title = "Part B – Weekday Cluster Projection")

plot_cluster_projection(coeff_matrix_weekend_b, 
                        coeff_matrix_weekend_b_3, 
                        clust_weekend_b, 
                        sort(rs), 
                        xx, 
                        title = "Part B – Weekend Cluster Projection")

p1 <- plot_cluster_projection(coeff_matrix_nonweekend_a, coeff_matrix_nonweekend_a_3, clust_nonweekend_a, sort(rs), xx, title = "Part A – Weekday Cluster Projection")
p2 <- plot_cluster_projection(coeff_matrix_weekend_a, coeff_matrix_weekend_a_3, clust_weekend_a, sort(rs), xx, title = "Part A – Weekend Cluster Projection")
p3 <- plot_cluster_projection(coeff_matrix_nonweekend_b, coeff_matrix_nonweekend_b_3, clust_nonweekend_b, sort(rs), xx, title = "Part B – Weekday Cluster Projection")
p4 <- plot_cluster_projection(coeff_matrix_weekend_b, coeff_matrix_weekend_b_3, clust_weekend_b, sort(rs), xx, title = "Part B – Weekend Cluster Projection")

ggsave("E:/case_study/project_1/cluster_weekday_a.png", plot = p1, width = 8, height = 6, dpi = 100)
ggsave("E:/case_study/project_1/cluster_weekend_a.png", plot = p2, width = 8, height = 6, dpi = 100)
ggsave("E:/case_study/project_1/cluster_weekday_b.png", plot = p3, width = 8, height = 6, dpi = 100)
ggsave("E:/case_study/project_1/cluster_weekend_b.png", plot = p4, width = 8, height = 6, dpi = 100)


################################################################################
################################# problem no. 6 ################################


# smoothed values a, non-weekend
sort(rs)[which(clust_nonweekend_a$cluster == 1)]
sort(rs)[which(clust_nonweekend_a$cluster == 2)]
d <- 1:365

loadprofiling <- function(x,y, weektype = "weekend"){
  days <- c(5,6) + 7*rep(0:51, each = 2)
  if(weektype == "nonweekend"){days <- (1:365)[-days]}
  clustno <- max(x$cluster)
  loadp <- matrix(nrow = 365*clustno, ncol = 24)
  for(d in 1:365){
    for(h in 1:24){
      hour <- matrix(nrow = 30, ncol = 2)
      for(i in 1:30){
      hour[i,2] <- x$cluster[i]
      hour[i,1] <-  y[[i]]$coefficients[1,h] + 
        sin((d - 31 - 28 - 21)*2*pi / 365)*y[[i]]$coefficients[2,h]
      }
      for(k in 1:clustno){
        if(!(d %in% days)){
          loadp[d+365*(k-1),h] <- NA
        }else{
          loadp[d+365*(k-1),h] <- mean(hour[which(hour[,2] == k)]) 
        }
      }
    }
  }
  
  return(loadp)
}

nonweekend_a_lp <- loadprofiling(clust_nonweekend_a, model_nonweekend_a, "nonweekend")
weekend_a_lp <- loadprofiling(clust_weekend_a, model_weekend_a)
nonweekend_b_lp <- loadprofiling(clust_nonweekend_b, model_nonweekend_b, "nonweekend")
weekend_b_lp <- loadprofiling(clust_weekend_b, model_weekend_b)

##### standard error
lpsd <- function(lp, cluster, weektype = "nonweekend", method = "a"){
  clustno <- max(cluster$cluster)
  if(method == "a"){
    X <- sM[,-c(25:48)]
  }else{
    X <- sM[,-c(1:24)]
  }
  days <- c(5,6) + 7*rep(0:51, each = 2)
  sdlp <- matrix(nrow = 365*clustno, ncol = 24)
  if(weektype == "nonweekend"){days <- (1:365)[-days]}
  for(d in 1:365){
    if(d %in% days){
      for(h in 1:24){
        v <- matrix(nrow = 30, ncol = 2)
        for(i in 1:30){
          x <- X[which(X$Household == sort(rs)[i]),]
          cnumber <- cluster$cluster[i] 
          v[i,1] <- (x[d,h] - lp[d+365*(cnumber-1),h])^2
          v[i,2] <- cluster$cluster[i]
        }
        for(k in 1:clustno){
            sdlp[d+365*(k-1),h] <- sqrt(mean(v[which(v[,2] == k)], na.rm = TRUE))
        }
      } 
    }
  }
  return(sdlp)
}

nonweekend_a_sd <- lpsd(nonweekend_a_lp, clust_nonweekend_a)
nonweekend_a_sd[which(is.nan(nonweekend_a_sd))] <- 0
weekend_a_sd <- lpsd(weekend_a_lp, clust_weekend_a, weektype = "weekend")
weekend_a_sd[which(is.nan(weekend_a_sd))] <- 0
nonweekend_b_sd <- lpsd(nonweekend_b_lp, clust_nonweekend_b, method = "b")
nonweekend_b_sd[which(is.nan(nonweekend_b_sd))] <- 0
weekend_b_sd <- lpsd(weekend_b_lp, clust_weekend_b, weektype = "weekend", method = "b")
weekend_b_sd[which(is.nan(weekend_b_sd))] <- 0

# loadprofiles for a) smoothed values
# household no. 3
lp1a <- nonweekend_a_lp[((clustering[1,1]-1)*365+1):(clustering[1,1]*365),]
lp1a[which(is.na(lp1a))] <- weekend_a_lp[((clustering[1,2]-1)*365+1):(
  clustering[1,2]*365),][which(is.na(weekend_a_lp[((clustering[1,2]-1)*365+1):(
    clustering[1,2]*365),]) == FALSE)]
# household no. 23
lp2a <- nonweekend_a_lp[((clustering[2,1]-1)*365+1):(clustering[2,1]*365),]
lp2a[which(is.na(lp2a))] <- weekend_a_lp[((clustering[2,2]-1)*365+1):(
  clustering[2,2]*365),][which(is.na(weekend_a_lp[((clustering[2,2]-1)*365+1):(
    clustering[2,2]*365),]) == FALSE)]
# household no. 40
lp3a <- nonweekend_a_lp[((clustering[3,1]-1)*365+1):(clustering[3,1]*365),]
lp3a[which(is.na(lp3a))] <- weekend_a_lp[((clustering[3,2]-1)*365+1):(
  clustering[3,2]*365),][which(is.na(weekend_a_lp[((clustering[3,2]-1)*365+1):(
    clustering[3,2]*365),]) == FALSE)]

# loadprofiles for b) smoothed values
# household no. 3
lp1b <- nonweekend_b_lp[((clustering[1,3]-1)*365+1):(clustering[1,3]*365),]
lp1b[which(is.na(lp1b))] <- weekend_b_lp[((clustering[1,4]-1)*365+1):(
  clustering[1,4]*365),][which(is.na(weekend_b_lp[((clustering[1,4]-1)*365+1):(
    clustering[1,4]*365),]) == FALSE)]
# household no. 23
lp2b <- nonweekend_b_lp[((clustering[2,3]-1)*365+1):(clustering[2,3]*365),]
lp2b[which(is.na(lp2b))] <- weekend_b_lp[((clustering[2,4]-1)*365+1):(
  clustering[2,4]*365),][which(is.na(weekend_b_lp[((clustering[2,4]-1)*365+1):(
    clustering[2,4]*365),]) == FALSE)]
# household no. 40
lp3b <- nonweekend_b_lp[((clustering[3,3]-1)*365+1):(clustering[3,3]*365),]
lp3b[which(is.na(lp3b))] <- weekend_b_lp[((clustering[3,4]-1)*365+1):(
  clustering[3,4]*365),][which(is.na(weekend_b_lp[((clustering[3,4]-1)*365+1):(
    clustering[3,4]*365),]) == FALSE)]

## standard deviation for a) smoothed values
# household no. 3
sd1a <- nonweekend_a_sd[((clustering[1,1]-1)*365+1):(clustering[1,1]*365),]
sd1a[which(is.na(sd1a))] <- weekend_a_sd[((clustering[1,2]-1)*365+1):(
  clustering[1,2]*365),][which(is.na(weekend_a_sd[((clustering[1,2]-1)*365+1):(
    clustering[1,2]*365),]) == FALSE)]
# household no. 23
sd2a <- nonweekend_a_sd[((clustering[2,1]-1)*365+1):(clustering[2,1]*365),]
sd2a[which(is.na(sd2a))] <- weekend_a_sd[((clustering[2,2]-1)*365+1):(
  clustering[2,2]*365),][which(is.na(weekend_a_sd[((clustering[2,2]-1)*365+1):(
    clustering[2,2]*365),]) == FALSE)]
# household no. 40
sd3a <- nonweekend_a_sd[((clustering[3,1]-1)*365+1):(clustering[3,1]*365),]
sd3a[which(is.na(sd3a))] <- weekend_a_sd[((clustering[3,2]-1)*365+1):(
  clustering[3,2]*365),][which(is.na(weekend_a_sd[((clustering[3,2]-1)*365+1):(
    clustering[3,2]*365),]) == FALSE)]

# standard deviation for b) smoothed values
# household no. 3
sd1b <- nonweekend_b_sd[((clustering[1,3]-1)*365+1):(clustering[1,3]*365),]
sd1b[which(is.na(sd1b))] <- weekend_b_sd[((clustering[1,4]-1)*365+1):(
  clustering[1,4]*365),][which(is.na(weekend_b_sd[((clustering[1,4]-1)*365+1):(
    clustering[1,4]*365),]) == FALSE)]
# household no. 23
sd2b <- nonweekend_b_sd[((clustering[2,3]-1)*365+1):(clustering[2,3]*365),]
sd2b[which(is.na(sd2b))] <- weekend_b_sd[((clustering[2,4]-1)*365+1):(
  clustering[2,4]*365),][which(is.na(weekend_b_sd[((clustering[2,4]-1)*365+1):(n
    clustering[2,4]*365),]) == FALSE)]
# household no. 40
sd3b <- nonweekend_b_sd[((clustering[3,3]-1)*365+1):(clustering[3,3]*365),]
sd3b[which(is.na(sd3b))] <- weekend_b_sd[((clustering[3,4]-1)*365+1):(
  clustering[3,4]*365),][which(is.na(weekend_b_lp[((clustering[3,4]-1)*365+1):(
    clustering[3,4]*365),]) == FALSE)]


##### visualization #####
###### Weekend (Sunday), household no. 3 ######
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(1, 1, 4, 1))  # 'oma' is outer margin for title

# ----- Plot 1 -----
plot(0:23, lp1a[34,], type = "l", col = "blue", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 3 A")
polygon(c(0:23, rev(0:23)),
        c(lp1a[34,] + sd1a[34,], rev(lp1a[34,] - sd1a[34,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp1a[34,], col = "blue", lwd = 2)

# ----- Plot 2 -----
plot(0:23, lp1b[34,], type = "l", col = "red", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 3 B")
polygon(c(0:23, rev(0:23)),
        c(lp1b[34,] + sd1b[34,], rev(lp1b[34,] - sd1b[34,])),
        col = rgb(1, 0, 0, 0.1), border = NA)
lines(0:23, lp1b[34,], col = "red", lwd = 2)

# ----- Plot 3 -----
plot(0:23, lp2a[34,], type = "l", col = "blue", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 23 A")
polygon(c(0:23, rev(0:23)),
        c(lp2a[34,] + sd2a[34,], rev(lp2a[34,] - sd2a[34,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp2a[34,], col = "blue", lwd = 2)

# ----- Plot 4 -----
plot(0:23, lp2b[34,], type = "l", col = "red", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 23 B")
polygon(c(0:23, rev(0:23)),
        c(lp2b[34,] + sd2b[34,], rev(lp2b[34,] - sd2b[34,])),
        col = rgb(1, 0, 0, 0.1), border = NA)
lines(0:23, lp2b[34,], col = "red", lwd = 2)

# ----- Plot 5 -----
plot(0:23, lp3a[34,], type = "l", col = "blue", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 40 A")
polygon(c(0:23, rev(0:23)),
        c(lp3a[34,] + sd3a[34,], rev(lp3a[34,] - sd3a[34,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp3a[34,], col = "blue", lwd = 2)

# ----- Plot 6 -----
plot(0:23, lp3b[34,], type = "l", col = "red", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 40 B")
polygon(c(0:23, rev(0:23)),
        c(lp3b[34,] + sd3b[34,], rev(lp3b[34,] - sd3b[34,])),
        col = rgb(1, 0, 0, 0.1), border = NA)
lines(0:23, lp3b[34,], col = "red", lwd = 2)

# ---- Add Main Title ----
mtext("Weekend – ±1 SD Range & Mean Load by Model", outer = TRUE, cex = 1.5, font = 2)

# ---- Custom Legend (add it outside plot area) ----
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")


###### Weekday (Monday), household no. 3 ######
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(1, 1, 4, 1))  # 'oma' is outer margin for title

# ----- Plot 1 -----
plot(0:23, lp1a[35,], type = "l", col = "blue", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 3 A")
polygon(c(0:23, rev(0:23)),
        c(lp1a[35,] + sd1a[35,], rev(lp1a[35,] - sd1a[35,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp1a[35,], col = "blue", lwd = 2)

# ----- Plot 2 -----
plot(0:23, lp1b[35,], type = "l", col = "red", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 3 B")
polygon(c(0:23, rev(0:23)),
        c(lp1b[35,] + sd1b[35,], rev(lp1b[35,] - sd1b[35,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp1b[35,], col = "red", lwd = 2)

# ----- Plot 3 -----
plot(0:23, lp2a[35,], type = "l", col = "blue", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 23 A")
polygon(c(0:23, rev(0:23)),
        c(lp2a[35,] + sd2a[35,], rev(lp2a[35,] - sd2a[35,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp2a[35,], col = "blue", lwd = 2)

# ----- Plot 4 -----
plot(0:23, lp2b[35,], type = "l", col = "red", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 23 B")
polygon(c(0:23, rev(0:23)),
        c(lp2b[35,] + sd2b[35,], rev(lp2b[35,] - sd2b[35,])),
        col = rgb(1, 0, 0, 0.1), border = NA)
lines(0:23, lp2b[35,], col = "red", lwd = 2)

# ----- Plot 5 -----
plot(0:23, lp3a[35,], type = "l", col = "blue", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 40 A")
polygon(c(0:23, rev(0:23)),
        c(lp3a[35,] + sd3a[35,], rev(lp3a[35,] - sd3a[35,])),
        col = rgb(0.2, 0.2, 1, 0.1), border = NA)
lines(0:23, lp3a[35,], col = "blue", lwd = 2)

# ----- Plot 6 -----
plot(0:23, lp3b[35,], type = "l", col = "red", lwd = 2, ylim = c(-200, 1100),
     xlab = "Hour", ylab = "Power", main = "Household 40 B")
polygon(c(0:23, rev(0:23)),
        c(lp3b[35,] + sd3b[35,], rev(lp3b[35,] - sd3b[35,])),
        col = rgb(1, 0, 0, 0.1), border = NA)
lines(0:23, lp3b[35,], col = "red", lwd = 2)

# ---- Add Main Title ----
mtext("Non-weekend – ±1 SD Range & Mean Load by Model", outer = TRUE, cex = 1.5, font = 2)

# ---- Custom Legend (add it outside plot area) ----
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")

######################## again but with proper visuals #########################

obs3a <- M[M$Household == 3, 1:24]
obs3b <- M[M$Household == 3, 25:48]
obs23a <- M[M$Household == 23, 1:24]
obs23b <- M[M$Household == 23, 25:48]
obs40a <- M[M$Household == 40, 1:24]
obs40b <- M[M$Household == 40, 25:48]

plot_single_panel <- function(y, sd_y, obs_y = NULL, title_text, day_index, model_color, sd_fill, file_path = NULL) {
  df <- data.frame(
    Hour = 0:23,
    Mean = y,
    SD_upper = y + sd_y,
    SD_lower = y - sd_y
  )
  
  if (!is.null(obs_y)) {
    df$Observation <- as.numeric(obs_y)
  }
  
  p <- ggplot(df, aes(x = Hour)) +
    # ✅ Confidence band with legend
    geom_ribbon(
      aes(ymin = SD_lower, ymax = SD_upper, fill = "Confidence Band"),
      alpha = 0.3,
      show.legend = TRUE
    ) +
    
    # Mean profile (solid line)
    geom_line(
      aes(y = Mean, linetype = "Mean Profile", color = "Mean Profile"),
      size = 1.3
    )
  
  # Observed data (dashed line)
  if (!is.null(obs_y)) {
    p <- p + geom_line(
      aes(y = Observation, linetype = "Observed", color = "Observed"),
      size = 1.1
    )
  }
  
  # Customize scales
  p <- p +
    scale_color_manual(
      name = "Legend",
      values = c("Mean Profile" = model_color, "Observed" = "black")
    ) +
    scale_linetype_manual(
      name = "Legend",
      values = c("Mean Profile" = "solid", "Observed" = "dashed")
    ) +
    scale_fill_manual(
      name = "Legend",
      values = c("Confidence Band" = sd_fill)
    ) +
    
    scale_x_continuous(breaks = 0:23) +
    labs(
      title = title_text,
      subtitle = paste("Day", day_index),
      x = "Hour",
      y = "Power (W)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(face = "bold", size = 13),
      axis.title = element_text(face = "bold", size = 13),
      axis.text = element_text(face = "bold", size = 11),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(face = "bold", size = 11),
      panel.grid = element_line(size = 0.3),
      legend.position = "top"
    )
  
  print(p)
}




plot_single_panel(
  y = lp1a[34, ],
  sd_y = sd1a[34, ],
  obs_y = obs3a[34, ],
  title_text = "Weekend Load Profiles Cluster 2 HH 3 A",
  day_index = 34,
  model_color = "blue",
  sd_fill = "lightblue"
)

plot_single_panel(
  y = lp1b[34, ],
  sd_y = sd1b[34, ],
  obs_y = obs3b[34, ],
  title_text = "Weekend Load Profiles Cluster 2 HH 3 B",
  day_index = 34,
  model_color = "red",
  sd_fill = "lightpink"
)

plot_single_panel(
  y = lp2a[48, ],
  sd_y = sd2a[48, ],
  obs_y = obs23a[48, ],
  title_text = "Weekend Load Profiles Cluster 2 HH 23 A",
  day_index = 48,
  model_color = "blue",
  sd_fill = "lightblue"
)

plot_single_panel(
  y = lp2b[48, ],
  sd_y = sd2b[48, ],
  obs_y = obs23b[48, ],
  title_text = "Weekend Load Profiles Cluster 2 HH 23 B",
  day_index = 48,
  model_color = "red",
  sd_fill = "lightpink"
)

plot_single_panel(
  y = lp3a[55, ],
  sd_y = sd3a[55, ],
  obs_y = obs40a[55, ],
  title_text = "Weekend Load Profiles Cluster 1 HH 40 A",
  day_index = 55,
  model_color = "blue",
  sd_fill = "lightblue"
)

plot_single_panel(
  y = lp3b[55, ],
  sd_y = sd3b[55, ],
  obs_y = obs40b[55, ],
  title_text = "Weekend Load Profiles Cluster 1 HH 40 B",
  day_index = 55,
  model_color = "red",
  sd_fill = "lightpink"
)

plot_single_panel(
  y = lp1a[35, ],
  sd_y = sd1a[35, ],
  obs_y = obs3a[35, ],
  title_text = "Weekday Load Profiles Cluster 2 HH 3 A",
  day_index = 35,
  model_color = "blue",
  sd_fill = "lightblue"
)

plot_single_panel(
  y = lp1b[35, ],
  sd_y = sd1b[35, ],
  obs_y = obs3b[35, ],
  title_text = "Weekday Load Profiles Cluster 1 HH 3 B",
  day_index = 35,
  model_color = "red",
  sd_fill = "lightpink"
)

plot_single_panel(
  y = lp2a[49, ],
  sd_y = sd2a[49, ],
  obs_y = obs23a[49, ],
  title_text = "Weekday Load Profiles Cluster 2 HH 23 A",
  day_index = 49,
  model_color = "blue",
  sd_fill = "lightblue"
)

plot_single_panel(
  y = lp2b[49, ],
  sd_y = sd2b[49, ],
  obs_y = obs23b[49, ],
  title_text = "Weekday Load Profiles Cluster 1 HH 23 B",
  day_index = 49,
  model_color = "red",
  sd_fill = "lightpink"
)

plot_single_panel(
  y = lp3a[56, ],
  sd_y = sd3a[56, ],
  obs_y = obs40a[56, ],
  title_text = "Weekday Load Profiles Cluster 2 HH 40 A",
  day_index = 56,
  model_color = "blue",
  sd_fill = "lightblue"
)

plot_single_panel(
  y = lp3b[56, ],
  sd_y = sd3b[56, ],
  obs_y = obs40b[56, ],
  title_text = "Weekday Load Profiles Cluster 1 HH 40 B",
  day_index = 56,
  model_color = "red",
  sd_fill = "lightpink"
)


################################################################################
################################# problem no. 7 ################################


# preprocessing the data again because the filters don't allow for NAs in the data

naclean <- function(hno){
  path = paste("E:/case_study/project_1/Haushalte mit Waermepumpe OHNE PV/OHNE PV/SFH", hno, ".csv", sep = "")
  M <- read.csv(path)
  M <- avrghours(M)
  vecv <- as.vector(t(M$Avhours))
  vecv[which(vecv == "NaN")] <- NA
  M <- matrix(vecv, nrow = 365, ncol = 24, byrow = TRUE)
  a <- apply(M,1, function(z) sum(is.na(z)))
  a <- which(a == 24)
  Mmean <- matrix(nrow = 7, ncol = 24)
  for(i in 1:7){
    s <- seq(i, 365,7)
    if(length(a) != 0){
      s <- s[-which(s %in% a)]
    }
    Mmean[i,] <- apply(M[s,],2, function(x) mean(x, na.rm = TRUE))
    for(j in 1:24){
      M[s,j][is.na(M[s,j])] <- Mmean[i,j]
    }
  }
  return(M)
}

M3 <- naclean(3)
M23 <- naclean(23)
M40 <- naclean(40)

set.seed(69)
sampledays <- sort(sample(1:365, 183))
sM3 <- M3[sampledays,]
sM23 <- M23[sampledays,]

nadays40 <- which(apply(M40, 1, function(x) sum(is.na(x)) == 24) == TRUE)
length(nadays40)
# 109
365 - 109
# 256
set.seed(69)
sampledays40 <- sort(sample((1:365)[-nadays40], 128))
sM40 <- M40[sampledays40,]

kalman <- function(para = c(0.5, 0.5, 0.5, 0.01, 0.01), a = 0, p = 1, Z, lp, sd, 
                   noload = FALSE, samp = 1:365){
  if(noload == TRUE){
    lp <- numeric(length = nrow(Z)*24)
    q <- numeric(length = nrow(Z)*24)
  }else{
    lp <- as.vector(lp[samp,]) # loadprofiles as vector for loop
    q <- as.vector(sd[samp,])^2 * para[4] # standard deviations as vector for loop
  }
  b <- numeric(length = nrow(Z)*24) # a vector for our state values
  z <- ski <- as.vector(t(as.matrix(Z)))
  c <- c(a, b)
  l <- p
  for(t in 2:(nrow(Z)*24+1)){ 
    a <- para[1]*c[t-1] + para[2]* lp[t-1]
    p <- para[1]^2 * l  +  q[t-1]
    b[t-1] <- para[3]*a
    S <- para[3]^2*p + para[5]
    k <- p*para[3] / S
    c[t] <- a + k *(z[t-1] - para[3]*a)
    l <- (1 - k*para[3])*p 
    ski[t-1] <- (z[t-1] - b[t-1])/sqrt(S)
  }
  return(list("prediction" = matrix(b, nrow = nrow(Z), ncol = 24, byrow = TRUE),
              "norm. res" = ski))
}

opti.f <- function(para, Z, lp, sd, samp, noload){
  sum((Z - kalman(Z = Z,lp = lp,sd = sd, para = para, noload = noload, 
                  samp = samp)$prediction)^2) 
}

griddy <- expand.grid(seq(0.1, 0.9, 0.3), seq(0.1, 0.9, 0.3), seq(0.1, 0.9, 0.3), 
                      seq(0.1, 0.9, 0.3), seq(0.1, 0.9, 0.3))


############################### with loadprofiles ##############################
################################# HOUSEHOLD NO. 03 #############################


optilopti3 <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM3, lp = lp1a, sd = sd1a, 
        samp = sampledays, noload = FALSE,
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
        upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)

which(optilopti3 == min(optilopti3))
# 12

optim(par = griddy[12,], fn = opti.f, Z = sM3, lp = lp1a, sd = sd1a, 
      samp = sampledays, lower = c(0.001, 0.001, 0.001, 0.001, 0.001), noload = FALSE,
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
#  0.5470328   0.4318589   0.6452441   0.1485472 321.9901298

## checking whether normally distributed 
kalman3 <- kalman(para = c(0.5470328, 0.4318589, 0.6452441, 0.1485472, 321.9901298), 
                  Z = M3[-sampledays,], lp = lp1a, sd = sd1a, 
                  samp = (1:365)[-sampledays])

Box.test(kalman3$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 2.224071e-09
# independence between values is rejected

shapiro.test(kalman3$`norm. res`)$p.value
# 4.966574e-78

optilopti3b <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM3, lp = lp1b, sd = sd1b, 
        samp = sampledays, noload = FALSE,
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
        upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)

which(optilopti3b == min(optilopti3b))
# 48

optim(par = griddy[48,], fn = opti.f, Z = sM3, lp = lp1b, sd = sd1b, 
      samp = sampledays, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
#  0.5453549   0.4027891   0.7291641   0.1538369 378.2764926
# 208269401

## checking whether normally distributed 
kalman3b <- kalman(para = c(0.5453549, 0.4027891, 0.7291641, 0.1538369, 378.2764926), 
                   Z = M3[-sampledays,], lp = lp1b, sd = sd1b, 
                   samp = (1:365)[-sampledays])

Box.test(kalman3b$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 8.140476e-07
# independence between values is rejected

shapiro.test(kalman3b$`norm. res`)$p.value
# 1.391758e-76


################################# HOUSEHOLD NO. 23 #############################


optilopti23 <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f,Z = sM23, lp = lp2a, sd = sd2a, 
        samp = sampledays, noload = FALSE, 
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
        upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)
which(optilopti23 == min(optilopti23))
# 73

optim(par = griddy[73,], fn = opti.f, Z = sM23, lp = lp2a, sd = sd2a, 
      samp = sampledays, lower = c(0.001, 0.001, 0.001, 0.001, 0.001), noload = FALSE,
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
#  0.55222861   0.49391960   0.83126954 468.24860952   0.05363419

## checking whether normally distributed 
kalman23 <- kalman(para = c(0.55222861, 0.49391960, 0.83126954, 468.24860952, 0.05363419), 
                   Z = M23[-sampledays,], lp = lp2a, sd = sd2a, 
                   samp = (1:365)[-sampledays])

Box.test(kalman23$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0.9608591 
# cannot reject assumption that they are independent

shapiro.test(kalman23$`norm. res`)$p.value
# 1.231266e-91

optilopti23b <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f,Z = sM23, lp = lp2b, sd = sd2b, 
        samp = sampledays, noload = FALSE, 
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
        upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)
which(optilopti23b == min(optilopti23b))
# 46

optim(par = griddy[46,], fn = opti.f, Z = sM23, lp = lp2b, sd = sd2b, 
      samp = sampledays, noload = FALSE, lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.53882409   0.52682234   0.84638147 480.97121160   0.05235726 

## checking whether normally distributed 
kalman23b <- kalman(para = c(0.53882409, 0.52682234, 0.84638147, 480.97121160, 0.05235726), 
                    Z = M23[-sampledays,], lp = lp2b, sd = sd2b, 
                    samp = (1:365)[-sampledays])

Box.test(kalman23b$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0.9706347
# cannot reject assumption that they are independent

shapiro.test(kalman23b$`norm. res`)$p.value
# 1.228752e-91


################################################################################
################################# HOUSEHOLD NO. 40 #############################


optilopti40 <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM40, lp = lp3a, sd = sd3a, noload = FALSE,
        samp = sampledays40, lower = c(0.001, 0.001, 0.001, 0.001, 0.001),
        upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000),
        method = "L-BFGS-B")$value)

which(optilopti40 == min(optilopti40))
# 72

optim(par = griddy[72,], fn = opti.f, Z = sM40, lp = lp3a, sd = sd3a, 
      samp = sampledays40, lower = c(0.001, 0.001, 0.001, 0.001, 0.001), noload = FALSE, 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
#  0.7561663   0.8438635   0.4012164   0.3466109 336.8471248

## checking whether normally distributed 
kalman40 <- kalman(para = c(0.7561663, 0.8438635, 0.4012164, 0.3466109, 336.8471248), 
                   Z = M40[-c(sampledays40, nadays40),], lp = lp3a, sd = sd3a, 
                   samp = (1:365)[-c(sampledays40, nadays40)])

Box.test(kalman40$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0.003759277
# independence is rejected

shapiro.test(kalman40$`norm. res`)$p.value
# 2.62306e-54

optilopti40b <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM40, lp = lp3b, sd = sd3b,
        samp = sampledays40, noload = FALSE, 
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001),
        upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000),
        method = "L-BFGS-B")$value)

which(optilopti40b == min(optilopti40b))
# 9

optim(par = griddy[9,], fn = opti.f, Z = sM40, lp = lp3b, sd = sd3b, 
      samp = sampledays40, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.6697760   0.6638399   0.5849621   0.6494355 483.9846355

## checking whether normally distributed 
kalman40b <- kalman(para = c(0.6697760, 0.6638399, 0.5849621, 0.6494355, 483.9846355), 
                    Z = M40[-c(sampledays40, nadays40),], lp = lp3b, sd = sd3b, 
                    samp = (1:365)[-c(sampledays40, nadays40)])

Box.test(kalman40b$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0.01704555
# independence is rejected

shapiro.test(kalman40b$`norm. res`)$p.value
# 1.279156e-55


################################# without loadprofiles #########################
################################# HOUSEHOLD NO. 03 #############################


noptilopti3 <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM3, 
        noload = TRUE, samp = sampledays, 
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), upper = c(1, 1, 1, 999, 999),
        control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)

which(noptilopti3 == min(noptilopti3))
# they are all the same because the predictions are sooo unbelievably small that
# the residuals just end up being extremely close to the original data.
# the starting values a and p could be changed to create different predictions

optim(par = griddy[1,], fn = opti.f, Z = sM3,
      samp = sampledays, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 1.000 0.100 1.000 0.100 0.001
# 244433985

## checking whether normally distributed 
kalman3no <- kalman(para = c(1.000, 0.100, 1.000, 0.100, 0.001), 
                  Z = M3[-sampledays,], noload = TRUE, 
                  samp = (1:365)[-sampledays])

Box.test(kalman3no$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0
# independence between values is rejected

shapiro.test(kalman3no$`norm. res`)$p.value
# 3.398862e-77

################################# HOUSEHOLD NO. 23 #############################
noptilopti23 <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM23,
        noload = TRUE, samp = sampledays, 
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), upper = c(1, 1, 1, 999, 999),
        control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)

which(noptilopti23 == min(noptilopti23))
# all the same

optim(par = griddy[1,], fn = opti.f, Z = sM23,
      samp = sampledays, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 1.0000000 0.1000000 0.9998557 0.1000000 0.1452911 
# 243988603

## checking whether normally distributed 
kalman23no <- kalman(para = c(1, 0.1, 0.9998557, 0.1, 0.1452911), 
                    Z = M23[-sampledays,], noload = TRUE, 
                    samp = (1:365)[-sampledays])

Box.test(kalman23no$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0
# independence between values is rejected

shapiro.test(kalman23no$`norm. res`)$p.value
# 2.446332e-70

################################# HOUSEHOLD NO. 40 #############################

noptilopti40 <- apply(griddy, 1, function(x) 
  optim(par = as.numeric(x), fn = opti.f, Z = sM40,
        noload = TRUE, samp = sampledays40, 
        lower = c(0.001, 0.001, 0.001, 0.001, 0.001), upper = c(1, 1, 1, 999, 999),
        control = list(maxit = 10000), 
        method = "L-BFGS-B")$value)

which(noptilopti40 == min(noptilopti40))
# all the same

optim(par = griddy[1,], fn = opti.f, Z = sM40,
      samp = sampledays40, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 1, 0.1, 1, 0.1, 0.001 
# 467654789

## checking whether normally distributed 
kalman40no <- kalman(para = c(1, 0.1, 1, 0.1, 0.001), 
                     Z = M40[-c(sampledays40, nadays40),], noload = TRUE, 
                     samp = (1:365)[-c(sampledays40, nadays40)])

Box.test(kalman40no$`norm. res`, lag = 1, type = "Ljung-Box")$p.value
# 0
# independence between values is rejected

shapiro.test(kalman40no$`norm. res`)$p.value
# 3.668025e-54


##### Visualization Kalman Filter #####
set.seed(69)
sampledays <- sort(sample(1:365, 183))
print(sampledays)

notsampledays <- setdiff(1:365, sampledays)
print(notsampledays)

testdays <- setdiff(1:365, sampledays)

28 %in% testdays
# If TRUE, you're good to proceed


plot_kalman_diagnostics <- function(
    para, kalman_result, M_matrix, testdays,
    target_day = 28, main_title = NULL
) {
  day_index <- which(testdays == target_day)
  
  if (length(day_index) == 0) {
    stop(paste("Day", target_day, "is not in testdays."))
  }
  
  actual_day <- M_matrix[target_day, ]
  predicted_day <- kalman_result$prediction[day_index, ]
  
  df_day <- data.frame(
    Hour = 0:23,
    Actual = actual_day,
    Predicted = predicted_day
  )
  
  if (!is.null(main_title)) {
    title_p1 <- main_title
  } else {
    title_p1 <- paste0("Day ", target_day, ": Actual vs. Predicted Load")
  }
  
  bold_theme <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold")
    )
  
  # Actual vs Predicted
  p1 <- ggplot(df_day, aes(x = Hour)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2, linetype = "dashed") +
    labs(title = title_p1,
         x = "Hour of the Day", y = "Power Consumption (W)", color = "") +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    bold_theme
  
  # Q-Q Plot
  p3 <- ggplot(data.frame(resid = kalman_result$`norm. res`), aes(sample = resid)) +
    stat_qq(color = "darkred") +
    stat_qq_line(color = "black") +
    labs(title = "Q-Q Plot of Standardized Residuals",
         x = "Theoretical Quantiles", y = "Sample Quantiles") +
    bold_theme
  
  # Combine Actual vs. Predicted + Q-Q
  combined_plot <- p1 / p3
  return(combined_plot)
}

# Household 3
p1 <- plot_kalman_diagnostics(para = c(0.5470328, 0.4318589, 0.6452441, 0.1485472, 321.9901298),
                              kalman_result = kalman3, M_matrix = M3, testdays = testdays,
                              target_day = 28, main_title = "Load A – Day 28, HH 3")
ggsave("E:/case_study/project_1/hh3_loadA.png", plot = p1, width = 10, height = 6, dpi = 100)

p2 <- plot_kalman_diagnostics(para = c(0.5453549, 0.4027891, 0.7291641, 0.1538369, 378.2764926),
                              kalman_result = kalman3b, M_matrix = M3, testdays = testdays,
                              target_day = 28, main_title = "Load B – Day 28, HH 3")
ggsave("E:/case_study/project_1/hh3_loadB.png", plot = p2, width = 10, height = 6, dpi = 100)

# Household 23
p3 <- plot_kalman_diagnostics(para = c(0.55222861, 0.49391960, 0.83126954, 468.24860952, 0.05363419),
                              kalman_result = kalman23, M_matrix = M23, testdays = testdays,
                              target_day = 28, main_title = "Load A – Day 28, HH 23")
ggsave("E:/case_study/project_1/hh23_loadA.png", plot = p3, width = 10, height = 6, dpi = 100)

p4 <- plot_kalman_diagnostics(para = c(0.53882409, 0.52682234, 0.84638147, 480.97121160, 0.05235726),
                              kalman_result = kalman23b, M_matrix = M23, testdays = testdays,
                              target_day = 28, main_title = "Load B – Day 28, HH 23")
ggsave("E:/case_study/project_1/hh23_loadB.png", plot = p4, width = 10, height = 6, dpi = 100)

# Household 40
p5 <- plot_kalman_diagnostics(para = c(0.7561663, 0.8438635, 0.4012164, 0.3466109, 336.8471248),
                              kalman_result = kalman40, M_matrix = M40, testdays = testdays,
                              target_day = 28, main_title = "Load A – Day 28, HH 40")
ggsave("E:/case_study/project_1/hh40_loadA.png", plot = p5, width = 10, height = 6, dpi = 100)

p6 <- plot_kalman_diagnostics(para = c(0.6697760, 0.6638399, 0.5849621, 0.6494355, 483.9846355),
                              kalman_result = kalman40b, M_matrix = M40, testdays = testdays,
                              target_day = 28, main_title = "Load B – Day 28, HH 40")
ggsave("E:/case_study/project_1/hh40_loadB.png", plot = p6, width = 10, height = 6, dpi = 100)


p7 <- plot_kalman_diagnostics(
      para = c(1.000, 0.100, 1.000, 0.100, 0.001),
      kalman_result = kalman3no,
      M_matrix = M3,
      testdays = testdays,
      target_day = 28,
      main_title = "No Load – Day 28, Household 3"
)

p8 <- plot_kalman_diagnostics(
      para = c(1, 0.1, 0.9998557, 0.1, 0.1452911),
      kalman_result = kalman23no,
      M_matrix = M23,
      testdays = testdays,
      target_day = 28,
      main_title = "No Load – Day 28, Household 23"
)

p9 <- plot_kalman_diagnostics(
      para = c(1, 0.1, 1, 0.1, 0.001),
      kalman_result = kalman40no,
      M_matrix = M40,
      testdays = testdays,
      target_day = 28,
      main_title = "No Load – Day 28, Household 40"
)

# Combine 
combined4 <- (p7 | p8 | p9)

# Display all plots together
print(combined4)
ggsave("E:/case_study/project_1/line_hh_no_load.png", plot = combined4, width = 10, height = 6, dpi = 100)


################################################################################
############################ Particle filter ###################################


partikel <- function(para = c(0.5, 0.5, 0.5, 100, 100), c = 0, l = 1, Z, M, lp, 
                     sd, noload = FALSE, samp){
  if(noload == TRUE){
    lp <- numeric(length = nrow(Z)*24)
    q <- numeric(length = nrow(Z)*24)
  }else{
    lp <- as.vector(lp[samp,])
    q <- as.vector(sd[samp,])^2 * para[4]
  }
  z <- as.vector(t(as.matrix(Z)))
  zquer <- numeric(length(samp)*24)
  xquer <- numeric(length(samp)*24+1)
  sigma <- numeric(length(samp)*24)
  xquer[1] <- mean(rnorm(M, mean = c, sd = l))
  for(t in 2:(length(samp)*24+1)){
    e <- rnorm(M, mean = 0, sd = sqrt(q[t-1]))
    xssss <- para[1]*xquer[t-1] + para[2]*lp[t-1] + e
    wssss <- dnorm(z[t-1], mean = para[3]*xssss, sd = para[5])
    w <- wssss / sum(wssss)
    x <- sample(xssss, size = M, replace = TRUE, prob = w)
    xquer[t] <- mean(x)
    zquer[t-1] <- para[3]*xquer[t]
    sigma[t-1] <- sum((para[3]*x - zquer[t-1])^2)/(M-1)
  }
  ski <- (z - zquer) / sigma
  return(list("predictions" = matrix(zquer, nrow = length(samp), byrow = TRUE),
              "norm. res" = ski))
}

opti.f2 <- function(para, Z, lp, sd, samp, M, noload){
  sum((Z - partikel(Z = Z,lp = lp,sd = sd, para = para, samp = samp, 
                    noload = noload, M = M)$predictions)^2) 
}


############################# with loadprofiles ################################
############################# HOUSEHOLD NO. 03 #################################


set.seed(123)
optim(par = c(0.1, 0.1, 0.1, 100, 100), fn = opti.f2, Z = sM3, lp = lp1a, sd = sd1a, 
      samp = sampledays, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.2829549   0.0010000   0.5781022 141.5006715 142.3552191
# 2430868
set.seed(123)
optim(par = c(0.11, 0.11, 0.1, 100, 100), fn = opti.f2, Z = sM3, lp = lp1a, sd = sd1a, 
      samp = sampledays, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.00107278   0.74272906   0.91641671  99.60063096 100.72242397
# 1675440
set.seed(123)
optim(par = c(0.8, 0.8, 0.1, 100, 100), fn = opti.f2, Z = sM3, lp = lp1a, sd = sd1a, 
      samp = sampledays, noload = FALSE, M = 1000, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.80038308   0.80038308   0.09981038 101.72194384 101.72194384
# 43403139

# higher variances lead to extremely high values
set.seed(123)
optim(par = c(0.81, 0.81, 0.1, 100, 100), fn = opti.f2, Z = sM3, lp = lp1a, sd = sd1a, 
      samp = sampledays, noload = FALSE, M = 1000, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 99999, 99999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.8100   0.8100   0.1000 100.0000 100.0001
# 43054493

set.seed(123)
partikel3 <- partikel(para = c(0.00107278, 0.74272906, 0.91641671, 99.60063096, 100.72242397),
                      Z = M3[-sampledays,], lp = lp1a, sd = sd1a, 
                      samp = (1:365)[-sampledays], M = 1000)
Box.test(partikel3$`norm. res`[-which(partikel3$`norm. res` == Inf | 
                                        partikel3$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# 0.9888 >> independence is not rejected

shapiro.test(partikel3$`norm. res`[-which(partikel3$`norm. res` == Inf | 
                                            partikel3$`norm. res` == -Inf)])$p.value
# 4.429112e-92

set.seed(123)
optim(par = c(0.11, 0.11, 0.1, 300, 150), fn = opti.f2, Z = sM3, lp = lp1b, sd = sd1b, 
      samp = sampledays, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.1103418   0.1099581   0.1003457 300.2684721 150.3260842
# 60541208

set.seed(123)
optim(par = c(0.8, 0.8, 0.3, 300, 150), fn = opti.f2, Z = sM3, lp = lp1b, sd = sd1b, 
      samp = sampledays, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
#  0.8   0.8   0.3 300.0 150.0 (lol?)
# 1324907

set.seed(123)
optim(par = c(0.6, 0.5, 0.6, 300, 300), fn = opti.f2, Z = sM3, lp = lp1b, sd = sd1b, 
      samp = sampledays, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.5994584   0.4995488   0.6003616 299.7287673 300.6319743
# 2204477

set.seed(123)
partikel3b <- partikel(para = c(0.8, 0.8, 0.3, 300, 150),
                      Z = M3[-sampledays,], lp = lp1b, sd = sd1b, 
                      samp = (1:365)[-sampledays], M = 1000)
Box.test(partikel3b$`norm. res`[-which(partikel3b$`norm. res` == Inf | partikel3b$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# 2.2e-16 >> independence is rejected

shapiro.test(partikel3b$`norm. res`[-which(partikel3b$`norm. res` == Inf | 
                                            partikel3b$`norm. res` == -Inf)])$p.value
# 2.003088e-90


################################################################################
############################### HOUSEHOLD NO. 23 ###############################

set.seed(123)
optim(par = c(0.2, 0.2, 0.2, 100, 100), fn = opti.f2, Z = sM23, lp = lp2a, sd = sd2a, 
      samp = sampledays, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
#  0.1999152   0.2003411   0.2003411  99.9573624 100.3833161
# 26724847
set.seed(123)
optim(par = c(0.5, 0.5, 0.5, 100, 100), fn = opti.f2, Z = sM23, lp = lp2a, sd = sd2a, 
      samp = sampledays, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.4269239   0.7208863   0.5969691 104.8232465 104.6423308
# 852708.7

set.seed(123)
partikel23 <- partikel(para = c(0.4269239, 0.7208863, 0.5969691, 104.8232465, 104.6423308),
                       Z = M23[-sampledays,], lp = lp2a, sd = sd2a, 
                       samp = (1:365)[-sampledays], M = 1000)
Box.test(partikel23$`norm. res`[-which(partikel23$`norm. res` == Inf | partikel23$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# 0.01824 >> independence is rejected

shapiro.test(partikel23$`norm. res`[-which(partikel23$`norm. res` == Inf | 
                                             partikel23$`norm. res` == -Inf)])$p.value
# 9.162591e-80

set.seed(123)
optim(par = c(0.1, 0.1, 0.1, 150, 150), fn = opti.f2, Z = sM23, lp = lp2b, sd = sd2b, 
      samp = sampledays, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.1798801   0.5311555   0.9984210 300.6512741 299.2594661
# 2324267

set.seed(123)
optim(par = c(0.5, 0.5, 0.5, 150, 150), fn = opti.f2, Z = sM23, lp = lp2b, sd = sd2b, 
      samp = sampledays, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.5   0.5   0.5 150.0 150.0
# 1350202

set.seed(123)
optim(par = c(0.9, 0.2, 0.5, 150, 150), fn = opti.f2, Z = sM23, lp = lp2b, sd = sd2b, 
      samp = sampledays, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.8970815   0.2001118   0.4993155 149.9633818 150.2017111
# 1801138

set.seed(123)
partikel23b <- partikel(para = c(0.5, 0.5, 0.5, 150.0, 150.0),
                       Z = M23[-sampledays,], lp = lp2b, sd = sd2b, 
                       noload = FALSE, samp = (1:365)[-sampledays], M = 1000)
Box.test(partikel23b$`norm. res`[-which(partikel23b$`norm. res` == Inf | partikel23b$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# 0.8546 >> independence is not rejected

shapiro.test(partikel23b$`norm. res`[-which(partikel23b$`norm. res` == Inf | 
                                             partikel23b$`norm. res` == -Inf)])$p.value
# 1.647282e-90


################################# HOUSEHOLD NO. 40 #############################
################################################################################


set.seed(123)
optim(par = c(0.2, 0.2, 0.2, 100, 100), fn = opti.f2, Z = sM40, lp = lp3a, sd = sd3a, 
      samp = sampledays40, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.2000000   0.2000001   0.2000001  99.9999931 100.0000623
# 182755021
set.seed(123)
optim(par = c(0.1, 0.2, 0.1, 100, 100), fn = opti.f2, Z = sM40, lp = lp3a, sd = sd3a, 
      samp = sampledays40, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.6883526   0.4345658   0.9861147 139.8168371 140.6922167
# 14383810
set.seed(123)
optim(par = c(0.6, 0.6, 0.1, 100, 100), fn = opti.f2, Z = sM40, lp = lp3a, sd = sd3a, 
      samp = sampledays40, M = 1000, noload = FALSE,
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.8140815   0.0010000   1.0000000 153.0644897 151.5554406
# 14140278
set.seed(123)
optim(par = c(0.5, 0.6, 0.1, 100, 100), fn = opti.f2, Z = sM40, lp = lp3a, sd = sd3a, 
      samp = sampledays40, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 1.0000000   0.7989293   0.7989293 207.6358893 201.2911058
# 19841003

set.seed(123)
partikel40 <- partikel(para = c(0.8140815, 0.0010000, 1.0000000, 153.0644897, 151.5554406),
                       Z = M40[-c(sampledays40, nadays40),], lp = lp3a, sd = sd3a, 
                       samp = (1:365)[-c(sampledays40, nadays40)], M = 1000)
Box.test(partikel40$`norm. res`[-which(partikel40$`norm. res` == Inf | partikel40$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# 0.9724 >> independence is not rejected

shapiro.test(partikel40$`norm. res`[-which(partikel40$`norm. res` == Inf | 
                                              partikel40$`norm. res` == -Inf)])$p.value
# 1.05822e-82

set.seed(123)
optim(par = c(0.1, 0.2, 0.1, 350, 350), fn = opti.f2, Z = sM40, lp = lp3b, sd = sd3b, 
      samp = sampledays40, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 1.0000000   0.9999995   0.8384425 424.7041360 425.6052806
# 15110724

set.seed(123)
optim(par = c(0.8, 0.8, 0.1, 350, 350), fn = opti.f2, Z = sM40, lp = lp3b, sd = sd3b, 
      samp = sampledays40, M = 1000, noload = FALSE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 1.0000   1.0000   1.0000 354.3251 355.2208
# 14812873

set.seed(123)
partikel40b <- partikel(para = c(1.0000, 1.0000, 1.0000, 354.3251, 355.2208),
                       Z = M40[-c(sampledays40, nadays40),], lp = lp3b, sd = sd3b, 
                       samp = (1:365)[-c(sampledays40, nadays40)], M = 1000)
Box.test(partikel40b$`norm. res`[-which(partikel40b$`norm. res` == Inf | partikel40b$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# 0.9242 independence is not rejected

shapiro.test(partikel40b$`norm. res`[-which(partikel40b$`norm. res` == Inf | 
                                             partikel40b$`norm. res` == -Inf)])$p.value
# 3.374293e-36


######################### without loadprofiles #################################
######################### HOUSEHOLD NO. 03 #####################################


set.seed(123)
optim(par = c(0.11, 0.11, 0.1, 100, 100), fn = opti.f2, Z = sM3, 
      samp = sampledays, M = 1000, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.10945473   0.11445224   0.09950475 100.63496552  99.95544238
# 481686092
set.seed(123)
optim(par = c(0.1, 0.3, 0.7, 150, 150), fn = opti.f2, Z = sM3,
      samp = sampledays, M = 1000, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.09999548   0.30003194   0.69996810 150.03635789 149.99744863
# 481686091

set.seed(123)
optim(par = c(0.1, 0.5, 0.9, 250, 250), fn = opti.f2, Z = sM3, 
      samp = sampledays, M = 1000, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.09999535   0.50002349   0.89995777 250.03518505 249.99662288
# 481686091

set.seed(123)
optim(par = c(0.2, 0.5, 0.3, 350, 250), fn = opti.f2, Z = sM3, 
      samp = sampledays, M = 1000, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.1999908   0.5000231   0.2999862 350.0300077 249.9974534
# 481686091

# it doesn't seem to get any better

set.seed(123)
partikel3no <- partikel(para = c(0.1999908, 0.5000231, 0.2999862, 350.0300077, 249.9974534),
                      Z = M3[-sampledays,], noload = TRUE, 
                      samp = (1:365)[-sampledays], M = 1000)
Box.test(partikel3no$`norm. res`[-which(partikel3no$`norm. res` == Inf | 
                                          partikel3no$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# this does not work because all normalized observations are Inf, leaving no 
# possible values for Box.test

shapiro.test(partikel3no$`norm. res`[-which(partikel3no$`norm. res` == Inf | 
                                              partikel3no$`norm. res` == -Inf)])$p.value
# does not work

############################# HOUSEHOLD NO. 23 #################################
set.seed(123)
optim(par = c(0.11, 0.11, 0.1, 100, 100), fn = opti.f2, Z = sM23, 
      samp = sampledays, M = 1000, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.10999521   0.11003915   0.09999565 100.01245583  99.99912594
# 807661737

set.seed(123)
partikel23no <- partikel(para = c(0.10999521, 0.11003915, 0.09999565, 100.01245583, 99.99912594),
                        Z = M23[-sampledays,], noload = TRUE, 
                        samp = (1:365)[-sampledays], M = 1000)
Box.test(partikel23no$`norm. res`[-which(partikel23no$`norm. res` == Inf | 
                                           partikel23no$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# this does not work because all normalized observations are Inf, leaving no 
# possible values for Box.test

shapiro.test(partikel23no$`norm. res`[-which(partikel23no$`norm. res` == Inf | 
                                              partikel23no$`norm. res` == -Inf)])$p.value

# 0


################################################################################
############################### HOUSEHOLD NO. 40 ###############################


set.seed(123)
optim(par = c(0.1, 0.5, 0.1, 200, 200), fn = opti.f2, Z = sM40, 
      samp = sampledays40, M = 1000, noload = TRUE, 
      lower = c(0.001, 0.001, 0.001, 0.001, 0.001), 
      upper = c(1, 1, 1, 999, 999), control = list(maxit = 10000), 
      method = "L-BFGS-B")
# 0.1000016   0.4999991   0.1000016 199.9998264 199.9998907
# 1135815529

set.seed(123)
partikel40no <- partikel(para = c(0.1000016, 0.4999991, 0.1000016, 199.9998264, 199.9998907),
                         Z = M40[-c(sampledays40, nadays40),], noload = TRUE, 
                         samp = (1:365)[-c(sampledays40, nadays40)], M = 1000)
Box.test(partikel40no$`norm. res`[-which(partikel40no$`norm. res` == Inf | partikel40no$`norm. res` == -Inf)], 
         lag = 1, 
         type = "Ljung-Box")
# this does not work because all normalized observations are Inf, leaving no 
# possible values for Box.test

shapiro.test(partikel40no$`norm. res`[-which(partikel40no$`norm. res` == Inf | 
                                              partikel40no$`norm. res` == -Inf)])$p.value

# 0


##### Visualization Partikel Filter #####
plot_particle_diagnostics <- function(para, particle_result, M_matrix, testdays, 
                                      target_day = 28, main_title = NULL) {
  # Find position of target day in testdays
  day_index <- which(testdays == target_day)
  
  if (length(day_index) == 0) {
    stop(paste("Day", target_day, "is not in testdays."))
  }
  
  # TRUE (observed) values from original M_matrix
  actual_day <- M_matrix[target_day, ]
  
  # PREDICTED values from Particle filter
  predicted_day <- particle_result$predictions[day_index, ]
  
  # Combine into a dataframe for plotting
  df_day <- data.frame(
    Hour = 0:23,
    Actual = actual_day,
    Predicted = predicted_day
  )
  
  # Determine Load or No Load
  load_status <- if (all(actual_day < 5)) "No Load" else "Load"
  
  # Title for plot
  if (!is.null(main_title)) {
    title_p1 <- main_title
  } else {
    title_p1 <- paste0("Day ", target_day, ": Actual vs. Predicted Load")
    if (!is.null(house_number)) {
      title_p1 <- paste0(title_p1, " – Household ", house_number)
    }
    title_p1 <- paste0(title_p1, " (", load_status, ")")
  }
  
  # Bold theme
  bold_theme <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      legend.text = element_text(face = "bold")
    )
  
  # Plot 1: Actual vs Predicted
  p1 <- ggplot(df_day, aes(x = Hour)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1.2, 
              linetype = "dashed") +
    labs(
      title = title_p1,
      x = "Hour of the Day",
      y = "Power Consumption (W)",
      color = ""
    ) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    bold_theme
  
  # Plot 2: Histogram of residuals
  p2 <- ggplot(data.frame(resid = particle_result$`norm. res`), aes(x = resid)) +
    geom_histogram(bins = 40, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(
      title = "Histogram of Standardized Residuals",
      x = "Standardized Residual",
      y = "Frequency"
    ) +
    bold_theme
  
  # Plot 3: Q-Q plot
  p3 <- ggplot(data.frame(resid = particle_result$`norm. res`), 
               aes(sample = resid)) +
    stat_qq(color = "darkred") +
    stat_qq_line(color = "black") +
    labs(
      title = "Q-Q Plot of Standardized Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    bold_theme
  
  # Combine plots (Actual vs Predicted + Q-Q)
  combined_plot <- p1 # / p3
  print(combined_plot)
}


plot_particle_diagnostics(
  para = c(0.00107278, 0.74272906, 0.91641671, 99.60063096, 100.72242397),
  particle_result = partikel3,
  M_matrix = M3,
  testdays = testdays,
  target_day = 28,
  main_title = "Particle Filter Actual vs. Predicted Load A – Day 28, Household 3"
)

plot_particle_diagnostics(
  para = c(0.4269239, 0.7208863, 0.5969691, 104.8232465, 104.6423308),
  particle_result = partikel23,
  M_matrix = M23,
  testdays = testdays,
  target_day = 28,
  main_title = "Particle Filter Actual vs. Predicted Load A – Day 28, Household 23"
)

plot_particle_diagnostics(
  para = c(0.8140815, 0.0010000, 1.0000000, 153.0644897, 151.5554406),
  particle_result = partikel40,
  M_matrix = M40,
  testdays = testdays,
  target_day = 28,
  main_title = "Particle Filter Actual vs. Predicted Load A – Day 28, Household 40"
)

plot_particle_diagnostics(
  para = c(0.8, 0.8, 0.3, 300, 150),
  particle_result = partikel3b,
  M_matrix = M3,
  testdays = testdays,
  target_day = 28,
  main_title = "Particle Filter Actual vs. Predicted Load B – Day 28, Household 3"
)

plot_particle_diagnostics(
  para = c(0.5, 0.5, 0.5, 150.0, 150.0),
  particle_result = partikel23b,
  M_matrix = M23,
  testdays = testdays,
  target_day = 28,
  main_title = "Particle Filter Actual vs. Predicted Load B – Day 28, Household 23"
)

plot_particle_diagnostics(
  para = c(1.0000, 1.0000, 1.0000, 354.3251, 355.2208),
  particle_result = partikel40b,
  M_matrix = M40,
  testdays = testdays,
  target_day = 28,
  main_title = "Particle Filter Actual vs. Predicted Load B – Day 28, Household 40"
)


plot_particle_diagnostics(
  para = c(0.1999908, 0.5000231, 0.2999862, 350.0300077, 249.9974534),
  particle_result = partikel3no,
  M_matrix = M3,
  testdays = testdays,
  target_day = 28,
  main_title = "Actual vs. Predicted No Load – Day 28, Household 3"
)


plot_particle_diagnostics(
  para = c(0.10999521, 0.11003915, 0.09999565, 100.01245583, 99.99912594),
  particle_result = partikel23no,
  M_matrix = M23,
  testdays = testdays,
  target_day = 28,
  main_title = "Actual vs. Predicted No Load – Day 28, Household 23"
)


plot_particle_diagnostics(
  para = c(0.1000016, 0.4999991, 0.1000016, 199.9998264, 199.9998907),
  particle_result = partikel40no,
  M_matrix = M40,
  testdays = testdays,
  target_day = 28,
  main_title = "Actual vs. Predicted No Load – Day 28, Household 40"
)

particle_3 <- plot_particle_diagnostics(
  para = c(0.1999908, 0.5000231, 0.2999862, 350.0300077, 249.9974534),
  particle_result = partikel3no,
  M_matrix = M3,
  testdays = testdays,
  target_day = 28,
  main_title = "No Load – Day 28, Household 3"
)

particle_23 <- plot_particle_diagnostics(
  para = c(0.10999521, 0.11003915, 0.09999565, 100.01245583, 99.99912594),
  particle_result = partikel23no,
  M_matrix = M23,
  testdays = testdays,
  target_day = 28,
  main_title = "No Load – Day 28, Household 23"
)

particle_40 <- plot_particle_diagnostics(
  para = c(0.1000016, 0.4999991, 0.1000016, 199.9998264, 199.9998907),
  particle_result = partikel40no,
  M_matrix = M40,
  testdays = testdays,
  target_day = 28,
  main_title = "No Load – Day 28, Household 40"
)

# Combine 
combined4 <- (particle_3 | particle_23 | particle_40)

# Display all plots together
print(combined4)
ggsave("E:/case_study/project_1/particle_no_load.png", plot = combined4, 
       width = 10, height = 6, dpi = 100)


################################################################################
################################# problem no. 8 ################################


########### difference between the different smoothing methods with loadprofiles
# (divided the MSE by the number of time units to make it easier for us to 
# compare the values)

################################## KALMAN FILTER ###############################
# household no. 3
sum((M3[-sampledays,] - kalman3$prediction)^2) / (182*24) 
sum((M3[-sampledays,] - kalman3b$prediction)^2) / (182*24)
# 53578.61 versus 53153.61

# household no. 23
sum((M23[-sampledays,] - kalman23$prediction)^2) / (182*24)
sum((M23[-sampledays,] - kalman23b$prediction)^2) / (182*24)
# 53237.97 versus 52793.2

# household no. 40
sum((M40[-c(sampledays40, nadays40),] - kalman40$prediction)^2) / (128*24)
sum((M40[-c(sampledays40, nadays40),] - kalman40b$prediction)^2) / (128*24)
# 110721.6 versus 109495.2

################################################################ PARTICLE FILTER
sum((M3[-sampledays,] - partikel3$prediction)^2) / (182*24) 
sum((M3[-sampledays,] - partikel3b$prediction)^2) / (182*24)
# 1193.905 versus 1139.615 (better than Kalman filter)

sum((M23[-sampledays,] - partikel23$prediction)^2) / (182*24) 
sum((M23[-sampledays,] - partikel23b$prediction)^2) / (182*24)
# 124.5958 versus 245.6398 (pretty good)

sum((M40[-c(sampledays40, nadays40),] - partikel40$prediction)^2) / (128*24)
sum((M40[-c(sampledays40, nadays40),] - partikel40b$prediction)^2) / (128*24)
# 1237.897 versus 2676.613

################## difference between with loadprofiles and without loadprofiles
# household no. 3
sum((M3[-sampledays,] - kalman3no$prediction)^2) / (182*24)
# 65026.09 versus 53578.61 / 53153.61
sum((M3[-sampledays,] - partikel3no$prediction)^2) / (182*24)
# 119103.5 versus 1193.905 / 1139.615

# household no. 23
sum((M23[-sampledays,] - kalman23no$prediction)^2) / (182*24)
# 56172.84 versus 53237.97 / 52793.2
sum((M23[-sampledays,] - partikel23no$prediction)^2) / (182*24)
# 181451.8 versus 124.5958 / 245.6398

# household no. 40
sum((M40[-c(sampledays40, nadays40),] - kalman40no$prediction)^2) / (128*24)
# 151963 versus 110721.6 / 109495.2
sum((M40[-c(sampledays40, nadays40),] - partikel40no$prediction)^2) / (128*24)
# 367335.6 versus 1237.897 / 2676.613

# We can improve the predictions with no loadprofiles by including our filter
# parameters a and p (kalman) or c and l (particle) in the optimization block
# the forecasts highly depend on these starter values when there are no 
# loadprofiles


##### Boxplots of hourly load by Household #####
#Identify hourly columns
hourly_cols <- grep("^[0-9]{1,2}h[a|b]$", names(M), value = TRUE)

M_long <- M %>%
  dplyr::select(Household, all_of(hourly_cols)) %>%
  pivot_longer(
    cols = -Household,
    names_to = "Hour",
    values_to = "Value"
  )

#Create the boxplot per household
ggplot(M_long, aes(x = Hour, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
  facet_wrap(~ Household, scales = "free_y") +  # One panel per household
  labs(
    title = "Boxplots of Hourly Load by Household",
    x = "Hour",
    y = "Load Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

summary_stats <- M_long %>%
  group_by(Household) %>%
  summarise(
    Median = median(Value, na.rm = TRUE),
    NA_Values = sum(is.na(Value)),
    Outliers = sum(
      Value < quantile(Value, 0.25, na.rm = TRUE) - 1.5 * IQR(Value, na.rm = TRUE) |
        Value > quantile(Value, 0.75, na.rm = TRUE) + 1.5 * IQR(Value, na.rm = TRUE),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(summary_stats)
glimpse(summary_stats)
print(summary_stats, n = Inf)


################## Scatter plot for outliers of each household #################
################################################################################


# Data
households <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 17, 18, 19, 20, 21, 22,
                23, 25, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 40)
outliers <- c(1762, 872, 1148, 225, 545, 445, 684, 337, 721, 1119, 862, 1098, 
              342, 816, 1075, 993, 939, 670, 909, 437, 1091, 317, 1214, 699, 1848, 932, 2100, 789, 448, 408, 397, 1221, 395)

df <- data.frame(Household = factor(households), Outliers = outliers)

# Ensure Household is a factor and ordered
df$Household <- factor(df$Household, levels = sort(unique(df$Household)))

# Identify which x-axis labels to color
x_colors <- ifelse(levels(df$Household) == "34", "darkred", "black")

# Bold label for x-axis "34"
df$LabelBold <- ifelse(df$Household == "34", "bold", "bold")  # All labels bold

# Assign shape based on outlier value
df$Shape <- ifelse(df$Outliers == 2100, "asterisk", "circle")

# Create the plot
outliers_per_household <- ggplot(df, aes(x = Household, y = Outliers)) +
  # Plot all points, but size and color adjusted for 2100
  geom_point(aes(shape = Shape, size = Outliers == 2100, color = Outliers == 2100)) +
  
  # Outlier labels bold
  geom_text(aes(label = Outliers), fontface = "bold", vjust = -0.8, size = 3.5) +
  
  # Shape and color settings
  scale_shape_manual(values = c("asterisk" = 8, "circle" = 1)) +
  scale_size_manual(values = c("TRUE" = 7, "FALSE" = 3), guide = "none") +
  scale_color_manual(values = c("TRUE" = "darkred", "FALSE" = "darkred"), guide = "none") +
  
  labs(title = "Outliers per Household", x = "Household", y = "Outliers") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = x_colors, angle = 90, vjust = 0.5, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )


# Print the plot
print(outliers_per_household)

ggsave("E:/case_study/project_1/outliers_per_household.png",
       plot = outliers_per_household,
       width = 14, height = 8, dpi = 100, device = "png")
