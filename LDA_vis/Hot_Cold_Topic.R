#install.packages("xtable")
#install.packages("lattice")
library(xtable)
library(lattice)

# length(dataset_original$Year)
# str(dataset_original$Year)
# str(dataset_original)
# length(theta)

### Hot and Cold Topics

years <- levels(factor(dataset_original$Year))
topics_n <- m@k
theta <- theta

theta_mean_by_year_by <- by(theta, dataset_original$Year, colMeans)
theta_mean_by_year <- do.call("rbind",theta_mean_by_year_by)
colnames(theta_mean_by_year) = paste(1:topics_n)
theta_mean_by_year_ts <- ts(theta_mean_by_year, start = as.integer(years[1]))
theta_mean_by_year_time <- time(theta_mean_by_year)

theta_mean_lm <- apply(theta_mean_by_year, 2, function(x) lm(x ~ theta_mean_by_year_time))
theta_mean_lm_coef <- lapply(theta_mean_lm, function(x) coef(summary(x)))
theta_mean_lm_coef_sign <- sapply(theta_mean_lm_coef, '[',"theta_mean_by_year_time","Pr(>|t|)")
theta_mean_lm_coef_slope <- sapply(theta_mean_lm_coef, '[',"theta_mean_by_year_time","Estimate")

theta_mean_lm_coef_slope_pos <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope >= 0]
theta_mean_lm_coef_slope_neg <- theta_mean_lm_coef_slope[theta_mean_lm_coef_slope < 0]

p_level <- c(0.05, 0.01, 0.001, 0.0001)
significance_total <- 
  sapply(p_level, function(x) (theta_mean_lm_coef_sign[theta_mean_lm_coef_sign < x]))
significance_neg <- 
  sapply(1:length(p_level), function(x) intersect(names(theta_mean_lm_coef_slope_neg), names(significance_total[[x]])))
significance_pos <- 
  sapply(1:length(p_level), function(x) intersect(names(theta_mean_lm_coef_slope_pos), names(significance_total[[x]])))


### C.14
library(xtable)

outputmat <- rbind(sapply(significance_neg , length), 
                   sapply(significance_pos , length), 
                   sapply(significance_total , length))
rownames(outputmat) <- c(" Negative trend" , " Positive trend" , " Total" )
colnames(outputmat) <- paste("p-level=", format(p_level, drop0trailing = TRUE , scientific = FALSE), sep=" ") 
xTable <- xtable(outputmat) 
xTableStrings <- unlist(strsplit(capture.output(print(xTable)), " \n" )) 
xTableStrings <- xTableStrings [ xTableStrings != " \\ begin{table }[ht]" ] 
xTableStrings <- xTableStrings [ xTableStrings != " \\end{table}" ] 
xTableStrings <- append(xTableStrings , " \\ hline" , after = 9) 
cat(xTableStrings , sep = " \n" )
table1 <- cat(xTableStrings , sep = " \n" )


### C.15 Original
# library(lattice)
# print(xyplot(theta_mean_by_year_ts[,names(sort(theta_mean_lm_coef_slope))], layout = c(5, 6),
#              #screens = c(rep(" cold topics" , 5), rep(" hot topics" , 5)), 
#              screens = rep (1:30 , each = 10),
#              superpose = TRUE, 
#              col = "blue",
#              alpha = 0.3, 
#              ylim = c(0, 0.015),
#              #ylab "Mean theta", 
#              ylab = expression(paste("Mean" ,theta)),
#              xlab = "Year",
#              type = c("1", "g"),
#              #aspect = "xy",
#              #auto.key = list(space = "right"),
#              auto.key = FALSE,
#              scales = list(x = list(alternating = FALSE)), 
#              #par.settings = standard.theme(color = FALSE)
# ))


### C.15
library(lattice)
print(xyplot(theta_mean_by_year_ts[,names(sort(theta_mean_lm_coef_slope))],
             layout = c(5, 6),
             #screens = c(rep(" cold topics" , 5), rep(" hot topics" , 5)), 
             screens = rep(1:32 , each = 1),
             #screens = rep(1:14) #each = 1),
             superpose = TRUE, 
             col = "blue",
             alpha = 0.3, 
             ylim = c(0, 0.015),
             #ylab "Mean theta", 
             ylab = expression(paste("Mean" ,theta)),
             xlab = "Year",
             type = c("1", "g"),
             #aspect = "xy",
             #auto.key = list(space = "right"),
             auto.key = FALSE,
             scales = list(x = list(alternating = FALSE)), 
             par.settings = standard.theme(color = FALSE)
))





topics_hot <- as.numeric(names(sort(theta_mean_lm_coef_slope[significance_pos[[4]]], decreasing=TRUE)))
topics_cold <- as.numeric(names(sort(theta_mean_lm_coef_slope[significance_neg[[4]]], decreasing=FALSE)))


### C.16 Orginal
cold_and_hot_ts <- cbind(theta_mean_by_year_ts[,topics_cold[1:5]] ,theta_mean_by_year_ts[,topics_hot[1:5]], deparse.level=0)


colnames(cold_and_hot_ts) <-as.character(c(topics_cold[1:5], topics_hot[1:5]))

print(xyplot(
  cold_and_hot_ts,layout=c(2, 1), screens=c(rep("Cold topics", 5), rep("Hot topics", 5)), superpose = TRUE, ylim = c(0, 0.015), ylab=expression(paste("Mean", theta)), xlab = "Year", type = c("l", "g"), auto.key = list(space = "right"), scales = list(x=list(alternating=FALSE))
  #par. settings = standard . theme ( color = FALSE )
))


### C.16
cold_and_hot_ts <- cbind(theta_mean_by_year_ts[,topics_cold[1:5]] ,theta_mean_by_year_ts[,topics_hot[1:5]], deparse.level=0)


colnames(cold_and_hot_ts) <-as.character(c(topics_cold[1:5], topics_hot[1:5]))

print(xyplot(
  cold_and_hot_ts,layout=c(2, 1), screens=c(rep("Cold topics", 5), rep("Hot topics", 5)), superpose = TRUE, ylim = c(0, 0.6), ylab=expression(paste("Mean", theta)), xlab = "Year", type = c("l", "g"), auto.key = list(space = "right"), scales = list(x=list(alternating=FALSE))
  #par. settings = standard . theme ( color = FALSE )
))

#theta_mean_lm_coef_sign
