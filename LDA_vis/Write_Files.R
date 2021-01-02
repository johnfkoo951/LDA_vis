install.packages("xlsx")
library(xlsx)

### Write Data in Excel
write.xlsx(ddd,"/.xlsx", sheetName = "Sheet1",
           col.name = TRUE, row.names = TRUE, append = FALSE)

### Append Data in Same File
write.xlsx(ddd2,"/.xlsx", sheetName = "Sheet1",
           col.name = TRUE, row.names = TRUE, append = TRUE)



### Sheet 1 ###
write.xlsx(theta_mean_by_year,"hot_cold_theta.xlsx", sheetName = "theta_mean_by_year",
           col.name = TRUE, row.names = TRUE, append = FALSE)

### Sheet 2 ###
write.xlsx(theta_mean_lm_coef,"hot_cold_theta.xlsx", sheetName = "theta_mean_lm_coef",
           col.name = TRUE, row.names = TRUE, append = TRUE)




str(theta_mean_lm)
df_yh <- data.frame(matrix(unlist(theta_mean_lm), nrow=length(theta_mean_lm), byrow=T))
df_yh2 <- data.frame(t(matrix(unlist(theta_mean_lm), nrow=length(theta_mean_lm), byrow=T)))
df_yh3 <- data.frame(matrix(unlist(theta_mean_lm$[1]), nrow=length(theta_mean_lm), byrow=T))

df_yh4 <- data.frame(matrix(unlist(theta_mean_lm[[1]]), nrow=length(theta_mean_lm), byrow=T))

print(theta_mean_lm[[1]])
print(theta_mean_lm[1])

ni <- topics_n
i <- 1
for(i in ni){
  result = (i)
  print(result)
}
i <- 1
j <- 2
paste(i, "*", j, "=", i*j)

i=4
temp <- data.frame()
for(i in 1:K){
  temp <- data.frame(t(matrix(unlist(theta_mean_lm[i][12]), nrow=length(theta_mean_lm[i][12]), byrow=T)))
}
for(i in 1:K){
  #paste("moddel",i) <- data.frame()
  #paste("model",i) <- as.data.frame(theta_mean_lm[i][12])
  temp <- data.frame(theta_mean_lm[[i]][12])
  myfile <- file.path(/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA Research, paste0("model","_",i,".xlsx"))
  write.xlsx(temp, myfile, sheetName = "Sheet1",
             col.name = TRUE, row.names = TRUE, append = FALSE)
}

write.xlsx(temp, "/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA Research/myfile.xlsx", sheetName = "Sheet1",
           col.name = TRUE, row.names = TRUE, append = FALSE)


data.frame(theta_mean_lm[[i]][12])
model_[i] <- 1
paste("model",i)
modelstr(theta_mean_lm[[i]][12])
mmm <- data.frame(theta_mean_lm[[i]][12])
mmm <- list(theta_mean_lm[[i]][12])
### 토픽이름 열추가
mmm$kd <- data.frame(rep(K,length(mmm$model.x)))

write.xlsx(mmm, "/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/LDA Research/myfile.xlsx", sheetName = "Sheet1",
           col.name = TRUE, row.names = TRUE, append = FALSE)

mmm$k <- mmm$model.x * mmm$model.theta_mean_by_year_time
mmm$k <- c(1,2,2)

str(theta_mean_lm)
View(theta_mean_lm)
i <- 1
theta_mean_lm[[i]][12][1]

mmm$k <- data.frame(rep(i,length(mmm$model.x)))
mmm2 <- mmm
write.xlsx(mmm2, "myfile2.xlsx", sheetName = "Sheet1",
           col.name = TRUE, row.names = TRUE, append = FALSE)
str(mmm)
str(mmm2)


theta_mean_lm
df_yh <- data.frame(matrix(unlist(theta_mean_lm), nrow=length(theta_mean_lm), byrow=T))
df_yh2 <- data.frame(t(matrix(unlist(theta_mean_lm), nrow=length(theta_mean_lm), byrow=T)))

View(df_yh)
str(df_yh)


write.xlsx(theta,"/Users/yhn_hac/Hanyang University/01-2. Study_Alone/R Data Analysis/LDA (Latent Dirichlet Allocation)/theta1.xlsx", sheetName = "Sheet1", col.name = TRUE, row,names = TRUE, append = FALSE)
