rm(list=ls())

## 1

# a

library(faraway)

data(pima)

pima$test <- factor(pima$test)

levels(pima$test) <- c("brak objawow","sa objawy")

pima$diastolic[pima$diastolic==0]=NA

d <- pima$diastolic

# b

mean_d <- mean(d, na.rm=T)

med_d <- median(d, na.rm=T)

q1_d <- quantile(d, 0.25, na.rm=T)

q3_d <- quantile(d, 0.75, na.rm=T)

p10_d <- quantile(d, 0.1, na.rm=T)

range_d <- max(d, na.rm=T)-min(d, na.rm=T)

iqr_d <- IQR(d, na.rm=T)

iqr_d2 <- q3_d - q1_d

std_d <- sd(d, na.rm=T)

# c

d2 <- d[pima$test=="sa objawy"]

mean_d2 <- mean(d2, na.rm=T)

std_d2 <- sd(d2, na.rm=T)

# d

p <- pima$pregnant

png(filename="1boxplot_d.png")

boxplot(p, main = "Pregnancy in Pima tribe", xlab = "Pima women", ylab = "Number of times pregnant", col = "blue")

dev.off()

# e

count_d2 <- length(d2)

# f

d3 <- d[!is.na(d)]

png(filename="1hist_f.png")

hist(d3, main = "histogram of diastolic", xlab = "diastolic")

dev.off()

dens <- density(d3)

png(filename="1dens_f.png")

plot(dens)

dev.off()

## 2

# a

data <- read.table("gala_data.txt", header = T)

attach(data)

#b

s <- data$Species

mean_s <- mean(s, na.rm=T)

med_s <- median(s, na.rm=T)

q1_s <- quantile(s, 0.25, na.rm=T)

q3_s <- quantile(s, 0.75, na.rm=T)

max_s <- max(s, na.rm=T)

min_s <- min(s, na.rm=T)

range_s <- max_s - min_s

iqr_s <- IQR(s, na.rm=T)

iqr_s2 <- q3_s - q1_s

std_s <- sd(s, na.rm=T)

var_s <- var(s, na.rm=T)

#c

png(filename = "2hist_area.png")

hist(data$Area, breaks =4, main = "Area histogram", xlab = "Area")

dev.off()

#d

png(filename = "2boxplot_area.png")

boxplot(s[data$Area<25], main = "Number of species on islands bigger than 25km^2", xlab = "Turtles", ylab = "Number of species", col = "red")

dev.off()

detach(data)

## 3

#a

library(moments)

a1 <- (-1-sqrt(10))/4

a2 <- -1/4

a3 <- (sqrt(10)-1)/4

a4 <- 1

a <- c(a1, a2, a2, a3, a4)

mean_a <- mean(a)

med_a <- median(a)

skew_a <- skewness(a)

png(filename = "3skewness.png")

plot(a)

dev.off()

## 4

#a

z <- c(5, 8, 9, 3, 8, 7)

med_z = median(z)

q1_z = quantile(z, 0.25)

q3_z = quantile(z, 0.75)