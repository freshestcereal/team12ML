rm(list=ls())
library(tree)
library(ISLR)
attach(Carseats)

high <- ifelse(Sales <=8, "No", "Yes")
Carseats <- data.frame(Carseats, high)

tree.carseats <- tree(high~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats, type = 'uniform', col = 'blue')
box(col="black")
text(tree.carseats, pretty = 0, cex = .42)

tree.carseats

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
high.test <- high[-train]
tree.carseats <- tree(high~.-Sales, Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type='class')
table(tree.pred,high.test)
(104+50)/200

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)                    
cv.carseats

plot(cv.carseats$size, cv.carseats$dev, type = 'b', axes = FALSE, xlab = "", ylab = "")
box(col="black")
axis(1, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 1)
axis(2, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 1)
mtext("Nodes", side = 1, line = 3, col = "black", cex = 1)
mtext("Deviance", side = 2, line = 3, col = "black", cex = 1)
plot(cv.carseats$k, cv.carseats$dev, type = 'b', ylab = '', xlab = '')
box(col="black")
axis(1, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 1)
axis(2, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 1)
mtext("K", side = 1, line = 3, col = "black", cex = 1)
mtext("Deviance", side = 2, line = 3, col = "black", cex = 1)

prune.carseats <- prune.misclass(tree.carseats, best=21)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, high.test)
(97+58)/200
