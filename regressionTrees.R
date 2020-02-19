library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty=0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

par(bg = 'black')
yhat <- predict(tree.boston,newdata = Boston[-train,])
boston.test <- Boston[-train,'medv']
plot(yhat, boston.test, col = 'white')
box(col = "aquamarine")
axis(1, col = "aquamarine", col.ticks = "aquamarine", col.axis = "white", cex.axis = 1)
axis(2, col = "aquamarine", col.ticks = "aquamarine", col.axis = "white", cex.axis = 1)
mtext("Y^", side = 1, line = 3, col = "aquamarine", cex = 1)
mtext("Test", side = 2, line=3, col = "aquamarine", cex = 1)
abline(0,1, col = 'white')
mean((yhat-boston.test)^2)
