rm(list=ls())
library(tree)
library(ISLR)
attach(OJ)

###A
train.oj <- sample(1:nrow(OJ), 800)
test.oj <- OJ[-train.oj,]

###B
tree.oj <- tree(Purchase~., OJ[train.oj,])
sum.tree.oj <- summary(tree.oj)

print(paste('The training error rate is', 124/800, 'and it has', sum.tree.oj$size, 'terminal nodes.'))

###C
tree.oj
'''
Beginning with LoyalCH < 0.48285, if that condition is true, then the path
follows to the left. This is the same for all branches. The next branch is
LoyalCH < 0.276142. In this instance, when true, it results in a terminal (ending)
node of MM. When greater than that value, it proceeds to one final split determined
by PriceDiff < 0.065. The result of this evaluation is two terminal nodes.
'''

###D
par(bg = 'white')
plot(tree.oj)
text(tree.oj, pretty = 0)
'''
Higher values of LoyalCH (greater than 0.48285) are more likely to result in
terminal nodes favoring CH, whereas lower values are more likely to end in MM.
'''

###E
oj.tree.pred <- predict(tree.oj, test.oj, type='class')
(tree.cm <- table(oj.tree.pred, test.oj$Purchase))
(tree.error <- 1 - (tree.cm[1,1] + tree.cm[2,2])/sum(tree.cm))


###F
(cv.oj <- cv.tree(tree.oj, FUN=prune.misclass))

###G
plot(cv.oj$size, cv.oj$dev, type = 'b')


###H
(best.prune <- cv.oj$size[which.min(cv.oj$dev)])


###I
prune.oj <- prune.misclass(tree.oj, best = best.prune)


###J
sum.prune.oj <- summary(prune.oj)
print(paste("The unpruned tree has a slightly lower training error rate of",
            124/800, "compared to the pruned tree's", 130/800, "error rate."))

###K
prune.tree.pred <- predict(prune.oj, test.oj, type='class')
(prune.cm <- table(prune.tree.pred, test.oj$Purchase))
(prune.error <- 1 - (prune.cm[1,1] + prune.cm[2,2])/sum(prune.cm))
print(paste("The pruned tree has a slightly higher test error rate of",
            sprintf('%.4f', prune.error), "compared to the unpruned tree's",
            sprintf('%.4f', tree.error), "error rate."))
      
            