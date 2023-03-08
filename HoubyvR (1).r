library(caret)
library(ranger)
library(dplyr)
library(e1071)

set.seed(123)

df= read.csv('mushrooms.csv')

summary(df)
# zde vidíme četnosti jednotlivých charakteristik

sapply(df, class)

colSums(is.na(df))
# žádné nulové hodnoty

prop.table(table(df$class))
# poměr počtu jedovatý/jedlý, je vyrovnaný, není třeba over/undersampling

lapply(df,unique)
# stalk root má 1 missing value ve formě otazníku a veil type obsahuje pouze 1 proměnnou

ggplot(df, aes(x =class, y = cap.color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of cap.color edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))
ggplot(df, aes(x = class, y = odor, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of odor edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = cap.shape, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of cap.shape edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = cap.surface, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of cap.surface edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = bruises, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of bruises edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = gill.spacing, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of gill.spacing edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = gill.attachment, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of gill.attachment edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = gill.size, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of gill.size edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y =gill.color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of gill.color edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = stalk.shape, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of stalk.shape edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = stalk.root, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of stalk.root edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = stalk.surface.above.ring, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of stalk.surface.above.ring edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y =stalk.surface.below.ring, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of stalk.surface.below.ring edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = stalk.color.above.ring, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of stalk.color.above.ring edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = stalk.color.below.ring, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of stalk.color.below.ring ediblity") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = veil.color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of veil.color edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = ring.number, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of ring.number ediblity") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = ring.type, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of ring.type edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = spore.print.color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of spore.print.color edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = habitat, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of habitat edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))

ggplot(df, aes(x = class, y = population, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("e", "p"), 
                     values = c("green", "black")) +
  labs(title = "Scatterplot of population edibility") +
  theme(axis.title = element_blank(), 
        text = element_text(size = 16)) +
  coord_cartesian(xlim = c(0.5, 4))


df_drop <- select(df, -gill.attachment, -veil.color, -stalk.root, -veil.type)

encode_numeric <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

df_drop["ring.number"] <- encode_numeric(df_drop$ring.number)-1

df_new <- mutate(df_drop,
  avgRingsByCapColor = group_by(df_drop, cap.color)$ring.number/length(df_drop$ring.number)
)

# tvorba nového sloupce a zakódování na numerické hodnoty

mushsample <- caret::createDataPartition( df_new$class, , p = 0.75, list = FALSE)
train_m <- df_new[mushsample, ]
test_m <- df_new[-mushsample, ]
# test a train rozdělení v poměru 0.75

cat("Number of rows: ", length(test_m$class))

cat("Number of rows: ", length(train_m$class))

train <-  data.frame(predict(dummyVars(" ~ .", data = train_m[, !(names(test_m) %in% c("ring.number", "class"))], fullRank=T), newdata=train_m[, !(names(train_m) %in% c("ring.number", "class"))]))

train["ring.number"] <- train_m["ring.number"]
train$class <- ifelse(train_m$class == "e", 1, 0)

test<- data.frame(predict(dummyVars(" ~ .", data = test_m[, !(names(test_m) %in% c("ring.number", "class"))], fullRank=T), newdata=test_m[, !(names(test_m) %in% c("ring.number", "class"))]))

test["ring.number"] <- test_m["ring.number"]
test$class <- ifelse(test_m$class == "e", 1,0)

# při analýze pracujeme s numerickými hodnotami, zbytek datasetu tedy pomocí dummyVars zbinarizujeme

fit_control <- trainControl(method = "cv",
                           number = 10)

# random forest
rf_fit <- train(as.factor(class) ~ ., 
                data = train, 
                method = "ranger",
               trControl = fit_control)

rf_fit

rf_pred <- predict(rf_fit, test)

confusionMatrix(rf_pred, as.factor(test$class))

# logisticka regrese
logistic_model <- glm(as.factor(class) ~ ., 
                      data = train, 
                      family = "binomial")

logistic_pred <- predict(logistic_model, 
                       test, type = "response")

logistic_pred <- ifelse(logistic_pred>0.5, 1, 0)
   
confusionMatrix(as.factor(logistic_pred), as.factor(test$class))  

library(pROC)



# create roc curve
roc_object <- roc( test$class, logistic_pred)
 
# calculate area under curve
auc( roc_object )

plot(roc_object)


