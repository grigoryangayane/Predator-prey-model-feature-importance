library(ggplot2)
##############################################################
#### C1
##### load the data
pred_prey <- read.csv("C1.csv") 
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c1_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals"),
  #Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
  Shapley_values = c(0.01132310, 0.18407584, 0.25398666, 0.02509582) #c1
)       


exp1 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Shapley Values", x = "Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

exp1
exp1 <- exp1 + theme(legend.key.width = unit(1.7, "cm"))
ggsave("exp1.png", exp1, dpi = 300, width = 8, height = 4)


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)


#########################################
pred_prey <- read.csv("C2.csv")
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c2_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)

###########################################
pred_prey <- read.csv("C3.csv")
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c3_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)

#######################################################
pred_prey <- read.csv("C4.csv")
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c4_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)
#######################################################################

pred_prey <- read.csv("C5.csv") 
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c5_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals"),
  Shapley_values = c(0.017227903, 0.097394722, 0.601897657, 0.005761675)
)       


exp5 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Shapley Values", x = "Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

exp5
exp5 <- exp5 + theme(legend.key.width = unit(1.7, "cm"))
ggsave("exp5.png", exp5, dpi = 300, width = 8, height = 4)


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)


#########################################################################################
pred_prey <- read.csv("C6.csv")
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c6_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)
##############################################################

pred_prey <- read.csv("C7.csv")
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c7_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)


###########################################################
##### load the data
pred_prey <- read.csv("predator-prey.csv") # this is C8
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals", "external")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals + external, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


ptop <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Shapley Values", x = "Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 4),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 7))

ptop
ggsave("ptop.png", ptop, dpi = 300)

#####
ptop <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Shapley Values", x = "Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

ptop
ptop <- ptop + theme(legend.key.width = unit(1.7, "cm"))
ggsave("ptop.png", ptop, dpi = 300, width = 8, height = 4)

####


p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Shapley Values", x = "Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = c(0.99, 0.05), # Adjust the position values as needed
        legend.justification = c(1, 0),
        legend.box = "horizontal",
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))

p3

ggsave("plot.png", p3, dpi = 300)

##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)



 ############################################################
pred_prey <- read.csv("C9.csv")
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals", "external")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals + external, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c9_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals", "external"),
  Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571)
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)
##################################################################

pred_prey <- read.csv("C10.csv") 
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\c10_predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(5, r_squaredCoalitions)
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals", "external")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals"),
  Shapley_values = c(0.01886637, 0.33991805, 0.31540826, 0.05401234)
)       

exp10 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Shapley Values", x = "Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

exp10
exp10 <- exp10 + theme(legend.key.width = unit(1.7, "cm"))
ggsave("exp10.png", exp10, dpi = 300, width = 8, height = 4)



##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)


######################################################################
#####################################################################
####################################################################
# Analysis of feature importance values across different experiments 
##### load the data
pred_prey <- read.csv("predator-prey.csv") # this is C8
pred_prey <- read.csv("C1.csv") # C1,C2, C3, C4, C5, C6, C7, C10
names(pred_prey) <- c("rotifers", "algae", "egg_ratio", "eggs", "dead_animals", "external")
View(pred_prey)

model1.1 = lm(rotifers ~ algae +eggs+ egg_ratio + dead_animals + external, data = pred_prey)
summary(model1.1)


variables <- colnames(pred_prey)[2:ncol(pred_prey)] # specifies what are the independent variables
variables # shows the independent variables
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+") 
  tmp <- paste0("rotifers~", tmp)
  formulas[[i]] <- tmp
}

formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=pred_prey)

r_squared_results <- sapply(models, function(x) summary(x)$r.squared)

write.csv(r_squared_results,"G:\\My Drive\\0 - My_Publications\\2 - working\\why shapley and not other cgt solutions for xai\\seatpos-2022\\predator_prey_r_squared.csv") 
# if you don't want the first column to be included you can use the row.names = FALSE in write.csv()

###########
#Running the Shapley values
install.packages("GameTheory")
library(GameTheory)

r_squared_data <- read.csv("predator_prey_r_squared.csv") #this is c8 - done
r_squared_data <- read.csv("c1_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c2_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c3_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c4_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c5_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c6_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c7_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c9_predator_prey_r_squared.csv") #done
r_squared_data <- read.csv("c10_predator_prey_r_squared.csv")
View(r_squared_data)

r_squaredCoalitions <- c(r_squared_data$x)
r_squaredCoalitions
features <- DefineGame(4, r_squaredCoalitions) # change into 5 when variables "external" is in the model
summary(features)
feature_names <- c("algae", "egg_ratio", "eggs", "dead_animals")
length(feature_names)
ShapleyValueCalculation <- ShapleyValue(features, feature_names)
ShapleyValueCalculation

# plot the graph
shapley_val <- data.frame(
  Feature = c("algae", "egg ratio", "eggs", "dead animals"), # "external is only used for C8 and C9"
  #Shapley_values = c(0.01132310, 0.18407584, 0.25398666, 0.02509582) #c1
  #Shapley_values = c(0.018824121, 0.239280518, 0.360122973, 0.009306356) #c2
  #Shapley_values = c(0.009328845, 0.252289541, 0.422956144, 0.008985991) #c3
  #Shapley_values = c(0.02457655, 0.31232762, 0.37274048, 0.02301654) #c4
  #Shapley_values = c(0.017227903, 0.097394722, 0.601897657, 0.005761675) #c5
  # Shapley_values = c(0.001206235, 0.213750997, 0.429051729, 0.017652937) #c6
  #Shapley_values = c(0.0114180, 0.1484029, 0.2868415, 0.0226937) #c7
  #Shapley_values = c(0.013674023, 0.082713690, 0.500157144, 0.005393282, 0.060462571) #c8
  #Shapley_values = c(0.022986963, 0.116110101, 0.581323326, 0.003833459, 0.017280255) #c9
  Shapley_values = c(0.01886637, 0.33991805, 0.31540826, 0.05401234) #c10
)       

p3 <- ggplot(shapley_val, aes(x = reorder(row.names(shapley_val), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056")) + #"#8B8B00" - color for "external"
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.title = element_blank())

p3

###############################
algae <- data.frame(
  Feature = c("Exp. 1", "Exp. 2", "Exp. 3", "Exp. 4", "Exp. 5", "Exp. 6", "Exp. 7", "Exp. 8", "Exp. 9", "Exp. 10"), # "external is only used for C8 and C9"
  Shapley_values = c(0.01132, 0.018824121, 0.009328845, 0.02457655, 0.017227903, 0.001206235, 0.011418, 0.013674, 0.022986963, 0.01886637) #c10
)       


##############################
alg <- ggplot(algae, aes(x = reorder(row.names(algae), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00", "#8b0000", "#3f0000", "#003f3f", "#3f3f00", "#720072")) + #"#8B8B00" - color for "external"
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

alg
alg <- alg + theme(legend.key.width = unit(1.7, "cm"))
ggsave("algae.png", alg, dpi = 300, width = 8, height = 4)

############################################################################
egg_ratio <- data.frame(
  Feature = c("Exp. 1", "Exp. 2", "Exp. 3", "Exp. 4", "Exp. 5", "Exp. 6", "Exp. 7", "Exp. 8", "Exp. 9", "Exp. 10"), # "external is only used for C8 and C9"
  Shapley_values = c(0.18407, 0.239280518, 0.252289541, 0.31232762, 0.097394722, 0.213750997, 0.1484029, 0.08271, 0.116110101, 0.33991805) #c10
)       

egg_rat <- ggplot(egg_ratio, aes(x = reorder(row.names(algae), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00", "#8b0000", "#3f0000", "#003f3f", "#3f3f00", "#720072")) + #"#8B8B00" - color for "external"
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

egg_rat
egg_rat <- egg_rat + theme(legend.key.width = unit(1.7, "cm"))
ggsave("egg_ratio.png", egg_rat, dpi = 300, width = 8, height = 4)

############################################################################
eggs <- data.frame(
  Feature = c("Exp. 1", "Exp. 2", "Exp. 3", "Exp. 4", "Exp. 5", "Exp. 6", "Exp. 7", "Exp. 8", "Exp. 9", "Exp. 10"), # "external is only used for C8 and C9"
  Shapley_values = c(0.253986, 0.360122973, 0.422956144, 0.37274048, 0.601897657, 0.429051729, 0.2868415, 0.5001, 0.581323326, 0.31540826) #c10
)       

egg <- ggplot(eggs, aes(x = reorder(row.names(algae), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00", "#8b0000", "#3f0000", "#003f3f", "#3f3f00", "#720072")) + #"#8B8B00" - color for "external"
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

egg
egg <- egg + theme(legend.key.width = unit(1.7, "cm"))
ggsave("eggs.png", egg, dpi = 300, width = 8, height = 4)

###########################################################################
dead_animals <- data.frame(
  Feature = c("Exp. 1", "Exp. 2", "Exp. 3", "Exp. 4", "Exp. 5", "Exp. 6", "Exp. 7", "Exp. 8", "Exp. 9", "Exp. 10"), # "external is only used for C8 and C9"
  Shapley_values = c(0.025095, 0.009306356, 0.008985991, 0.02301654, 0.005761675, 0.017652937, 0.0226937, 0.00539, 0.003833459, 0.05401234) #c10
)       

d_animals <- ggplot(dead_animals, aes(x = reorder(row.names(algae), Shapley_values), y = Shapley_values, fill = factor(Feature))) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00", "#8b0000", "#3f0000", "#003f3f", "#3f3f00", "#720072")) + #"#8B8B00" - color for "external"
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values")+
  xlab("Features")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"), # Adjust the size as needed
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10))

d_animals
d_animals <- d_animals + theme(legend.key.width = unit(1.7, "cm"))
ggsave("d_animals.png", d_animals, dpi = 300, width = 8, height = 4)


###########################################
library(ggplot2)
library(dplyr)

dead_animals <- data.frame(
  Feature = paste("Exp.", 1:10),
  Shapley_values = c(0.025095, 0.009306356, 0.008985991, 0.02301654, 0.005761675, 0.017652937, 0.0226937, 0.00539, 0.003833459, 0.05401234)
)

# Reorder the rows based on Shapley values
dead_animals <- dead_animals %>%
  arrange(Shapley_values)

# Add a row number column
dead_animals$row_number <- seq_len(nrow(dead_animals))

d_animals <- ggplot(dead_animals, aes(x = row_number, y = Shapley_values, fill = Feature)) +
  scale_fill_manual(values = c("#008B8B", "#00468B", "#59C7EB", "#9B0056", "#8B8B00", "#8b0000", "#3f0000", "#003f3f", "#3f3f00", "#720072")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Shapley Values") +
  xlab("Features") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 1, fill = NA),
        legend.position = "top",
        legend.justification = "center",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "lines"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_x_continuous(breaks = dead_animals$row_number, labels = dead_animals$Feature)

d_animals
d_animals <- d_animals + theme(legend.key.width = unit(1.7, "cm"))
ggsave("d_animals.png", d_animals, dpi = 300, width = 8, height = 4)

###########################################################################
##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)

##############################################################