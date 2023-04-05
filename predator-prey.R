##### load the data
pred_prey <- read.csv("predator-prey.csv")
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


##  correlation analysis
cor(pred_prey$rotifers, pred_prey$algae)
cor(pred_prey$rotifers, pred_prey$egg_ratio)
cor(pred_prey$rotifers, pred_prey$eggs)
cor(pred_prey$rotifers, pred_prey$dead_animals)
cor(pred_prey$rotifers, pred_prey$external)
