# Jialuo Ding 44732024
# This file aim to classify the data

# Extract data
bcw = readRDS("./data/bcw_processed.Rda")

# For reproducible result
set.seed(2024)

# Set traning and test ratio
m = nrow(bcw)
training_percentage = 0.7
test_percentage = 0.3

# Sample random index
ind <- sample(2, m, replace = TRUE, prob = c(training_percentage, test_percentage))

# Select training and test data
training_data = bcw[ind == 1, ]
test_data = bcw[ind == 2, ]

# Divide features and labels
training_features <- training_data[,1:9]
training_labels<- training_data[,10]
test_features<-test_data[,1:9]
test_labels<-test_data[,10]

# Install and import "party" library
install.packages("party")
library(party)

# Specify target and predictors
myFormula <- Class~Clump.Thickness+Uniformity.of.Cell.Size+
  Uniformity.of.Cell.Shape+Marginal.Adhesion+Single.Epithelial.Cell.Size+
  Bare.Nuclei+Bland.Chromatin+Normal.Nucleoli+Mitoses

# Generate classification tree
bcw_ctree <- ctree(myFormula, data = training_data)

# visualise the tree
jpeg(filename = "./plots/ctree.jpeg")
plot(bcw_ctree)
dev.off()
jpeg(filename = "./plots/ctree_simple.jpeg")
plot(bcw_ctree, type="simple")
dev.off()

# predict test labels
ctree_pred <- predict(bcw_ctree, newdata = test_features)

# create the confusion matrix
cm = as.matrix(table(Actual = test_labels, predicted = ctree_pred))

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm,1,sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class

# Calculate the accuracy, precision, recall and f1
accuracy = sum(diag)/n
precision = diag/colsums
recall = diag/rowsums
f1 = 2*precision*recall/(precision+recall)

result <- data.frame(accuracy, precision, recall, f1)
result

# Build my classification tree
my_bcw_ctree <- ctree(myFormula, data = training_data, 
                      controls =ctree_control(minbucket = 3L, minsplit = 7L, 
                                              testtype = "Bonferroni" ,mincriterion = 0.1))

# visualise the tree
jpeg(filename = "./plots/my_ctree.jpeg")
plot(my_bcw_ctree)
dev.off()
jpeg(filename = "./plots/my_ctree_simple.jpeg")
plot(my_bcw_ctree, type="simple")
dev.off()

# predict test labels
my_ctree_pred <- predict(my_bcw_ctree, newdata = test_features)
# create the confusion matrix
my_cm = as.matrix(table(Actual = test_labels, predicted = my_ctree_pred))
my_n = sum(my_cm) # number of instances
my_nc = nrow(my_cm) # number of classes
my_diag = diag(my_cm) # number of correctly classified instances per class
my_rowsums = apply(my_cm,1,sum) # number of instances per class
my_colsums = apply(my_cm, 2, sum) # number of predictions per class

# Calculate the accuracy, precision, recall and f1
my_accuracy = sum(my_diag)/my_n
my_precision = my_diag/my_colsums
my_recall = my_diag/my_rowsums
my_f1 = 2*my_precision*my_recall/(my_precision+my_recall)

my_result <- data.frame(my_accuracy, my_precision, my_recall, my_f1)

# Compare the two results
my_result
result

# Install and import "class" library
install.packages("class")
library(class)

# Create a function to calculate accuracy, precision and recall
calculation <- function(my_k){
  set.seed(2024)
  # Classify using K-NN
  knn_pre <- knn(train = training_features, test = test_features, cl = training_labels, k = my_k)
  # create the confusion matrix
  cm = as.matrix(table(Actual = test_labels, predicted = knn_pre))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm,1,sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  
  # Calculate the accuracy, precision, recall and f1
  accuracy = sum(diag)/n
  precision = diag/colsums
  recall = diag/rowsums
  f1 = 2*precision*recall/(precision+recall)
  
  result <- data.frame(accuracy, precision, recall, f1)
  return(result)
}

# k = 1
calculation(1)

# k = 2
calculation(2)

# k = 3
calculation(3)

# k = 4
calculation(4)

# k = 5
calculation(5)
   
