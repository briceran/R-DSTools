
library(pracma)# allows usage of randn to create
# random matrices more easily

library(data.table) # allows us to use function fread,
# which quickly reads data from csv files

set.seed(1)

sigmoid <- function(x){
  return( 1/(1 + exp(-x)))
}

dsigmoid <- function(y) {
  return(y*(1 - y))
}


loss_l2 <- function(p, y) {

#Use L2-norm (least squares error) as loss function.

#L = \sum_{i=1}^{n} (y_i - p_i)^2

#Args:
#p: predicted probability of shape (n, 1).
#y: true values of shape (n, 1).

#Returns:
#L: the loss. It is a scalar.
#dp: the derivative of loss to predictions p of shape (n, 1)

loss = sum((y - p)^2)
dp = -(y - p)
return (list(loss, dp))

}

loss_neg_likelihood <- function(p, y) {

# Average negative log likelihood as loss function.

#L  = - \sum_{i=1}{n} ( y_i \log p_i + (1-y_i) \log (1-p_i) ) / n

#Args:
#p: predicted probability of shape (n, 1).
#y: true values of shape (n, 1).

#Returns:
#L: the loss. It is a scalar.
#dp: the derivative of loss to predictions p of shape (n, 1)
#"""

loss = -sum(y*log(p) + (1-y)*log(1-p) ) / dim(p)[1]
dy = (1 - y)/(1 - p) - (y / p)
dy = dy / (dim(y)[1])
return(list(loss, dy))
}

accuracy <- function(p, y) {

#Calculate the accuracy of predictions against truth labels.

#Accuracy = # of correct predictions / # of data.

#Args:
#p: predictions of shape (n, 1)
#y: true labels of shape (n, 1)

#Returns:
#accuracy: The ratio of correct predictions to the size of data.

  return(mean((p > 0.5) == (y == 1)))

}

test <- function(X, model) {

#     Make prediction on X using model by doing forward feeding.
#
#     Args:
#         X: The data matrix of shape (n, p)
#         model: tuple of trained parameters (W1, b1, W2, b2).
#
#     Returns:
#         y_predict: The prediction of shape (n, 1).

W1 = model[[1]]
b1 = model[[2]]
W2 = model[[3]]
b2 = model[[4]]

result = feed_forward(X, W1, b1)
Z = result[[1]]
result = feed_forward(Z, W2, b2)
y_predict = result[[1]]

return(y_predict)
}




feed_forward <- function(X, W, b) {

  #Feed forward a logistic regression layer.

  #Args:
  #X: Input data of shape (n, p).
  #n = # of data examples.
  #p = # of predictors.
  #W: Weight matrix in this layer of shape (p, d)
  #p = # of predictors.
  #d = # of nodes in the next layer.
  #b: bias terms of shape (d, )

#Returns:
#y = sigmoid(XW + b.T)  of shape (n, d)

n <- dim(X)[1]
p = sigmoid(X%*%W + repmat(t(b),n,1))
params = list(X, W, b, p)

return(list(p, params))
}


back_prop <- function(dy, params) {

  #Back propagation of a logistic regression layer.

  #Args:
  #dy: Derivatives output of this layer of shape (n, d)
  #params: A tuple of parameters at the current layer.
  #In logistic regression layer, we have (X, W, b, p).

  #Returns:
  #dX: Derivatives to the input for BP to previous layers.
  #Same shape of X: (n, p)

  #dW: Derivatives to weights W at this layer.
  #Same shape of W: (p, d)

  #db: Derivatives to bias b at this layer.
  #Same shape of b: (d,1)

  # Unpack params

  X = params[[1]]
  W = params[[2]]
  b = params[[3]]
  p = params[[4]]


  dW = t(X)%*%(dy*dsigmoid(p))
  db = colSums(dy*dsigmoid(p))
  db = matrix(db,ncol=1)
  dX = (dy*dsigmoid(p))%*%t(W)

  return(list(dX, dW, db))
}


train <- function(X, y, num_hidden=100, num_classes= 1, num_iterations=2000,
          learning_rate=1e-2) {

  n = dim(X)[1]
  p = dim(X)[2]

  y = matrix(y,nrow = n, ncol =1)

# Initialize parameters.
# Weights and bias two layers, random numbers centered at 0.

W1 = 2*randn(p, num_hidden) - 1
b1 = 2*randn(num_hidden,1) - 1
W2 = 2*randn(num_hidden, num_classes) - 1
b2 = 2*randn(num_classes,1) - 1

all_loss = NULL
accuracies = NULL

for (it in 1:num_iterations) {

# Feed forward.

result = feed_forward(X, W1, b1)
Z = result[[1]]
params1 = result[[2]]

result = feed_forward(Z, W2, b2)
y_out = result[[1]]
params2 = result[[2]]

# Calculate the loss.
result = loss_l2(y_out, y)
loss = result[[1]]
dy = result[[2]]

# Back propagation.
result = back_prop(dy, params2)
dZ = result[[1]]
dW2 = result[[2]]
db2 = result[[3]]

result = back_prop(dZ, params1)
dX = result[[1]]
dW1 = result[[2]]
db1 = result[[3]]

# Update parameters along the gradients.
W1 = W1 - (learning_rate * dW1)
b1 <- b1 - (learning_rate*db1)
W2 = W2 - (learning_rate * dW2)
b2 = b2 - (learning_rate * db2)

# Save loss and accuracy for plotting the curves.
all_loss = append(all_loss,loss)

accuracies = append(accuracies,accuracy(y_out, y))

if (it %% 100 == 0){
  print(c(it,loss,accuracies[length(accuracies)]))
   }
}

model = list(W1,b1,W2,b2)

return(list(model,all_loss, accuracies))
}


# load data
load_digits <- function(subset=NULL, normalize=TRUE) {

#Load digits and labels from digits.csv.

#Args:
#subset: A subset of digit from 0 to 9 to return.
#If not specified, all digits will be returned.
#normalize: Whether to normalize data values to between 0 and 1.

#Returns:
#digits: Digits data matrix of the subset specified.
#The shape is (n, p), where
#n is the number of examples,
#p is the dimension of features.
#labels: Labels of the digits in an (n, ) array.
#Each of label[i] is the label for data[i, :]

# load digits.csv, adopted from sklearn.

df <- fread("digits.csv")
df <- as.matrix(df)

## only keep the numbers we want.
if (length(subset)>0) {

  c <- dim(df)[2]
  l_col <- df[,c]
  index = NULL

  for (i in 1:length(subset)){

    number = subset[i]
    index = c(index,which(l_col == number))
  }
  sort(index)
  df = df[index,]
}

# convert to numpy arrays.
digits = df[,-1]
labels = df[,c]

# Normalize digit values to 0 and 1.
if (normalize == TRUE) {
  digits = digits - min(digits)
digits = digits/max(digits)}


# Change the labels to 0 and 1.
for (i in 1:length(subset)) {
  labels[labels == subset[i]] = i-1
}

return(list(digits, labels))

}

split_samples <- function(digits,labels) {

# Split the data into a training set (70%) and a testing set (30%).

num_samples <- dim(digits)[1]
num_training <- round(num_samples*0.7)
indices = sample(1:num_samples, size = num_samples)
training_idx <- indices[1:num_training]
testing_idx <- indices[-(1:num_training)]

return (list(digits[training_idx,], labels[training_idx],
        digits[testing_idx,], labels[testing_idx]))
}


#====================================
# Load digits and labels.
result = load_digits(subset=c(3, 5), normalize=TRUE)
digits = result[[1]]
labels = result[[2]]

result = split_samples(digits,labels)
training_digits = result[[1]]
training_labels = result[[2]]
testing_digits = result[[3]]
testing_labels = result[[4]]

# print dimensions
length(training_digits)
length(testing_digits)

# Train a net and display training accuracy.
final_results = train(training_digits, training_labels)
model = final_results[[1]]

# Evaluate on the testing set.
y_predict = test(testing_digits, model)
testing_accuracy = accuracy(y_predict, testing_labels)
print('Accuracy on testing data:')
testing_accuracy


par(mfrow=c(1,2))
plot(1:length(final_results[[2]]),final_results[[2]],xlab='Iteration',ylab='Loss')
plot(1:length(final_results[[3]]),final_results[[3]],xlab='Iteration',ylab='Accuracy')
