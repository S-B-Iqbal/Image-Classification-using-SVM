##### Optical Character recognition using SVM #####

#### Exploring and preparing data

## frey and Slate state that when the glyphs are scanned, 
## they are converted into pixel with 16 different statistical attributes

letters <- read.csv("letterdata.csv")

str(letters)


# Dividing data into training and test set

letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

#### Training a model

install.packages("kernlab")
require(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

## kernel specifies a non-linear maapping such as :
## "rbfdot" = radial basis
## "polydot" = polynomial
## "tanhdot" = hyperbolic tangent sigmoid
## "vanilladot" = linear

letter_classifier


#### Evaluating model performance

letter_predict <- predict(letter_classifier, letters_test)
head(letter_predict)

table(letter_predict, letters_test$letter)

# checking the number of letters correctly predicted

match = letter_predict==letters_test$letter

table(match)

prop.table(table(match))

#### Improving model performance

# Checking the Gaussian RBF kernel performance

letter_rbf = ksvm(letter~., data = letters_train, kernel = "rbfdot", C = 100)

letter_rbf_predict = predict(letter_rbf, letters_test)

match = letter_rbf_predict == letters_test$letter

prop.table(table(match))

