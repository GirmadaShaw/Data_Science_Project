# Vipul Vishek - 1032201185
# Paramveer Singh - 1032202055

# PROJECT 11

# The Iris dataset is a classic dataset in statistics and is included in the datasets package in R. 
#It contains measurements of sepal length, sepal width, petal length, and petal width for 150 iris flowers, 
#representing three species: setosa, versicolor, and virginica.


#1 Loading the Dataset

library( datasets )
library( moments )

data( iris )


# Display first few lines

head( iris )

#Observation: 
#             One could notice that there exists 5 columns in the dataset. 4 columns are numerical, namely, 
#             Sepal.Length , Sepal.Width, Petal.Length , Petal.Width and 1 column is categorical, namely, Species 
#             There are 150 samples in total. 
              
dim( iris )

#2  Data Exploration 

# Summary Statistic for Sepal.Length 
summary( iris$Sepal.Length )
print("Skewness: ")
skewness(iris$Sepal.Length)
print( "Kurtosis" )
kurtosis(iris$Sepal.Length)
print( "Standard Deviation" )
sd( iris$Sepal.Length)

#     Observation : 
#                    The positively skewed feature centers around 5.843 with a spread 
#                    from 5.100 to 6.400 (Q1 to Q3). It has a moderate variability (IQR), 
#                    moderate kurtosis (2.426), and a standard deviation of 0.828.        

# Summary Statistic for Sepal.Width 
summary( iris$Sepal.Width  )
print("Skewness: ")
skewness(iris$Sepal.Width)
print("Skewness: ")
kurtosis(iris$Sepal.Width)
print( "Standard Deviation" )
sd( iris$Sepal.Width)

#     Observation : 
#                   The feature centers around 3.057 with a range from 2.000 to 4.400 (Q1 to Q3). 
#                   It exhibits a positively skewed distribution (skewness = 0.316) with moderate 
#                   kurtosis (3.181). The standard deviation is 0.436, indicating low variability.   


# Summary Statistic for Petal.Length 
summary( iris$Petal.Length)
print("Skewness: ")
skewness(iris$Petal.Length)
print("Skewness: ")
kurtosis(iris$Petal.Length)
print( "Standard Deviation" )
sd( iris$Petal.Length)


#     Observation : 
#   `               The feature has a central tendency around 3.758, with a range from 1.000 to 6.900 (Q1 to Q3). 
#                   It exhibits a negatively skewed distribution (skewness = -0.272) and moderate kurtosis 
#                   (1.604). The standard deviation is 1.765, indicating moderate variability.            `


# Summary Statistic for Petal.Width 
summary( iris$Petal.Width )
print("Skewness: ")
skewness(iris$Petal.Width)
print("Skewness: ")
kurtosis(iris$Petal.Width)
print( "Standard Deviation" )
sd( iris$Petal.Width)

# Observation: 
#                   The feature is centered around 1.199 with a range from 0.100 to 2.500 (Q1 to Q3). 
#                   It shows a slightly negatively skewed distribution (skewness = -0.102) and moderate 
#                   kurtosis (1.664). The standard deviation is 0.762, indicating moderate variability.

# Summary Statistic for Species
summary( iris$Species )

# Observation: 
#           It has three categories of flowers Setosa, Versicolor and Virginica and each are 50 in no.

#  Visualizing Sepal.Length

library( ggplot2 )

ggplot( iris , aes( x = iris$Sepal.Length)) + geom_histogram( bins = 3)
ggplot( iris , aes( x = iris$Sepal.Width)) + geom_histogram( bins = 3)
ggplot( iris , aes( x = iris$Petal.Length)) + geom_histogram( bins = 3)
ggplot( iris , aes( x = iris$Petal.Width)) + geom_histogram( bins = 3)

ggplot( iris , aes( x = iris$Sepal.Length)) + geom_density( )
ggplot( iris , aes( x = iris$Sepal.Width)) + geom_density( )
ggplot( iris , aes( x = iris$Petal.Length)) + geom_density( )
ggplot( iris , aes( x = iris$Petal.Width)) + geom_density( )

ggplot( iris , aes( x = iris$Sepal.Length)) + geom_boxplot( )
ggplot( iris , aes( x = iris$Sepal.Width)) + geom_boxplot( )
ggplot( iris , aes( x = iris$Petal.Length)) + geom_boxplot( )
ggplot( iris , aes( x = iris$Petal.Width)) + geom_boxplot( )


#3 Species Comparison

library( dplyr )

iris %>% select( Sepal.Length , Sepal.Width , Petal.Length , Petal.Width , Species ) %>% group_by( Species ) %>% summarise( Sepal.Length = mean(Sepal.Length) ,
                                                                                                                            Sepal.Width = mean(Sepal.Width),
                                                                                                                            Petal.Length = mean(Petal.Length),
                                                                                                                            Petal.Width = mean(Petal.Width) )
# Side by Side 

pairs(iris[, 1:4], col = iris$Species, pch = 19)

# Observation:

#               The graphs says it all. Sepal Length and Petal Length are highly correlated to Sepal Width and Petal Width respectively.



# Correlation Analysis 

iris_for_cor  = iris %>% select( Sepal.Length , Sepal.Width , Petal.Length , Petal.Width )
print( "Correlation Matrix: ")
cor_matrix = cor( iris_for_cor)
cor_matrix


# Create a heat-map for correlation

heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Correlation Heatmap",
        margins = c(5, 5))

#   Observation: 
#                The intensity of colors in the heat-map indicates the strength and direction of 
#                the correlation. Positive correlations are often represented by lighter shades, 
#                while negative correlations are represented by darker shades.

#               Patterns in the heat-map can reveal clusters of variables that are highly correlated 
#               with each other. This can be useful for identifying groups of variables that tend to 
#               move together or are inversely related

# Linear Regression for Sepal.Length and Sepal.Width

iris_for_regression = iris %>% select( Sepal.Length , Sepal.Width)
set.seed(42)        # To get the same values everytime I run the code.

idx = sample( nrow(iris_for_regression) , 0.75*nrow(iris_for_regression) )

X_train = iris_for_regression[idx,]
X_test = iris_for_regression[-idx,]


X_train_shape = dim( X_train )
X_train_shape                               # see the dimensions of the Training Data
X_test_shape = dim( X_test)
X_test_shape                                # see the dimensions of the Testing Data
  

# Training the Model

linear_model = lm( formula = iris$Sepal.Length ~ iris$Sepal.Width , data = X_train )    
summary( linear_model )

#Predicting the values

y_pred = predict( linear_model , data = X_test$Sepal.Length )

rmse_score = sqrt( mean( (X_test$Sepal.Width - y_pred)^2 )  )
rmse_score

regress_line = (-0.22333611)*X_test + 6.5262226


print( "Coefficients are:")
print( linear_model$coefficients)

plot( x = iris_for_regression$Sepal.Length , 
      y = iris_for_regression$Sepal.Width,
      xlab = "Sepal Length",
      ylab = "Sepal Width"
)

# abline( x = iris_for_regression$Sepal.Length , y = iris_for_regression$Sepal.Width )
abline( lm( iris_for_regression$Sepal.Length ~ iris_for_regression$Sepal.Width , 
            data = iris_for_regression ) , col = 'red')


#5. Scatter Plots 


#a. Sepal.Length v/s Sepal.Width 


plot( x = iris_for_regression$Sepal.Length , 
      y = iris_for_regression$Sepal.Width,
      xlab = "Sepal Length",
      ylab = "Sepal Width"
)
cor_coef1 = cor( iris$Sepal.Length , iris$Sepal.Width)
text(x = 6 , y = 4, labels = paste("Correlation =", round(cor_coef1 , 2)), pos = 4, col = "red")


#b. Sepal.Length v/s Petal.length

plot( x = iris_for_regression$Sepal.Length , 
      y = iris_for_regression$Petal.Length,
      xlab = "Sepal Length",
      ylab = "Petal Length"
)
cor_coef2 = cor( iris$Sepal.Length , iris$Petal.Length)
text(x = 15, y = 7.5, labels = paste("Correlation =", round(cor_coef2 , 2)), pos = 4, col = "red")

#c. Sepal.Length v/s Petal.Width

plot( x = iris_for_regression$Sepal.Length , 
      y = iris_for_regression$Petal.Width,
      xlab = "Sepal Length",
      ylab = "Petal Width"
)
cor_coef3 = cor( iris$Sepal.Length , iris$Petal.Width )
text(x = 15, y = 7.5, labels = paste("Correlation =", round(cor_coef3, 2)), pos = 4, col = "red")


#d. Petal.Length v/s Petal.Width


plot(x = iris$Petal.Length,
     y = iris$Petal.Width,
     xlab = "Petal Length",
     ylab = "Petal Width")
cor_coef4 = cor( iris$Petal.Length , iris$Petal.Width )
text(x = 1, y = 2, labels = paste("Correlation =", round(cor_coef4 , 2)), pos = 4, col = "red")



#e. Sepal.Width v/s Petal.Length

plot( x = iris_for_regression$Sepal.Width , 
      y = iris_for_regression$Petal.Length,
      xlab = "Sepal Width",
      ylab = "Petal Legth "
      
)
cor_coef5 = cor( iris$Sepal.Width , iris$Petal.Length )
text(x = 1, y = 2, labels = paste("Correlation =", round(cor_coef5 , 2)), pos = 4, col = "red")



#f. Sepal.Width v/s Petal.Width

plot( x = iris_for_regression$Sepal.Width , 
      y = iris_for_regression$Petal.Width,
      xlab = "Sepal Width",
      ylab = "Petal Width"
)
cor_coef5 = cor( iris$Sepal.Width , iris$Petal.Width )
text(x = 1, y = 2, labels = paste("Correlation =", round(cor_coef5 , 2)), pos = 4, col = "red")







