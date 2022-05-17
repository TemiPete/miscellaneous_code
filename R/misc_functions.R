

# ===============
# Usage: Miscellaneous R functions I use
# Author: Temi
# Date: Tuesday Jan 26 2022
# 
# 
#

#' Create a generic qqplot
#' @param data_points A vector of data points
#' @param distribution A distribution to compare with. Please input just one value
#' @param neg_log1_values Should the plot use the -log10 values? Defaults to F
#' @param mean
#' @param sd
#' @param df
#' @param min
#' @param max
#' @return a qqplot object
qq_generic <- function(data_points, distribution=c('normal', 'chisq', 'uniform', 't', 'poisson', 'neg_binom', 'gamma'), 
                       neg_log10_values=F, mean=0, sd=1, df=1, min=0, max=1, lambda=0.5, shape=20, rate=1, params_to_plot=NULL, params_to_abline=NULL){
    
    n <- length(data_points)
    
    if(length(distribution) > 1){
        stop('Input one distribution.')
    }
    
    theoretical_quantiles <- switch(as.character(distribution), 
                                    'normal' = qnorm(ppoints(n), mean = mean, sd = sd)[order(order(data_points))],
                                    'chisq' = qchisq(ppoints(n), df=df)[order(order(data_points))],
                                    'uniform' = qunif(ppoints(n), min=min, max=max)[order(order(data_points))],
                                    't' = qt(ppoints(n), df=df)[order(order(data_points))],
                                    'poisson' = qpois(ppoints(n), lambda = lambda)[order(order(data_points))],
                                    'gamma' = qgamma(ppoints(n), shape = shape)[order(order(data_points))],
                                    #'neg_binom' = qnbinom(ppoints(n), )
                                    stop('The distribution is invalid. Input a valid distribution.')
    )
    
    xlab_use <- 'Theoretical Quantiles'
    ylab_use <- 'Sample Quantiles'
    
    if(neg_log10_values == T){
        theoretical_quantiles <- -log10(theoretical_quantiles)
        data_points <- -log10(data_points) 
        xlab_use <- '-log10(Theoretical quantiles)'
        ylab_use <- '-log10(Sample quantiles)'
    }
    
    do.call(base::plot, c(list(x=theoretical_quantiles, y=data_points, xlab = xlab_use, ylab = ylab_use), params_to_plot))
    #plot(theoretical_quantiles, data_points, xlab = xlab_use, ylab = ylab_use, ...)
    do.call(graphics::abline, c(list(a = 0, b = 1, col = "red"), params_to_abline))
    
}

# qq_generic(result$observed_chistats, distribution = 'normal')


#' A function to generate N positive numbers between two numbers that less than that sum up to M
#' @param N N-positive integers
#' @param M What N should sum to
#' @param min
#' @param max
#' @returns A vector of length N that sum up to M
rand_vect_cont <- function(N, M, min=0, max=1) {
    vec <- runif(N, min = min, max = max)
    vec / sum(vec) * M
}

#' Compute a matrix to the power of n
#' Adapted from: https://stackoverflow.com/questions/3274818/matrix-power-in-r
#' @param x a matrix
#' @param n nth power
#' @returns
matrix_power <- function(x, n){
    if (n == 0) {
        I <- diag(length(diag(x)))
        return(I)        
    }
    base::Reduce(`%*%`, replicate(n, x, simplify = FALSE))
}

#' Function to bind (cbind or rbind) vectors of different lengths into a dataframe
#' 

