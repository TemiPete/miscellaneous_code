

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
qq_generic <- function(data_points, distribution=c('normal', 'chisq', 'uniform', 't'), neg_log10_values=F, mean=0, sd=1, df=1, min=0, max=1){
    
    n <- length(data_points)
    
    if(length(distribution) > 1){
        stop('Input one distribution.')
    }
    
    theoretical_quantiles <- switch(as.character(distribution), 
                                    'normal' = qnorm(ppoints(n), mean = mean, sd = sd)[order(order(data_points))],
                                    'chisq' = qchisq(ppoints(n), df=df)[order(order(data_points))],
                                    'uniform' = qunif(ppoints(n), min=min, max=max)[order(order(data_points))],
                                    't' = qt(ppoints(n), df=df)[order(order(data_points))],
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
    
    plot(theoretical_quantiles, data_points, xlab = xlab_use, ylab = ylab_use)
    abline(a = 0, b = 1, col = "red")
    
}

# qq_generic(result$observed_chistats, distribution = 'normal')
