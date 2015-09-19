eccentricity <- function(x, method = 'michalsky')
{
    x <- as.IDate(x)
    ## Day of Year
    dn <- yday(x)
    X <- 2 * pi * (dn-1)/365

    switch(method,
           cooper = 1 + 0.033*cos(2*pi*dn/365),
           spencer = , 
           michalsky = , 
           strous = 1.000110 + 0.034221*cos(X) +
               0.001280*sin(X) + 0.000719*cos(2*X) +
                   0.000077*sin(2*X),
           stop('Unknown method')
           )
}
