Gamma2.f<-function(x, pr=0.95) 
{ 
    # x is a matrix of counts.  You can use output of crosstabs or xtabs in R. 
    # A matrix of counts can be formed from a data frame by using design.table. 
     
    # Confidence interval calculation and output from Greg Rodd 
     
    # Check for using S-PLUS and output is from crosstabs (needs >= S-PLUS 6.0) 
    if(is.null(version$language) && inherits(x, "crosstabs")) { oldClass(x)<-NULL; 
attr(x, "marginals")<-NULL} 
         
    n <- nrow(x) 
    m <- ncol(x) 
    pi.c<-pi.d<-matrix(0,nr=n,nc=m) 
         
    row.x<-row(x) 
    col.x<-col(x) 
     
    for(i in 1:(n)){ 
        for(j in 1:(m)){ 
            pi.c[i, j]<-sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j]) 
            pi.d[i, j]<-sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j]) 
        } 
    } 
 
    C <- sum(pi.c*x)/2 
    D <- sum(pi.d*x)/2 

    psi<-2*(D*pi.c-C*pi.d)/(C+D)^2 
    sigma2<-sum(x*psi^2)-sum(x*psi)^2 
     
    gamma <- (C - D)/(C + D) 
    pr2 <- 1 - (1 - pr)/2 
    CIa <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma 
 
    list(gamma = gamma, C = C, D = D, sigma = sqrt(sigma2), Level = paste( 
        100 * pr, "%", sep = ""), CI = paste(c("[", max(CIa[1], -1),  
        ", ", min(CIa[2], 1), "]"), collapse = ""))      
} 
