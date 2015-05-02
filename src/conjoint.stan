data {
    int N; 
    int K; 
    vector[K] ideo[N];
    vector[K] pvoto[N];
}
    
parameters {
    real free_alpha0[K-1];
    real alpha;
}


model {
    real alpha0[K];
    vector[K] u[N];

    alpha ~ uniform(-1E3, 0);

    for (k in 1:(K-1)) {
        free_alpha0[k] ~ normal(0, 1E3);
        alpha0[k] <- free_alpha0[k];            
    }
    
    alpha0[K] <- 0;

    for (i in 1:N) {
        for (j in 1:K) {
            u[i, j] <- alpha0[j] + alpha*ideo[i, j];
        }

        increment_log_prob(log(softmax(u[i])') * pvoto[i]);
    }
}

## generated quantities {
##     real alpha0[K];
##     vector[K] u[N];
##     vector[K] p[N];

##     for (k in 1:(K-1)) {
##         alpha0[k] <- free_alpha0[k];            
##     }
    
##     alpha0[K] <- 0;

##     for (i in 1:N) {
##         for (j in 1:K) {
##             u[i, j] <- alpha0[j] + alpha*ideo[i, j];
##         }
##         p[i] <- softmax(u[i]);
##     }
## }
