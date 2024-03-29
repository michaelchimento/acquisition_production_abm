data {
    int K;              // num behaviors
    int N;              // num observations in dataset
    int J;              // num individuals
    int tech[N];        // techique chosen
    real y[N,K];    // observed personal yields of techs (1/0)
    real s[N,K];        // observed number of times observing behaviors
    int bout[N];        // processing bout per individual
    int id[N];          // individual id
    int N_effects;      // number of learning parameters to estimate
}

parameters {
    vector[N_effects] mu;                     //population means
}


model {
    vector[K] AC;       // attraction scores
    real logPrA;        // asocial learning Pr
    real PrS;           // social learning Pr
    vector[K] s_temp;   // social learning temp
    real rho;           // sensitivity to recent experience
    real sigma;         // social weight vs personal
    //real fc;            // conformity exponent
    //real tau;        // sensitivity to attraction scores


   //priors
    mu[1] ~ normal(0,1); //rho
    mu[2] ~ normal(0,1); //sigma
    //mu[3] ~ normal(0,0.5); //fc
    //mu[4] ~ normal(0,0.5); //tau

    //liklihood loop
    for ( i in 1:N ) {
        //update attractions
        for ( j in 1:K ) {
            if ( bout[i] > 1 ) {
                AC[j] = (1-rho)*AC[j] + rho*y[i-1,j];
            } else {
                AC[j] = 0;
            }
        }//j

        if ( bout[i]==1 ) {
            // calculate new individual's parameter values
            rho= inv_logit(mu[1]);
            sigma = inv_logit(mu[2]);
            //fc = exp(mu[3]);
            //tau = exp(mu[4]);
        }

        logPrA = AC[tech[i]] - log_sum_exp( AC );

        //conformity aspect below
        if ( bout[i] > 1 ) {
            if (sum( s[i] ) > 0 ) {
                // compute frequency cue
                for ( j in 1:K ) s_temp[j] = pow(s[i,j],1);
                PrS = s_temp[tech[i]]/sum(s_temp);
                target += log( (1-sigma)*exp(logPrA) + sigma*PrS ) ;
            } else {
                target += logPrA;
            }
        } else {
            target += logPrA;
         }
     }//i

}//end of model


