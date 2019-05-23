#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(S);
  DATA_VECTOR(R);
  DATA_VECTOR(weights);

  PARAMETER(log_a);
  PARAMETER(log_b);
  PARAMETER(log_sd);

  Type sd=exp(log_sd);
  ADREPORT(sd);

  //CALCULATE NEGATIVE LOG LIKELIHOOD
  Type nll=0;
  for(int i=0; i<R.size(); i++)
  {
    nll -= weights(i)*dnorm(log(R(i)), log(exp(log_a)*S(i)/(1+exp(log_b)*S(i))), sd, true);
  }

  return nll;
}
