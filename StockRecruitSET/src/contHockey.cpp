#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(S);
  DATA_VECTOR(R);
  DATA_VECTOR(weights);
  DATA_SCALAR(g);

  PARAMETER(log_beta);
  PARAMETER(log_delta);
  PARAMETER(log_sd);

  Type sd=exp(log_sd);
  Type delta=exp(log_delta);
  ADREPORT(sd);

  //CALCULATE NEGATIVE LOG LIKELIHOOD
  Type nll=0;
  for(int i=0; i<R.size(); i++)
  {
    nll -= weights(i)*
          dnorm(log(R(i)), log_beta + log(S(i) + sqrt(delta*delta + (g*g)/Type(4)) - sqrt((S(i)-delta)*(S(i)-delta) + (g*g)/Type(4))), sd, true);
  }

  return nll;
}
