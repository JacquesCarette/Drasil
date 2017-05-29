#include "DerivedValues.h"
#include "Constants.h"

#include <math.h>

void derived_params(InputParameters* params){
  params->asprat = params->a / params->b;
  params->sd = sqrt(pow(params->sdx, 2.0) + pow(params->sdy, 2.0) + pow(params->sdz, 2.0));
  params->ldf = pow((TD / 60.0), (M / 16.0)); 
  params->wtnt = params->w * params->tnt;
  
  if(params->t == 2.50) params->h = 2.16;
  else if(params->t == 2.70) params->h = 2.59;
  else if(params->t == 3.00) params->h = 2.92;
  else if(params->t == 4.00) params->h = 3.78;
  else if(params->t == 5.00) params->h = 4.57;
  else if(params->t == 6.00) params->h = 5.56;
  else if(params->t == 8.00) params->h = 7.42;
  else if(params->t == 10.00) params->h = 9.02;
  else if(params->t == 12.00) params->h = 11.91;
  else if(params->t == 16.00) params->h = 15.09;
  else if(params->t == 19.00) params->h = 18.26;
  else if(params->t == 22.00) params->h = 21.44;
  
  if(params->gt == 1) params->gtf = 1;
  else if(params->gt == 2) params->gtf = 2;
  else if(params->gt == 3) params->gtf = 4;
}