#include "InputParameters.h"

#include <stdlib.h>

InputParameters* new_InputParameters(){
  InputParameters* params = (InputParameters*)malloc(sizeof(InputParameters));
  params->a = 0.0;
  params->b = 0.0;
  params->t = 2.5;
  params->gt = 1;
  params->w = 0.0;
  params->tnt = 0.0;
  params->sdx = 0.0;
  params->sdy = 0.0;
  params->sdz = 0.0;
  params->pbtol = 0.0;
  params->asprat = 0.0;
  params->sd = 0.0;
  params->h = 0.0;
  params->gtf = 0.0;
  params->ldf = 0.0;
  params->wtnt = 0.0;
  return params;
}
      
void delete_InputParameters(InputParameters* params){
  free(params);
}