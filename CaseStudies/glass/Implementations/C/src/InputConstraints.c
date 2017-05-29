#include "InputConstraints.h"

#include "InputParameters.h"

#include <stdlib.h>
#include <stdio.h>

void check_constraints(InputParameters* params) {
    if (params->a <= 0 || params->b <= 0) {
        printf("InputError: a and b must be greater than 0");
        exit(EXIT_FAILURE);
    }
    if (params->asprat < 1 || params->asprat > 5) {
        printf("InputError: a/b must be between 1 and 5");
        exit(EXIT_FAILURE);
    }
    if (!(  (params->t == 2.50) 
         || (params->t == 2.70) 
         || (params->t == 3.00) 
         || (params->t == 4.00) 
         || (params->t == 5.00) 
         || (params->t == 6.00)
         || (params->t == 8.00) 
         || (params->t == 10.00) 
         || (params->t == 12.00) 
         || (params->t == 16.00) 
         || (params->t == 19.00) 
         || (params->t == 22.00) ) ) {
        printf("InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]");
        exit(EXIT_FAILURE);
    }
    if (params->tnt <= 0) {
        printf("InputError: TNT must be greater than 0");
        exit(EXIT_FAILURE);
    }
    if (params->wtnt < 4.5 || params->wtnt > 910) {
        printf("InputError: wtnt must be between 4.5 and 910");
        exit(EXIT_FAILURE);
    }
    if (params->sd < 6 || params-> sd > 130) {
        printf("InputError: SD must be between 6 and 130");
        exit(EXIT_FAILURE);
    }
}