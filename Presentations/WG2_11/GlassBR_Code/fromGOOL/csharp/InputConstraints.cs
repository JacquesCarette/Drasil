using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class InputConstraints {
        
        public static void check_constraints(InputParameters inparams) {
            if (inparams.a <= 0.0) {
                throw new System.ApplicationException("InputError: a must be greater than 0");
            }
            if (inparams.b <= 0.0) {
                throw new System.ApplicationException("InputError: b must be greater than 0");
            }
            if (inparams.asprat < 1.0) {
                throw new System.ApplicationException("InputError: a/b cannot be less than 1.0");
            }
            if (inparams.asprat > 5.0) {
                throw new System.ApplicationException("InputError: a/b cannot be greater than 5.0");
            }
            if (!((((((((((((inparams.t == 2.5) || (inparams.t == 2.7)) || (inparams.t == 3.0)) || (inparams.t == 4.0)) || (inparams.t == 5.0)) || (inparams.t == 6.0)) || (inparams.t == 8.0)) || (inparams.t == 10.0)) || (inparams.t == 12.0)) || (inparams.t == 16.0)) || (inparams.t == 19.0)) || (inparams.t == 22.0))) {
                throw new System.ApplicationException("InputError: t must be in [2.5,2.7,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,19.0,22.0]");
            }
            if (inparams.tnt <= 0.0) {
                throw new System.ApplicationException("InputError: tnt must be greater than 0");
            }
            if (inparams.wtnt < 4.5) {
                throw new System.ApplicationException("InputError: wtnt cannot be less than 4.5");
            }
            if (inparams.wtnt > 910.0) {
                throw new System.ApplicationException("InputError: wtnt cannot be greater than 910.0");
            }
            if (inparams.sd < 6.0) {
                throw new System.ApplicationException("InputError: sd cannot be less than 6.0");
            }
            if (inparams.sd > 130.0) {
                throw new System.ApplicationException("InputError: sd cannot be greater than 130.0");
            }
        }
    }
}

