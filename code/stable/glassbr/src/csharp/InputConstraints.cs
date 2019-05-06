using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class InputConstraints {
        
        public static void input_constraints(InputParameters inParams) {
            if (!((0.1 <= inParams.a) && (inParams.a <= 5.0))) {
                throw new System.ApplicationException("InputError");
            }
            if (!((0.1 <= inParams.b) && (inParams.b <= 5.0))) {
                throw new System.ApplicationException("InputError");
            }
            if (!((6.0 <= inParams.SD) && (inParams.SD <= 130.0))) {
                throw new System.ApplicationException("InputError");
            }
            if (!((4.5 <= inParams.w) && (inParams.w <= 910.0))) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.AR <= 5.0)) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.a > 0)) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.a >= inParams.b)) {
                throw new System.ApplicationException("InputError");
            }
            if (!((0 < inParams.b) && (inParams.b <= inParams.a))) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.SD > 0)) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.w > 0)) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.AR >= 1)) {
                throw new System.ApplicationException("InputError");
            }
            if (!((0 < inParams.P_btol) && (inParams.P_btol < 1))) {
                throw new System.ApplicationException("InputError");
            }
            if (!(inParams.TNT > 0)) {
                throw new System.ApplicationException("InputError");
            }
        }
    }
}

