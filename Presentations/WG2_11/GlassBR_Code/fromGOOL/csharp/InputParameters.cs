using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class InputParameters {
        public double a;
        public double b;
        public double t;
        public int gt;
        public double w;
        public double tnt;
        public double sdx;
        public double sdy;
        public double sdz;
        public double pbtol;
        public double asprat;
        public double sd;
        public double h;
        public double gtf;
        public double ldf;
        public double wtnt;
        public double E;
        public double td;
        public double m;
        public double k;
        public double lsf;
        
        public InputParameters() {
            a = 0.0;
            b = 0.0;
            t = 2.5;
            gt = 1;
            w = 0.0;
            tnt = 0.0;
            sdx = 0.0;
            sdy = 0.0;
            sdz = 0.0;
            pbtol = 0.0;
            asprat = 0.0;
            sd = 0.0;
            h = 0.0;
            gtf = 0.0;
            ldf = 0.0;
            wtnt = 0.0;
            E = 7.17 * (Math.Pow(10.0, 7.0));
            td = 3.0;
            m = 7.0;
            k = 2.86 * (Math.Pow(10.0, -53.0));
            lsf = 1.0;
        }
    }
}

