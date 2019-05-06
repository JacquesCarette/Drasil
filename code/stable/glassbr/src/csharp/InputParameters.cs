using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

namespace GlassBR {
    public class InputParameters {
        public double a;
        public double b;
        public double SD;
        public double w;
        public double AR;
        public double P_btol;
        public double TNT;
        public string g;
        public double t;
        public double SD_x;
        public double SD_y;
        public double SD_z;
        public double h;
        public double LDF;
        public double GTF;
        public double w_TNT;
        
        public InputParameters() {
            a = 0.0;
            b = 0.0;
            SD = 0.0;
            w = 0.0;
            AR = 0.0;
            P_btol = 0.0;
            TNT = 0.0;
            g = "";
            t = 0.0;
            SD_x = 0.0;
            SD_y = 0.0;
            SD_z = 0.0;
            h = 0.0;
            LDF = 0.0;
            GTF = 0.0;
            w_TNT = 0.0;
        }
    }
}

