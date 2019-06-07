using System;
using System.IO;
using System.Collections;
using System.Collections.Generic;

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
        this.a = 0.0;
        this.b = 0.0;
        this.SD = 0.0;
        this.w = 0.0;
        this.AR = 0.0;
        this.P_btol = 0.0;
        this.TNT = 0.0;
        this.g = "";
        this.t = 0.0;
        this.SD_x = 0.0;
        this.SD_y = 0.0;
        this.SD_z = 0.0;
        this.h = 0.0;
        this.LDF = 0.0;
        this.GTF = 0.0;
        this.w_TNT = 0.0;
    }
}

