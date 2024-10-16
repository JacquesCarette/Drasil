/** \file InputParameters.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the structure for holding input values, the function for reading inputs, the function for calculating derived values, and the function for checking the physical constraints and software constraints on the input
    \note Generated by Drasil v0.1-alpha
*/

using System;
using System.Collections.Generic;
using System.IO;

/** \brief Structure for holding the input values and derived values
*/
public class InputParameters {
    public double a;
    public double b;
    public double w;
    public double P_btol;
    public double TNT;
    public string g;
    public double t;
    public double SD_x;
    public double SD_y;
    public double SD_z;
    public double h;
    public double LDF;
    public int GTF;
    public double SD;
    public double AR;
    public double w_TNT;
    
    /** \brief Initializes input object by reading inputs, calculating derived values, and checking physical constraints and software constraints on the input
        \param filename name of the input file
    */
    public InputParameters(string filename) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function InputParameters called with inputs: {");
        outfile.Write("  filename = ");
        outfile.WriteLine(filename);
        outfile.WriteLine("  }");
        outfile.Close();
        
        this.get_input(filename);
        this.derived_values();
        this.input_constraints();
    }
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
    */
    private void get_input(string filename) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function get_input called with inputs: {");
        outfile.Write("  filename = ");
        outfile.WriteLine(filename);
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        this.a = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.a' assigned ");
        outfile.Write(this.a);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.b = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.b' assigned ");
        outfile.Write(this.b);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.w = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.w' assigned ");
        outfile.Write(this.w);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.P_btol = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.P_btol' assigned ");
        outfile.Write(this.P_btol);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.TNT = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.TNT' assigned ");
        outfile.Write(this.TNT);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.g = infile.ReadLine();
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.g' assigned ");
        outfile.Write(this.g);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.t = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.t' assigned ");
        outfile.Write(this.t);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.SD_x = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.SD_x' assigned ");
        outfile.Write(this.SD_x);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.SD_y = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.SD_y' assigned ");
        outfile.Write(this.SD_y);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.ReadLine();
        this.SD_z = Double.Parse(infile.ReadLine());
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.SD_z' assigned ");
        outfile.Write(this.SD_z);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        infile.Close();
    }
    
    /** \brief Calculates values that can be immediately derived from the inputs
    */
    private void derived_values() {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function derived_values called with inputs: {");
        outfile.WriteLine("  }");
        outfile.Close();
        
        this.h = 1.0 / 1000.0 * (this.t == 2.5 ? 2.16 : this.t == 2.7 ? 2.59 : this.t == 3.0 ? 2.92 : this.t == 4.0 ? 3.78 : this.t == 5.0 ? 4.57 : this.t == 6.0 ? 5.56 : this.t == 8.0 ? 7.42 : this.t == 10.0 ? 9.02 : this.t == 12.0 ? 11.91 : this.t == 16.0 ? 15.09 : this.t == 19.0 ? 18.26 : 21.44);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.h' assigned ");
        outfile.Write(this.h);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        
        this.LDF = Math.Pow(3.0 / 60.0, 7.0 / 16.0);
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.LDF' assigned ");
        outfile.Write(this.LDF);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        
        if (this.g == "AN") {
            this.GTF = 1;
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'this.GTF' assigned ");
            outfile.Write(this.GTF);
            outfile.WriteLine(" in module InputParameters");
            outfile.Close();
        }
        else if (this.g == "FT") {
            this.GTF = 4;
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'this.GTF' assigned ");
            outfile.Write(this.GTF);
            outfile.WriteLine(" in module InputParameters");
            outfile.Close();
        }
        else if (this.g == "HS") {
            this.GTF = 2;
            outfile = new StreamWriter("log.txt", true);
            outfile.Write("var 'this.GTF' assigned ");
            outfile.Write(this.GTF);
            outfile.WriteLine(" in module InputParameters");
            outfile.Close();
        }
        else {
            throw new Exception("Undefined case encountered in function GTF");
        }
        
        this.SD = Math.Sqrt(Math.Pow(this.SD_x, 2.0) + Math.Pow(this.SD_y, 2.0) + Math.Pow(this.SD_z, 2.0));
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.SD' assigned ");
        outfile.Write(this.SD);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        
        this.AR = this.a / this.b;
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.AR' assigned ");
        outfile.Write(this.AR);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
        
        this.w_TNT = this.w * this.TNT;
        outfile = new StreamWriter("log.txt", true);
        outfile.Write("var 'this.w_TNT' assigned ");
        outfile.Write(this.w_TNT);
        outfile.WriteLine(" in module InputParameters");
        outfile.Close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints and software constraints
    */
    private void input_constraints() {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function input_constraints called with inputs: {");
        outfile.WriteLine("  }");
        outfile.Close();
        
        if (!(0.1 <= this.a && this.a <= 5.0)) {
            Console.Write("a has value ");
            Console.Write(this.a);
            Console.Write(", but is expected to be ");
            Console.Write("between ");
            Console.Write(0.1);
            Console.Write(" (d_min)");
            Console.Write(" and ");
            Console.Write(5.0);
            Console.Write(" (d_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(0.1 <= this.b && this.b <= 5.0)) {
            Console.Write("b has value ");
            Console.Write(this.b);
            Console.Write(", but is expected to be ");
            Console.Write("between ");
            Console.Write(0.1);
            Console.Write(" (d_min)");
            Console.Write(" and ");
            Console.Write(5.0);
            Console.Write(" (d_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(4.5 <= this.w && this.w <= 910.0)) {
            Console.Write("w has value ");
            Console.Write(this.w);
            Console.Write(", but is expected to be ");
            Console.Write("between ");
            Console.Write(4.5);
            Console.Write(" (w_min)");
            Console.Write(" and ");
            Console.Write(910.0);
            Console.Write(" (w_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        HashSet<string> set_g = new HashSet<string> {"AN", "FT", "HS"};
        if (!(set_g.Contains(this.g))) {
            Console.Write("g has value ");
            Console.Write(this.g);
            Console.Write(", but is expected to be ");
            Console.Write("an element of the set ");
            Console.Write("{ ");
            foreach (string set_i1 in set_g) {
                Console.Write(set_i1);
                Console.Write(" ");
            }
            Console.Write("}");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        HashSet<double> set_t = new HashSet<double> {2.5, 2.7, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 19.0, 22.0};
        if (!(set_t.Contains(this.t))) {
            Console.Write("t has value ");
            Console.Write(this.t);
            Console.Write(", but is expected to be ");
            Console.Write("an element of the set ");
            Console.Write("{ ");
            foreach (double set_i1 in set_t) {
                Console.Write(set_i1);
                Console.Write(" ");
            }
            Console.Write("}");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(6.0 <= this.SD && this.SD <= 130.0)) {
            Console.Write("SD has value ");
            Console.Write(this.SD);
            Console.Write(", but is expected to be ");
            Console.Write("between ");
            Console.Write(6.0);
            Console.Write(" (SD_min)");
            Console.Write(" and ");
            Console.Write(130.0);
            Console.Write(" (SD_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(this.AR <= 5.0)) {
            Console.Write("AR has value ");
            Console.Write(this.AR);
            Console.Write(", but is expected to be ");
            Console.Write("below ");
            Console.Write(5.0);
            Console.Write(" (AR_max)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        
        if (!(this.a > 0.0)) {
            Console.Write("a has value ");
            Console.Write(this.a);
            Console.Write(", but is expected to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(this.a >= this.b)) {
            Console.Write("a has value ");
            Console.Write(this.a);
            Console.Write(", but is expected to be ");
            Console.Write("above ");
            Console.Write(this.b);
            Console.Write(" (b)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(0.0 < this.b && this.b <= this.a)) {
            Console.Write("b has value ");
            Console.Write(this.b);
            Console.Write(", but is expected to be ");
            Console.Write("between ");
            Console.Write(0.0);
            Console.Write(" and ");
            Console.Write(this.a);
            Console.Write(" (a)");
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(this.w > 0.0)) {
            Console.Write("w has value ");
            Console.Write(this.w);
            Console.Write(", but is expected to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(0.0 <= this.P_btol && this.P_btol <= 1.0)) {
            Console.Write("P_btol has value ");
            Console.Write(this.P_btol);
            Console.Write(", but is expected to be ");
            Console.Write("between ");
            Console.Write(0.0);
            Console.Write(" and ");
            Console.Write(1.0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(this.TNT > 0.0)) {
            Console.Write("TNT has value ");
            Console.Write(this.TNT);
            Console.Write(", but is expected to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(this.SD > 0.0)) {
            Console.Write("SD has value ");
            Console.Write(this.SD);
            Console.Write(", but is expected to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
        if (!(this.AR >= 1.0)) {
            Console.Write("AR has value ");
            Console.Write(this.AR);
            Console.Write(", but is expected to be ");
            Console.Write("above ");
            Console.Write(1.0);
            Console.WriteLine(".");
            throw new Exception("InputError");
        }
    }
}
