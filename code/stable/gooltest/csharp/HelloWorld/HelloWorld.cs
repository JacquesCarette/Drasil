/** \file HelloWorld.cs
    \author Brooks MacLachlan
    \brief Tests various GOOL functions. It should run without errors.
*/
using System;
using System.Collections.Generic;

public class HelloWorld {
    
    public static void Main(string[] args) {
        // Initializing variables
        int a;
        int b = 5;
        List<double> myOtherList = new List<double> {1.0, 1.5};
        int oneIndex = myOtherList.IndexOf(1.0);
        Console.WriteLine(oneIndex);
        a = myOtherList.Count;
        myOtherList.Insert(2, 2.0);
        myOtherList.Add(2.5);
        double e;
        e = myOtherList[1];
        myOtherList[1] = 17.4;
        List<string> myName = new List<string>(7);
        myName = new List<string>("Brooks Mac".Split(' '));
        Console.Write("[");
        for (int list_i1 = 0; list_i1 < myName.Count - 1; list_i1++) {
            Console.Write(myName[list_i1]);
            Console.Write(", ");
        }
        if (myName.Count > 0) {
            Console.Write(myName[myName.Count - 1]);
        }
        Console.WriteLine("]");
        List<Boolean> boringList = new List<Boolean> {false, false, false, false, false};
        Console.Write("[");
        for (int list_i1 = 0; list_i1 < boringList.Count - 1; list_i1++) {
            Console.Write(boringList[list_i1]);
            Console.Write(", ");
        }
        if (boringList.Count > 0) {
            Console.Write(boringList[boringList.Count - 1]);
        }
        Console.WriteLine("]");
        List<double> mySlicedList = new List<double>(2);
        
        List<double> temp = new List<double>(0);
        for (int i_temp = 1; i_temp < 3; i_temp++) {
            temp.Add(myOtherList[i_temp]);
        }
        mySlicedList = temp;
        
        if (b >= 6) {
            string dummy = "dummy";
        }
        else if (b == 5) {
            // If body ----------------------------------------------------------------
            int c;
            int d;
            a = 5;
            b = a + 2;
            c = b + 3;
            d = b;
            d -= a;
            c -= d;
            b += 17;
            c += 17;
            a++;
            d++;
            c--;
            b--;
            List<int> myList = new List<int>(5);
            char myObj = 'o';
            const string myConst = "Imconstant";
            Console.WriteLine(a);
            Console.WriteLine(b);
            Console.WriteLine(c);
            Console.WriteLine(d);
            Console.Write("[");
            for (int list_i1 = 0; list_i1 < myOtherList.Count - 1; list_i1++) {
                Console.Write(myOtherList[list_i1]);
                Console.Write(", ");
            }
            if (myOtherList.Count > 0) {
                Console.Write(myOtherList[myOtherList.Count - 1]);
            }
            Console.WriteLine("]");
            Console.Write("[");
            for (int list_i1 = 0; list_i1 < mySlicedList.Count - 1; list_i1++) {
                Console.Write(mySlicedList[list_i1]);
                Console.Write(", ");
            }
            if (mySlicedList.Count > 0) {
                Console.Write(mySlicedList[mySlicedList.Count - 1]);
            }
            Console.WriteLine("]");
            Console.WriteLine("Type an int");
            d = Int32.Parse(Console.ReadLine());
            Console.WriteLine("Type another");
            Console.ReadLine();
            
            Console.WriteLine(" too");
            Console.Write("boo");
            Console.Write(true);
            Console.Write(0);
            Console.Write('c');
            Console.WriteLine(!(true));
            Console.WriteLine(-1);
            Console.WriteLine(Math.Sqrt(4.0));
            Console.WriteLine(Math.Abs(-4));
            Console.WriteLine(Math.Log10(2.0));
            Console.WriteLine(Math.Log(2.0));
            Console.WriteLine(Math.Exp(-2.0));
            Console.WriteLine(Math.Sin(2.0));
            Console.WriteLine(Math.Cos(2.0));
            Console.WriteLine(Math.Tan(2.0));
            Console.WriteLine(Math.Tan(2.0));
            Console.WriteLine(true && false);
            Console.WriteLine(true || false);
            Console.WriteLine(true && !(false));
            Console.WriteLine(!(true && true));
            Console.WriteLine(6 + 2);
            Console.WriteLine(6 - 2);
            Console.WriteLine(6 * 2);
            Console.WriteLine(6 / 2);
            Console.WriteLine(6 % 4);
            Console.WriteLine(Math.Pow(6, 2));
            Console.WriteLine(6 + 2 * 3);
            Console.WriteLine(1.0 / Math.Sin(1.0));
            Console.WriteLine(1.0 / Math.Cos(1.0));
            Console.WriteLine(a);
            Console.WriteLine(true ? 5 : 0);
            Console.WriteLine(1.0 / Math.Tan(1.0));
            // End If body ------------------------------------------------------------
        }
        else {
            Console.WriteLine(args[5]);
        }
        if (boringList != null) {
            Console.WriteLine("Ew, boring list!");
        }
        else {
            Console.WriteLine("Great, no bores!");
        }
        switch(a) {
            case 5:
                b = 10;
                break;
            case 0:
                b = 5;
                break;
            default:
                b = 0;
                break;
        };
        for (int i = 0; i < 9; i += 1) {
            Console.WriteLine(i);
        }
        while (a < 13) {
            Console.WriteLine("Hello");
            a++;
        }
        foreach (double num in myOtherList) {
            Console.WriteLine(Helper.doubleAndAdd(num, 1.0));
        }
        try {
            throw new Exception("Good-bye!");
        } catch {
            Console.WriteLine("Caught intentional error");
        }
    }
}
