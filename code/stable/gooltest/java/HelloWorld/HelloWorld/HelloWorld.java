package HelloWorld;

/** \file HelloWorld.java
    \author Brooks MacLachlan
    \brief Tests various GOOL functions. It should run without errors.
*/
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;

public class HelloWorld {
    
    public static void main(String[] args) {
        // Initializing variables
        int a;
        int b = 5;
        ArrayList<Double> myOtherList = new ArrayList<Double>(Arrays.asList(1.0, 1.5));
        int oneIndex = myOtherList.indexOf(1.0);
        System.out.println(oneIndex);
        a = myOtherList.size();
        myOtherList.add(2, 2.0);
        myOtherList.add(2.5);
        double e;
        e = myOtherList.get(1);
        myOtherList.set(1, 17.4);
        ArrayList<String> myName = new ArrayList<String>(7);
        myName = new ArrayList<String>(Arrays.asList("Brooks Mac".split(" ")));
        System.out.println(myName);
        ArrayList<Boolean> boringList = new ArrayList<Boolean>(Arrays.asList(false, false, false, false, false));
        System.out.println(boringList);
        ArrayList<Double> mySlicedList = new ArrayList<Double>(2);
        
        ArrayList<Double> temp = new ArrayList<Double>(0);
        for (int i_temp = 1; i_temp < 3; i_temp++) {
            temp.add(myOtherList.get(i_temp));
        }
        mySlicedList = temp;
        
        if (b >= 6) {
            String dummy = "dummy";
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
            ArrayList<Integer> myList = new ArrayList<Integer>(5);
            char myObj = 'o';
            final String myConst = "Imconstant";
            System.out.println(a);
            System.out.println(b);
            System.out.println(c);
            System.out.println(d);
            System.out.println(myOtherList);
            System.out.println(mySlicedList);
            System.out.println("Type an int");
            d = Integer.parseInt((new Scanner(System.in)).nextLine());
            System.out.println("Type another");
            (new Scanner(System.in)).next();
            
            System.out.println(" too");
            System.out.print("boo");
            System.out.print(true);
            System.out.print(0);
            System.out.print('c');
            System.out.println(!(true));
            System.out.println(-1);
            System.out.println(Math.sqrt(4.0));
            System.out.println(Math.abs(-4));
            System.out.println(Math.log10(2.0));
            System.out.println(Math.log(2.0));
            System.out.println(Math.exp(-2.0));
            System.out.println(Math.sin(2.0));
            System.out.println(Math.cos(2.0));
            System.out.println(Math.tan(2.0));
            System.out.println(Math.tan(2.0));
            System.out.println(true && false);
            System.out.println(true || false);
            System.out.println(true && !(false));
            System.out.println(!(true && true));
            System.out.println(6 + 2);
            System.out.println(6 - 2);
            System.out.println(6 * 2);
            System.out.println(6 / 2);
            System.out.println(6 % 4);
            System.out.println(Math.pow(6, 2));
            System.out.println(6 + 2 * 3);
            System.out.println(1.0 / Math.sin(1.0));
            System.out.println(1.0 / Math.cos(1.0));
            System.out.println(a);
            System.out.println(true ? 5 : 0);
            System.out.println(1.0 / Math.tan(1.0));
            // End If body ------------------------------------------------------------
        }
        else {
            System.out.println(args[5]);
        }
        if (boringList != null) {
            System.out.println("Ew, boring list!");
        }
        else {
            System.out.println("Great, no bores!");
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
            System.out.println(i);
        }
        while (a < 13) {
            System.out.println("Hello");
            a++;
        }
        for (double num : myOtherList) {
            System.out.println(Helper.doubleAndAdd(num, 1.0));
        }
        try {
            throw new Exception("Good-bye!");
        } catch (Exception exc) {
            System.out.println("Caught intentional error");
        }
    }
}
