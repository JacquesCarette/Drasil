using System;
using System.Collections.Generic;

public class PatternTest {
    
    public static void Main(string[] args) {
        int n;
        
        Console.WriteLine("myStrat");
        n = 3;
        
        Observer obs1 = new Observer();
        Observer obs2 = new Observer();
        
        List<Observer> observerList = new List<Observer> {obs1};
        observerList.Insert(observerList.Count, obs2);
        for (int observerIndex = 0; observerIndex < observerList.Count; observerIndex++) {
            observerList[observerIndex].printNum();
        }
        
        obs1.setX(10);
        Console.Write(obs1.getX());
    }
}
