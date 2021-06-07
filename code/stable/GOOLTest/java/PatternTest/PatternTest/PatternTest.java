package PatternTest;

import java.util.ArrayList;
import java.util.Arrays;

public class PatternTest {
    
    public static void main(String[] args) {
        int n;
        String myFSM = "Off";
        myFSM = "On";
        switch(myFSM) {
            case "Off":
                System.out.println("Off");
                break;
            case "On":
                System.out.println("On");
                break;
            default:
                System.out.println("Neither");
                break;
        };
        
        System.out.println("myStrat");
        n = 3;
        
        Observer obs1 = new Observer();
        Observer obs2 = new Observer();
        
        ArrayList<Observer> observerList = new ArrayList<Observer>(Arrays.asList(obs1));
        observerList.add(observerList.size(), obs2);
        for (int observerIndex = 0; observerIndex < observerList.size(); observerIndex++) {
            observerList.get(observerIndex).printNum();
        }
        
        obs1.setX(10);
        System.out.print(obs1.getX());
    }
}
