package FileTests;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

public class FileTests {
    
    public static void main(String[] args) throws FileNotFoundException, IOException {
        PrintWriter fileToWrite;
        fileToWrite = new PrintWriter(new FileWriter(new File("testText.txt"), false));
        fileToWrite.print(0);
        fileToWrite.println(0.89);
        fileToWrite.print("ello");
        fileToWrite.println("bye");
        fileToWrite.println("!!");
        fileToWrite.close();
        Scanner fileToRead;
        fileToRead = new Scanner(new File("testText.txt"));
        String fileLine;
        fileLine = fileToRead.nextLine();
        fileToRead.nextLine();
        ArrayList<String> fileContents = new ArrayList<String>(0);
        
        while (fileToRead.hasNextLine()) {
            fileContents.add(fileToRead.nextLine());
        }
        
        System.out.println(fileContents);
        fileToRead.close();
    }
}
