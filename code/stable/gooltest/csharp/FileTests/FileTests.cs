using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;

public class FileTests {
    
    public static void Main(string[] args) {
        StreamWriter fileToWrite;
        fileToWrite = new StreamWriter("testText.txt", false);
        fileToWrite.Write(0);
        fileToWrite.WriteLine(0.89);
        fileToWrite.Write("ello");
        fileToWrite.WriteLine("bye");
        fileToWrite.WriteLine("!!");
        fileToWrite.Close();
        StreamReader fileToRead;
        fileToRead = new StreamReader("testText.txt");
        string fileLine;
        fileLine = fileToRead.ReadLine();
        fileToRead.ReadLine();
        Debug.Assert( fileLine != "" , "First line should not be empty.");
        List<string> fileContents = new List<string>(0);
        
        while (!(fileToRead.EndOfStream)) {
            fileContents.Add(fileToRead.ReadLine());
        }
        
        Console.Write("[");
        for (int list_i1 = 0; list_i1 < fileContents.Count - 1; list_i1++) {
            Console.Write(fileContents[list_i1]);
            Console.Write(", ");
        }
        if (fileContents.Count > 0) {
            Console.Write(fileContents[fileContents.Count - 1]);
        }
        Console.WriteLine("]");
        Debug.Assert( fileContents.Count > 0 , "fileContents should not be empty.");
        fileToRead.Close();
    }
}
