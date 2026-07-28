function FileTests(varargin)
    fileToWrite = fopen("testText.txt", "w");
    fprintf(fileToWrite, '%g', 0);
    fprintf(fileToWrite, '%g\n', 0.89);
    fprintf(fileToWrite, '%s', "ello");
    fprintf(fileToWrite, '%s\n', "bye");
    fprintf(fileToWrite, '%s\n', "!!");
    fclose(fileToWrite);
    fileToRead = fopen("testText.txt", "r");
    fileLine = fgetl(fileToRead);
    fgetl(fileToRead);
    assert(fileLine ~= "", "First line should not be empty.");
    
    fileContents = readlines(fileToRead);
    
    fprintf('%s', "[");
    list_i1 = 0;
    while list_i1 < length(fileContents) - 1
        fprintf('%s', fileContents(list_i1 + 1));
        fprintf('%s', ", ");
        list_i1 = list_i1 + 1;
    end
    if length(fileContents) > 0
        fprintf('%s', fileContents(length(fileContents) - 1 + 1));
    end
    fprintf('%s\n', "]");
    assert(length(fileContents) > 0, "fileContents should not be empty.");
    fclose(fileToRead);
end
