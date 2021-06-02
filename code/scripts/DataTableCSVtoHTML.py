import csv
import os.path

#Assuming that make deploy has been called first. Constructs the needed paths
currentPath = os.path.dirname(__file__)
analysisPath = os.path.relpath('..\\deploy\\analysis\\DataTable.csv', currentPath)
analysisHTMLPath = os.path.relpath('.\\dataTable.html', analysisPath)

with open (analysisPath, 'r', newline='') as dataTable, open (analysisHTMLPath, 'w') as dataTablehtml:
    contents = csv.reader(dataTable, delimiter=' ', quotechar='|')
    #Adding the title to dataTable.html
    dataTablehtml.write('<!DOCTYPE html>\n<html>\n\t<title>Auto-Generated Data Table for Drasil</title>\n')
    #Adding the table to dataTable.html
    dataTablehtml.write('\t<table border="1" class="dataframe">\n')
    titleRow = True
    headerRow = True
    for row in contents:
        #The header row should only be True for the header.
        if titleRow == True:
            titleRow = False
            #Adding titles to dataTable.html
            dataTablehtml.write('\t\t<thead>\n\t\t\t<tr>\n')
            for title in row:
                #may need to include an if statement that takes out spaces?
                dataTablehtml.write('\t\t\t\t<th>' + title + '</th>\n')
            dataTablehtml.write('\t\t\t</tr>\n\t\t</thead>\n')
        elif headerRow == True:
            headerRow = False
            #Adding headers to dataTable.html
            dataTablehtml.write('\t\t<thead>\n\t\t\t<tr>\n')
            for header in row:
                #may need to include an if statement that takes out spaces?
                dataTablehtml.write('\t\t\t\t<th>' + header + '</th>\n')
            dataTablehtml.write('\t\t\t</tr>\n\t\t</thead>\n\t\t<tbody>\n')
        else:
            dataTablehtml.write('\t\t\t<tr>\n')
            for item in row:
                dataTablehtml.write('\t\t\t\t<td>' + item + '</td>\n')
            dataTablehtml.write('\t\t\t</tr>\n')
    dataTablehtml.write('\t\t</tbody>\n')



    dataTablehtml.write('</html>')