module ReadMeGen (openMDFile) where

import System.IO
import Data.Time

type FileName = String
-- type FileData = String
-- type FileDataList = [String]

-- have separate scripts to obtain list of libraries and unsupported OSs

-- gets current date and time (EST) --
getDateTime :: ZonedTime -> String
getDateTime rawDateTime = formatTime defaultTimeLocale "%F %T %Z" rawDateTime

-- gets info to be inputted into README file (program dependencies, etc.) --
getMDFileInfo :: FileName -> IO [String]
getMDFileInfo fileName = do
	infoFile <- openFile fileName ReadMode
	caseStudyName <- hGetLine infoFile
	progLang <- hGetLine infoFile
	progLangVers <- hGetLine infoFile
	libraries <- hGetLine infoFile
	unsupportedOSs <- hGetLine infoFile
	inputFilePath <- hGetLine infoFile
	return [caseStudyName,progLang,progLangVers,libraries,unsupportedOSs,inputFilePath]

-- opens/creates README file (markdown file format; .md), inputs info from getMDFileInfo --
-- (current sample) example use: openMDFile "README.md" "READMEInfo.txt"
openMDFile :: FileName -> FileName -> IO ()
openMDFile fileName infoFileName = do
	currentDateTime <- getZonedTime
	-- a <- caseStudyName, b <- progLang, c <- progLangVers
	-- d <- libraries, e <- unsupportedOSs, f <- inputFilePath
	[a,b,c,d,e,f] <- getMDFileInfo infoFileName
	mdFile <- openFile (fileName) WriteMode
	hPutStrLn mdFile ("------------------------------------------------------------" ++ 
					   "\n### Summary of Key Information for Running " ++ a ++ " on " 
					   ++ b ++ 
					   "\nLast Updated: " ++ getDateTime currentDateTime ++ 
					   "\n" ++ b ++ " Version: " ++ c ++
					   "\n------------------------------------------------------------")

	hPutStrLn mdFile ("\n**Program Dependencies:**" ++ "\n - " ++ d)

	hPutStrLn mdFile ("\n**Unsupported OSs:**" ++ "\n - " ++ e)

	hPutStrLn mdFile ("\n**How to Run Program:**" ++ "\n - Enter in the following line into your terminal command line: "
						++ "\n`make run RUNARGS=" ++ f ++ "`")

	hPutStrLn mdFile ("------------------------------------------------------------")

	hClose mdFile
