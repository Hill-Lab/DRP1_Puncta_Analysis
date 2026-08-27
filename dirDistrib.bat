@rem
@echo off

rem enforce input arguments
if "%2" == "" (
	echo Usage: %0 '^<base directory^>' '^<number of files per new directory^>'
	Exit /b
)

SetLocal EnableDelayedExpansion

rem detect number of files in specified base directory
For /f %%A in ('dir %1 ^| find "File(s)"') do Set /A totalfiles = %%A

rem terminate execution if no files detected
if %totalfiles% equ 0 (
	echo No files to distribute
	Exit /b
)

rem initialize variables
Set /A numFiles = %2
Set /A fileNum = 1
Set /A folderNum = 1
Set /A count = 0

echo Distributing files...

rem iterate through all files and distribute user-specified number to folders
For %%f in (%1\*) do (
	if not exist "%1%\!folderNum!\" (
		md "%1%\!folderNum!\"
	)	
	Move "%%f" "%1%\!folderNum!\" >nul

	If !fileNum! equ %numFiles% (
		Set /A folderNum += 1
		Set /A fileNum = 0
	)
	Set /A fileNum += 1
	Set /A count += 1
)

echo %count% files have been distributed to %folderNum% folders
Exit /b