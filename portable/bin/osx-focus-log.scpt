#!/usr/bin/osascript
tell application "System Events"
	set frontApp to name of first application process whose frontmost is true
end tell

set filePath to ""

if frontApp is "Visual Studio Code" then
	tell application "Visual Studio Code"
		set filePath to path of document 1
	end tell

else if frontApp is "TextEdit" then
	tell application "TextEdit"
		if count of documents > 0 then
			set filePath to path of document 1
		end if
	end tell

else if frontApp is "Finder" then
	tell application "Finder"
		if exists Finder window 1 then
			set filePath to POSIX path of (target of Finder window 1 as alias)
		end if
	end tell
end if

set isoNow to do shell script "date -u +%Y-%m-%dT%H:%M:%SZ"
set logLine to isoNow & " " & frontApp & " " & filePath
do shell script "echo " & quoted form of logLine & " >> ~/.focus_history"
