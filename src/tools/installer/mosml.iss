; Inno Setup Script for installing Moscow ML ver. 2.00
; Created: 2001-02-04 by Ken Friis Larsen <kfl@it.edu>
; Modified: 2001-05-10 kfl

[Setup]
AppName            = Moscow ML
AppVerName         = Moscow ML version 2.00
AppVersion         = 2.00
;AppCopyright=Copyright © 1997-2001 Peter Sestoft
AppPublisherURL    = http://www.dina.kvl.dk/~sestoft/mosml.html
DefaultDirName     = {pf}\Mosml
DefaultGroupName   = Moscow ML
CompressLevel      = 9
InfoAfterFile      = src/install/infoafter.txt
OutputBaseFilename = mosml-setup-2.00

[Dirs]
Name: "{app}\bin"
Name: "{app}\copyrght"
Name: "{app}\config.w32"
Name: "{app}\doc"
Name: "{app}\doc\mosmllib"
Name: "{app}\examples"
Name: "{app}\examples\calc"
Name: "{app}\examples\cgi"
Name: "{app}\examples\lexyacc"
Name: "{app}\examples\lexyacc\cl"
Name: "{app}\examples\manual"
Name: "{app}\examples\mls"
Name: "{app}\examples\modules"
Name: "{app}\examples\parsercomb"
Name: "{app}\examples\paulson"
Name: "{app}\examples\pretty"
Name: "{app}\examples\small"
Name: "{app}\examples\units"
Name: "{app}\examples\weak"
Name: "{app}\examples\webserver"
Name: "{app}\include"
Name: "{app}\lib"
Name: "{app}\tools"
Name: "{app}\utility"
Name: "{app}\utility\sml-mode-3.3b"

[Files]
Source : "bin\*"; DestDir : "{app}\bin"
Source : "config"; DestDir : "{app}"
Source : "config.w32\*"; DestDir : "{app}\config.w32" 
Source : "copyrght\*"; DestDir : "{app}\copyrght"
Source : "doc\*"; DestDir : "{app}\doc"
Source : "doc\recomp"; DestDir : "{app}\doc"
Source : "doc\mosmllib\*"; DestDir : "{app}\doc\mosmllib"
Source : "examples\*" ; DestDir : "{app}\examples"
Source : "examples\calc\*" ; DestDir : "{app}\examples\calc"
Source : "examples\cgi\*" ; DestDir : "{app}\examples\cgi"
Source : "examples\lexyacc\*" ; DestDir : "{app}\examples\lexyacc"
Source : "examples\lexyacc\cl\*" ; DestDir : "{app}\examples\lexyacc\cl"
Source : "examples\manual\*" ; DestDir : "{app}\examples\manual"
Source : "examples\mls\*" ; DestDir : "{app}\examples\mls"
Source : "examples\modules\*" ; DestDir : "{app}\examples\modules"
Source : "examples\parsercomb\*" ; DestDir : "{app}\examples\parsercomb"
Source : "examples\paulson\*" ; DestDir : "{app}\examples\paulson"
Source : "examples\pretty\*" ; DestDir : "{app}\examples\pretty"
Source : "examples\small\*" ; DestDir : "{app}\examples\small"
Source : "examples\units\*" ; DestDir : "{app}\examples\units"
Source : "examples\weak\*" ; DestDir : "{app}\examples\weak"
Source : "examples\webserver\*" ; DestDir : "{app}\examples\webserver"
Source : "include\*" ; DestDir : "{app}\include"
Source : "install.txt" ; DestDir : "{app}"
Source : "readme" ; DestDir : "{app}"
Source : "roadmap" ; DestDir : "{app}"
Source : "lib\*" ; DestDir : "{app}\lib"
Source : "tools\*" ; DestDir : "{app}\tools"
Source : "utility\*" ; DestDir : "{app}\utility"
Source : "utility\sml-mode-3.3b\*" ; DestDir : "{app}\utility\sml-mode-3.3b"

; Some helper programs for Windows 95 and derrived
Source : "addline.bat"; DestDir : "{tmp}" ; Flags : deleteafterinstall ; OnlyBelowVersion : 0,3.0
Source : "dellines.exe"; DestDir : "{app}\lib" ; OnlyBelowVersion : 0,3.0


[Icons]
Name : "{group}\Moscow ML" ; FileName : "{app}\bin\mosml.exe" ; Parameters : "-P full" ; WorkingDir : "{app}" ; IconFilename : "{app}\utility\FS2_cow.ico" 
Name : "{userdesktop}\Moscow ML" ; FileName : "{app}\bin\mosml.exe" ; Parameters : "-P full" ; WorkingDir : "{app}" ; IconFilename : "{app}\utility\FS2_cow.ico" 

[Registry]
; Set environment variables on Windows NT, Windows 2000
Root: HKCU ; Subkey : "Environment"; ValueType : string ; ValueName : "MOSMLLIB"; ValueData : "{app}\lib"; Flags: uninsdeletevalue; MinVersion : 0,3.51
Root: HKCU ; Subkey : "Environment"; ValueType : expandsz ; ValueName : "Path"; ValueData : "{olddata};{app}\bin"; MinVersion : 0,3.51

; The following two lines are system-wide installation
;Root: HKLM ; Subkey : "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType : string ; ValueName : "MOSMLLIB"; ValueData : "{app}\lib"; MinVersion : 0,3.51
;Root: HKLM ; Subkey : "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType : expandsz ; ValueName : "PATH"; ValueData : "{olddata};{app}\bin"; MinVersion : 0,3.51

[Run]
; Set environment variables on Windows 95 and derrived
Filename : "{tmp}\addline.bat"; Description : "Set environment variables"; StatusMsg : "Setting environment variables..."; Parameters : """{app}\lib"" ""{app}\bin""" ; Flags : postinstall ; OnlyBelowVersion : 0,3.0

[UninstallRun]
; Unset environment variables on Windows 95 and derrived
Filename : "{app}\lib\dellines.exe" ; OnlyBelowVersion : 0,3.0


