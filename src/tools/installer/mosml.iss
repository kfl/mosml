; Inno Setup script for Moscow ML ver. 2.10.1

[Setup]
AppId={{A34767A0-A549-48AB-90BB-F460DA36ED51}
AppName=Moscow ML
AppVersion=2.10.1
AppVerName=Moscow ML version 2.10.1
AppPublisherURL=http://mosml.org
AppSupportURL=https://github.com/kfl/mosml/issues
AppUpdatesURL=http://mosml.org
DefaultDirName={pf}\mosml
DefaultGroupName=Moscow ML
OutputBaseFilename=mosml-setup-2.10.1
Compression=lzma
SolidCompression=yes
ChangesEnvironment=yes

[Languages]
Name: "da"; MessagesFile: "compiler:Languages\Danish.isl"
Name: "en"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "mosml\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs; Components: mosml

[Components]
Name: "mosml"; Description: "Moscow ML v2.10.1"; Flags: fixed; Types: full custom

[Types]
Name: "full"; Description: "{cm:FullInstall}"
Name: "custom"; Description: "{cm:CustomInstall}"; Flags: iscustom

[Tasks]
Name: startmenu; Description: "{cm:StartMenu}"
Name: desktopicon; Description: "{cm:DesktopIcon}"

[Icons]
Name: "{group}\{cm:MosMLName}"; Filename: "{app}\bin\mosml.exe"; Parameters: "-P full"; WorkingDir: "{userdocs}"; Components: mosml; Tasks: startmenu
Name: "{group}\{cm:UninstallProgram,Moscow ML}"; Filename: "{uninstallexe}"; Tasks: startmenu
Name: "{group}\{cm:Documentation}\{cm:OwnersManual}"; Filename: "{app}\share\doc\mosml\manual.pdf"; Tasks: startmenu
Name: "{group}\{cm:Documentation}\{cm:LibraryDocs}"; Filename: "{app}\share\doc\mosml\mosmllib\index.html"; Tasks: startmenu
Name: "{group}\{cm:Documentation}\{cm:LanguageOverview}"; Filename: "{app}\share\doc\mosml\mosmlref.pdf"; Tasks: startmenu
Name: "{commondesktop}\{cm:MosMLName}"; Filename: "{app}\bin\mosml.exe"; Parameters: "-P full"; WorkingDir: "{userdocs}"; Components: mosml; Tasks: desktopicon

[CustomMessages]
en.FullInstall=Full installation
da.FullInstall=Fuld installation
en.CustomInstall=Custom installation
da.CustomInstall=Brugerdefineret installation

en.StartMenu=Create Start Menu shortcuts
da.StartMenu=Lav genveje i startmenuen
en.DesktopIcon=Create shortcuts on your desktop
da.DesktopIcon=Lav genveje på skrivebordet

en.MosMLName=Moscow ML Commandline
da.MosMLName=Moscow ML Kommandolinje
en.UninstallProgram=Uninstall %1
da.UninstallProgram=Afinstallér %1

en.Documentation=Documentation
da.Documentation=Dokumentation
en.OwnersManual=Moscow ML Owner's Manual
da.OwnersManual=Moscow ML Owner's Manual
en.LibraryDocs=Moscow ML Library
da.LibraryDocs=Moscow ML Library
en.LanguageOverview=Moscow ML Language Overview
da.LanguageOverview=Moscow ML Language Overview

en.Updating=Updating %1...
da.Updating=Opdaterer %1...
en.Setting=Setting %1...
da.Setting=Sætter %1...
da.Deleting=Sletter %1...
en.Deleting=Deleting %1...

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: expandsz; ValueName: "Path"; ValueData: "{olddata};{app}\bin"; Flags: preservestringtype; Components: mosml
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "MOSMLLIB"; ValueData: "{app}\lib\mosml"; Flags: uninsdeletevalue; Components: mosml
