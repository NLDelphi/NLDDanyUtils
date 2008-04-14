unit NLDShortcuts;

// Dany Rosseel


{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use


{ History of this unit:
  30-05-2004: * Initial version, with thanks to walterheck.
  27-06-2004: * Moved the "GetSpecialFolderPath"and "GetOsVersion" to a new
                unit: "RcsDirectories"
}


(*
  MOST IMPORTANT:
  - Most of the code in this unit comes from the NLD Forum "Tiphoek" by 
    moderator walterheck:
    url= http://www.nldelphi.com/forum/showthread.php?postid=23162#post23162
  - Ikzelf heb alleen de routine "RemoveShortCut" toegevoegd. 
*)

(*
    AANROEP
 procedure TForm1.Button1Click(Sender: TObject);
var
  SourceFileName: TFileName;
  Location: TLocationType;
  IconPath: string;
  IconIndex: Integer;
  ShortcutName: string;
  Arguments: string;
  Tooltip: string;
begin
  SourceFileName:='c:\windows\explorer.exe';
  Arguments:= 'c:';
  Location:=locDesktop;
  IconPath:=Application.ExeName;
  IconIndex:=0;
  Tooltip:='Dubbelklik het icoon om de applicatie te starten';
  ShortcutName:='Tiphoek - Create een shortcut';
  case CreateShortCut(SourceFileName, Location, IconPath, IconIndex, Tooltip, ShortcutName, Arguments) of
    RES_FAILED_NOT_SUPPORTED:
      ShowMessage('Geen support voor deze lokatie');
    RES_OK:
      ShowMessage('Geen problemen');
    RES_FAILED_UNKNOWN:
      ShowMessage('Onbekende fout tijdens het maken van de shortcut');
    RES_FAILED_PATH_RETRIEVE:
      ShowMessage('Fout bij het opzoeken van het pad voor de locatie');
  end;
end;
 *)

{$IFDEF NoDebug}

{$O+} // Optimisation ON
{$D-} // Debug information OFF
{$I-} // I/O checking OFF
{$L-} // Local Symbols OFF
{$Q-} // Overflow Checking OFF
{$R-} // Range Checking OFF

{$ELSE}
{$O-} // Optimisation OFF
{$D+} // Debug information ON
{$I+} // I/O checking ON
{$L+} // Local Symbols ON
{$Q+} // Overflow Checking ON
{$R+} // Range Checking ON

{$ENDIF}

{$W-} // Stack Frames OFF
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses Windows, SysUtils, NLDRcsDirectories;

function CreateShortCut(SourceFileName: TFileName;
                        Location: TLocationType;
                        IconPath: string;
                        IconIndex: Integer;
                        Tooltip: string;
                        ShortcutName: string;
                        Arguments: string;
                        WorkingDirectory: string = '';
                        StartMenuFolder: string = ''): Integer;
function RemoveShortCut(Location: TLocationType;
                        ShortCutName: string;
                        StartMenuFolder: string = ''): Integer;



//*************************************************************************

implementation

uses
  ActiveX,
  ShlObj,
  ComObj;

function CreateShortCut(SourceFileName: TFileName;
                        Location: TLocationType;
                        IconPath: string;
                        IconIndex: Integer;
                        Tooltip: string;
                        ShortcutName: string;
                        Arguments: string;
                        WorkingDirectory: string = '';
                        StartMenuFolder: string = ''): Integer;
var
  PrefixPath: string;
  MyObject: IUnknown;
  MySLink: IShellLink;
  MyPFile: IPersistFile;
  WFileName: WideString;
begin

  PrefixPath := GetSpecialFolderPath(Location, StartMenuFolder);
  if PrefixPath = '' then
  begin
    Result := RES_FAILED_PATH_RETRIEVE;
    exit;
  end;

  // maak een shellink object aan...
  MyObject := CreateComObject(CLSID_ShellLink);
  MySLink  := MyObject as IShellLink;
  MyPFile  := MyObject as IPersistFile;

  // doe de juiste instellingen...
  MySLink.SetPath(PChar(SourceFileName));
  if trim(WorkingDirectory) = ''
  then MySLink.SetWorkingDirectory(PChar(ExtractFileDir(SourceFileName)))
  else MySLink.SetWorkingDirectory(PChar(WorkingDirectory));
  MySLink.SetIconLocation(PChar(IconPath),IconIndex);
  MySLink.SetDescription(PChar(Tooltip));
  MySLink.SetArguments(PChar(Arguments));

  //create de shortcut...
  WFileName := IncludeTrailingBackSlash(PrefixPath) + ShortcutName + '.lnk';
  MyPFile.Save(PWChar(WFileName), False);

  // alles ging goed!! ...
  Result := RES_OK;
end;

function RemoveShortCut(Location: TLocationType;
                        ShortCutName: string;
                        StartMenuFolder: string = ''): Integer;
var
  PrefixPath: string;
  WFileName: WideString;
begin

  PrefixPath := GetSpecialFolderPath(Location, StartMenuFolder);
  if PrefixPath = '' then
  begin
    Result := RES_FAILED_PATH_RETRIEVE;
    exit;
  end;

  //remove de shortcut...
  if PrefixPath <> '' then
  begin
    WFileName := PrefixPath + '\' + ShortcutName + '.lnk';
    if fileexists(WFileName) then deletefile(WFileName);
  end;

  // alles ging goed!! ...
  Result := RES_OK;
end;

end.
