unit NLDRcsEnvironmentVars;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit:
  23-07-2004: * Initial Version, donated to NL Deplhi.
  25-07-2004: * Added TStrings trimming in applicable procedures
  27-07-2004: * Some minor changes.
}

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

uses Windows, Classes, SysUtils;

procedure GetEnvironmentVars(EVars: TStrings);
// Gets all environment variables in a StringList "EVars".
// To access the different entries, one can use the "count", "names" and
// "values" properties of "TStrings".
// Also the function "GetEnvironmentVar" below can be used.

procedure GetEnvironmentVarNames(Names: TStrings);
// Gets all environment variable names (not the values) in a StringList "Names"

procedure SetEnvironmentVar(const Name, Value: string);
// Sets the (local) environment variable "Name" to the value "Value".

function GetEnvironmentVar(const Name: string): string;
// Returns the value of the (local) environment variable "Name".

function GetPATHEnvironmentVar: string; overload;
// Returns the complete "PATH" environment variable value in a string.

procedure GetPATHEnvironmentVar(Paths: TStrings); overload;
// Returns the "PATH" environment variable value in "Paths",
// each entry in "Paths" is actually one path.


{ Addirional info:
In XP, environment variables are stored in the registry:
  HKEY_CURRENT_USER\Environment and
  HKEY_LOCAL_MACHINE\CurrentControlSet\Control\SessionManager\Environment
}

implementation

uses NLDRcsStrings;

procedure GetEnvironmentVars(EVars: TStrings);
var
  S: pointer;
  Tmp: string;
  P: PChar;
begin
  EVars.Clear;
  S := Windows.GetEnvironmentStrings;
  P := S;
  Tmp := string(P);
  while Length(Tmp) > 0 do
  begin
    if BeginString(Tmp, '=') > '' then Evars.Add(Tmp);
    P := P + Length(Tmp) + 1;
    Tmp := string(P);
  end;
  Windows.FreeEnvironmentStrings(S);
  TrimTStrings(EVars, [trsTrim, trsEmptyLines]);
end;

procedure GetEnvironmentVarNames(Names: TStrings);
var
  L: TStrings;
  I: Integer;
begin
  Names.Clear;
  L := TStringList.Create;
  try
    GetEnvironmentVars(L);
    for I := 0 to L.Count - 1 do
      Names.Add(L.Names[I]);
  finally
    L.Free;
  end;
  TrimTStrings(Names, [trsTrim, trsEmptyLines]);
end;

procedure SetEnvironmentVar(const Name, Value: string);
begin
  Windows.SetEnvironmentVariable(PChar(Name), PChar(Value));
end;

function GetEnvironmentVar(const Name: string): string;
begin
  Result := SysUtils.GetEnvironmentVariable(Name);
end;

function GetPATHEnvironmentVar: string;
begin
  Result := GetEnvironmentVar('PATH');
end;

procedure GetPATHEnvironmentVar(Paths: TStrings);
begin
  StringToTStrings(GetPathEnvironmentVar, Paths, ';');
  TrimTStrings(Paths, [trsTrim, trsEmptyLines]);
end;

end.
