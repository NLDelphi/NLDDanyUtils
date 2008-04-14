unit NLDSimpleLogFile;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use


{ History of this unit:
  08-08-2004: * Initial version
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

uses Classes, SysUtils, NLDRcsDirectories;

const Extension = '.log';

{ The logfile always resides in the startup directory of the application.
The name of the logfile used depends on the value of the "No" parameter.
If the parameter is zero (the default), then the logfilename is simply the
name of the application (without its extension) with extension ".log"
(e.g. "Application.log".)
If "No" is > 0 then the same is valid but in the logfilename also "No"
is incorporated, e.g. "Application15.log" (if No = 15).
}

procedure AddToLogFile(S: String; No: Integer = 0); overload;
// Appends "S" to the logfile

procedure AddToLogFile(S: TStrings; No: Integer = 0); overload;
// Appends all lines in "S" to the logfile

procedure ClearLogFile(No: Integer = 0);
// Empties the logfile

function  LogFileName(No: Integer = 0): string;
// Returns the name of the logfile, e.g. for viewing purposes


implementation

procedure AddToLogFile(S: String; No: Integer = 0); overload;
var F: TextFile;
begin
  AssignFile(F, LogFileName(No));
  if Not FileExists(LogFileName(No)) then
  begin
    Rewrite(F);
    CloseFile(F);
  end;
  Append(F);
  WriteLn(F, S);
  CloseFile(F);
end;

procedure AddToLogFile(S: TStrings; No: Integer = 0); overload;
var I: Integer;
    F: TextFile;
begin
  AssignFile(F, LogFileName(No));
  if Not FileExists(LogFileName(No)) then
  begin
    Rewrite(F);
    CloseFile(F);
  end;
  Append(F);
  for I := 0 to S.Count - 1 do  WriteLn(F, S[I]);
  CloseFile(F);
end;

procedure ClearLogFile(No: Integer = 0);
var F: TextFile;
begin
  AssignFile(F, LogFileName(No));
  Rewrite(F);   // make a new file
  CloseFile(F);
end;

function DefaultLogFile: string;
begin
  Result := AppStartupDirectory(true) + ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

function LogFileName(No: Integer = 0): string;
begin
  if No <= 1
  then Result := DefaultLogFile + Extension
  else Result := DefaultLogFile + IntToStr(No) + Extension;
end;


end.
