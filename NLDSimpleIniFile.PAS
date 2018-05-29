unit NLDSimpleIniFile;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit:
  08-02-2004: * Initial version
  12-02-2004: * Changed the place of the inifile to the 'startup' directory
                in stead of the *.exe file directory
  03-05-2004: * Donated to NL Delphi
  08-05-2004: * Names of procedures and functions not longer overloaded:
                it was not always possible to have the parameter list different.
              * Changed the names of the 'Save' procedures in 'Write' procedures.
              * Added writing and reading of booleans and integers.
  17-05-2004: * Made the whole thing a class derived from TIniFile.
              * Preserved the "Class-less" procedures and functions as defined before
              * Added procedures to write and read TStrings in two ways:
                1. A 'key' used for each string (named 'StringX'),
                   one can only define the section here.
                2. The whole TStrings is stored as one comma separated string.
                   One can define the section and the keys here.
  30-05-2004: * Removed some "TStrings" routines,
                those left are capable of handling all types of strings.
  02-06-2004: * Completed the "class free" set of routines
  03-06-2004: * Simplified the "class free" TString routines
  13-11-2004: * Added procedures and functions to write and read "Objects"
                texts or status to and from the inifile.
  06-02-2005: * Added "const" in a number of function/procedure parameterlists
  23-04-2005: * Added some formatting of the IniFile at destruction time.
  15-08-2005: * Added the type "TMaskEdit" to the "WriteObject" and "ReadObject" procedures
  19-11-2006: * Disabled the formatting action on read only inifiles.
	      * Improved the formatting action.
  30-01-2010: * Added the type "TRadioGroup" to the to the "WriteObject" and "ReadObject" procedures
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

uses Classes,
  IniFiles,
  StdCtrls,
  ExtCtrls;

{ ---  Class "TSimpleIniFile" --- }

type
  TSimpleIniFile = class(TIniFile)
  private
    IniFileName: string;
  public
    constructor Create(fn: string = '');
    // Added procedures in this Class with respect to "TIniFile"
    destructor Destroy; override;
    procedure WriteTStrings(const Section, Key: string; Texts: TStrings);
      overload;
    procedure ReadTStrings(const Section, Key: string; Texts: TStrings);
      overload;
    procedure WriteTStrings(const Section: string; Texts: TStrings); overload;
    procedure ReadTStrings(const Section: string; Texts: TStrings); overload;
    procedure WriteObject(const Section: string; Obj: TObject);
    procedure ReadObject(const Section: string; Obj: TObject);
  end;

{ --- Procedures and Functions without usage of a Class --- }

// BinaryStream ----------------------------------------------------
procedure WriteBinaryStreamToIniFile(const Section, Key: string; Value:
  TStream);
function ReadBinaryStreamFromIniFile(const Section, Key: string; Value:
  TStream):
  Integer;

// Bool ----------------------------------------------------
procedure WriteBoolToIniFile(const Section, Key: string; Value: Boolean);
function ReadBoolFromIniFile(const Section, Key: string): Boolean;

// Date ----------------------------------------------------
procedure WriteDateToIniFile(const Section, Key: string; Value: TDateTime);
function ReadDateFromIniFile(const Section, Key: string): TDateTime;

// DateTime ----------------------------------------------------
procedure WriteDateTimeToIniFile(const Section, Key: string; Value: TDateTime);
function ReadDateTimeFromIniFile(const Section, Key: string): TDateTime;

// Float ----------------------------------------------------
procedure WriteFloatToIniFile(const Section, Key: string; Value: Double);
function ReadFloatFromIniFile(const Section, Key: string): Double;

// Integer ----------------------------------------------------
procedure WriteIntegerToIniFile(const Section, Key: string; Value: Integer);
function ReadIntegerFromIniFile(const Section, Key: string): Integer;

// Time ----------------------------------------------------
procedure WriteTimeToIniFile(const Section, Key: string; Value: TDateTime);
function ReadTimeFromIniFile(const Section, Key: string): TDateTime;

// String ----------------------------------------------------
procedure WriteStringToIniFile(const Section, Key, Value: string);
function ReadStringFromIniFile(const Section, Key: string): string;

// TStrings ----------------------------------------------------
procedure WriteTStringsToIniFile(const Section, Key: string; Texts: TStrings);
overload;
procedure ReadTStringsFromIniFile(const Section, Key: string; Texts: TStrings);
overload;
procedure WriteTStringsToIniFile(const Section: string; Texts: TStrings);
overload;
procedure ReadTStringsFromIniFile(const Section: string; Texts: TStrings);
overload;

// Objects Texts and Statusses
procedure WriteObjectToIniFile(const Section: string; Obj: TObject);
procedure ReadObjectFromIniFile(const Section: string; Obj: TObject);

// Miscellaneous ----------------------------------------------------
procedure DeleteKeyFromIniFile(const Section, Key: string);
procedure EraseSectionFromIniFile(const Section: string);
procedure ReadSectionFromIniFile(const Section: string; Texts: TStrings);
procedure ReadSectionsFromIniFile(Texts: TStrings);
procedure ReadSectionValuesFromIniFile(const Section: string; Texts: TStrings);
function SectionExistsInIniFile(const Section: string): Boolean;
function ValueExistsInIniFile(const Section, Key: string): Boolean;

implementation

uses SysUtils,
  NLDRcsStrings,
  Mask;

var
  DefaultIniFileName: string;

constructor TSimpleIniFile.Create(fn: string = '');
begin
  if fn = '' then
    IniFileName := DefaultIniFileName
  else
    IniFileName := fn;
  inherited Create(IniFileName);
end;

destructor TSimpleIniFile.Destroy;
//var
  //Orig, Formatted: TStrings;
  //I: Integer;
begin
  inherited destroy; // close the Inifile
  
  {
  // Format the IniFile
  if FileExists(IniFileName) and
    (not FileIsReadOnly(IniFileName)) then
  begin
    Orig := TStringList.Create;
    Formatted := TStringList.Create;
    try
      Orig.LoadFromFile(IniFileName);
      Formatted.Assign(Orig);
      TrimTStrings(Formatted, [trsTrim, trsEmptyLines]);
      I := Formatted.Count - 1;
      while I > 1 do
      begin
        if (Length(Formatted[I]) > 2) and (Formatted[I][1] = '[')
          and (Pos(']', Formatted[I]) = Length(Formatted[I]))
          then Formatted.Insert(I, '');
        Dec(I);
      end;
      if CompareKeys(Orig, Formatted) <> 0 then
      Formatted.SaveToFile(IniFileName);
    finally
      Orig.Free;
      Formatted.Free;
    end;
  end;
  }
end;

procedure TSimpleIniFile.WriteTStrings(const Section, Key: string; Texts:
  TStrings);
begin
  WriteString(Section, Key, QuotedStr(Texts.CommaText));
end;

procedure TSimpleIniFile.WriteTStrings(const Section: string; Texts: TStrings);
var
  I: Integer;
begin
  EraseSection(Section);
  for I := 0 to Texts.Count - 1 do
    WriteString(Section, 's' + IntToStr(I), Texts[I]);
end;

procedure TSimpleIniFile.ReadTStrings(const Section, Key: string; Texts:
  TStrings);
begin
  Texts.CommaText := ReadString(Section, Key, '');
end;

procedure TSimpleIniFile.ReadTStrings(const Section: string; Texts: TStrings);
var
  Keys: TStrings;
  I: Integer;
begin
  Texts.Clear;
  Keys := TStringList.Create;
  try
    ReadSection(Section, Keys);
    for I := 0 to Keys.Count - 1 do
      Texts.Add(ReadString(Section, Keys[I], ''));
  finally
    Keys.Free;
  end;
end;

procedure TSimpleIniFile.WriteObject(const Section: string; Obj: TObject);
begin
  if Obj is TCheckBox then
  begin
    with Obj as TCheckBox do
    begin
      WriteBool(Section, Name, Checked);
    end;
  end;

  if Obj is TEdit then
  begin
    with Obj as TEdit do
    begin
      if Modified then WriteString(Section, Name, Text);
      Modified := false;
    end;
  end;

  if Obj is TMaskEdit then
  begin
    with Obj as TMaskEdit do
    begin
      if Modified then WriteString(Section, Name, Text);
      Modified := false;
    end;
  end;

  if Obj is TLabeledEdit then
  begin
    with Obj as TLabeledEdit do
    begin
      if Modified then WriteString(Section, Name, Text);
      Modified := false;
    end;
  end;

  if Obj is TComboBox then
  begin
    with Obj as TComboBox do
    begin
      WriteString(Section, Name, Text);
      WriteTStrings(Section, Name + '_list', Items);
    end;
  end;

  if Obj is TRadioGroup then
  begin
    with Obj as TRadioGroup do
    begin
      WriteInteger(Section, 'Index', ItemIndex);
    end;
  end;

end;

procedure TSimpleIniFile.ReadObject(const Section: string; Obj: TObject);
begin
  if Obj is TCheckBox then
  begin
    with Obj as TCheckBox do
    begin
      Checked := ReadBool(Section, Name, false);
    end;
  end;

  if Obj is TEdit then
  begin
    with Obj as TEdit do
    begin
      Text := ReadString(Section, Name, '');
      Modified := false;
    end;
  end;

  if Obj is TMaskEdit then
  begin
    with Obj as TMaskEdit do
    begin
      Text := ReadString(Section, Name, '');
      Modified := false;
    end;
  end;

  if Obj is TLabeledEdit then
  begin
    with Obj as TLabeledEdit do
    begin
      Text := ReadString(Section, Name, '');
      Modified := false;
    end;
  end;

  if Obj is TComboBox then
  begin
    with Obj as TComboBox do
    begin
      Text := ReadString(Section, Name, '');
      ReadTStrings(Section, Name + '_list', Items);
      ItemIndex := Items.Indexof(trim(Text));
    end;
  end;

  if Obj is TRadioGroup then
  begin
    with Obj as TRadioGroup do
    begin
      ItemIndex := ReadInteger(Section, 'Index', 0);
    end;
  end;

end;

//--------------------------------------------------------------
// Procedures without the usage of a class
// The inifile used always resides in the directory where the application starts in

// BinaryStream ----------------------------------------------------

procedure WriteBinaryStreamToIniFile(const Section, Key: string; Value:
  TStream);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteBinaryStream(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadBinaryStreamFromIniFile(const Section, Key: string; Value:
  TStream):
  Integer;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadBinaryStream(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

// Bool ----------------------------------------------------

procedure WriteBoolToIniFile(const Section, Key: string; Value: Boolean);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteBool(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadBoolFromIniFile(const Section, Key: string): Boolean;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadBool(Section, Key, false);
  finally
    IniFile.Free;
  end;
end;

// Date ----------------------------------------------------

procedure WriteDateToIniFile(const Section, Key: string; Value: TDateTime);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteDate(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadDateFromIniFile(const Section, Key: string): TDateTime;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadDate(Section, Key, 0);
  finally
    IniFile.Free;
  end;
end;

// DateTime ----------------------------------------------------

procedure WriteDateTimeToIniFile(const Section, Key: string; Value: TDateTime);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteDateTime(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadDateTimeFromIniFile(const Section, Key: string): TDateTime;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadDateTime(Section, Key, 0);
  finally
    IniFile.Free;
  end;
end;

// Float ----------------------------------------------------

procedure WriteFloatToIniFile(const Section, Key: string; Value: Double);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteFloat(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadFloatFromIniFile(const Section, Key: string): Double;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadFloat(Section, Key, 0.0);
  finally
    IniFile.Free;
  end;
end;

// Integer ----------------------------------------------------

procedure WriteIntegerToIniFile(const Section, Key: string; Value: Integer);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteInteger(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadIntegerFromIniFile(const Section, Key: string): Integer;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadInteger(Section, Key, 0);
  finally
    IniFile.Free;
  end;
end;

// Time ----------------------------------------------------

procedure WriteTimeToIniFile(const Section, Key: string; Value: TDateTime);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteTime(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadTimeFromIniFile(const Section, Key: string): TDateTime;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadTime(Section, Key, 0);
  finally
    IniFile.Free;
  end;
end;

// String ----------------------------------------------------

procedure WriteStringToIniFile(const Section, Key, Value: string);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteString(Section, Key, Value);
  finally
    IniFile.Free;
  end;
end;

function ReadStringFromIniFile(const Section, Key: string): string;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ReadString(Section, Key, '');
  finally
    IniFile.Free;
  end;
end;

// TStrings ----------------------------------------------------

procedure WriteTStringsToIniFile(const Section, Key: string; Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteTStrings(Section, Key, Texts);
  finally
    IniFile.Free;
  end;
end;

procedure ReadTStringsFromIniFile(const Section, Key: string; Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.ReadTStrings(Section, Key, Texts);
  finally
    IniFile.Free;
  end;
end;

procedure WriteTStringsToIniFile(const Section: string; Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteTStrings(Section, Texts);
  finally
    IniFile.Free;
  end;
end;

procedure ReadTStringsFromIniFile(const Section: string; Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  Texts.Clear;
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.ReadTStrings(Section, Texts);
  finally
    IniFile.Free;
  end;
end;

// Objects ----------------------------------------------------------

procedure WriteObjectToIniFile(const Section: string; Obj: TObject);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.WriteObject(Section, Obj);
  finally
    IniFile.Free;
  end;
end;

procedure ReadObjectFromIniFile(const Section: string; Obj: TObject);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.ReadObject(Section, Obj);
  finally
    IniFile.Free;
  end;
end;

// Miscellaneous ----------------------------------------------------

procedure DeleteKeyFromIniFile(const Section, Key: string);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.DeleteKey(Section, Key);
  finally
    IniFile.Free;
  end;
end;

procedure EraseSectionFromIniFile(const Section: string);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.EraseSection(Section);
  finally
    IniFile.Free;
  end;
end;

procedure ReadSectionFromIniFile(const Section: string; Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.ReadSection(Section, Texts);
  finally
    IniFile.Free;
  end;
end;

procedure ReadSectionsFromIniFile(Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.ReadSections(Texts);
  finally
    IniFile.Free;
  end;
end;

procedure ReadSectionValuesFromIniFile(const Section: string; Texts: TStrings);
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    IniFile.ReadSectionValues(Section, Texts);
  finally
    IniFile.Free;
  end;
end;

function SectionExistsInIniFile(const Section: string): Boolean;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.SectionExists(Section);
  finally
    IniFile.Free;
  end;
end;

function ValueExistsInIniFile(const Section, Key: string): Boolean;
var
  IniFile: TSimpleIniFile;
begin
  IniFile := TSimpleIniFile.Create;
  try
    Result := IniFile.ValueExists(Section, Key);
  finally
    IniFile.Free;
  end;
end;

//----------------------------------
initialization
  DefaultIniFileName := ChangeFileExt(IncludeTrailingBackslash(GetCurrentDir) +
    ExtractFileName(ParamStr(0)), '.ini');

end.
