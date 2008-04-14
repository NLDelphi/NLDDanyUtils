unit NLDSimpleRegistry;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use


{ History of this unit:
  27-09-2003: Initial version
  07-10-2003: Adaptions made to meet coding conventions
  29-05-2004: Added functions to read and write TStrings
  30-05-2004: Removed an obsolete TStrings routine.
  17-08-2005: Added routine to read and write multi-sz strings
  23-09-2005: Added the routines of GolezTrol (NLDelphi) to read and write
              WideStrings
  06-02-2006: Added the "Access mode" overload to TSimpleRegistry.Create
}

(*
  This Unit offers the most important functions of the TRegistry unit with the
  following differences:
  1. You do not have to 'open' and 'close' keys any more.
  2. Each function has an extra parameter: the 'Key'. This Key is automatically
     opened and closed to perform the function. The 'Key' is always a full path
     starting from the RootKey.
  3. All 'Read' functions require an extra parameter: the 'Default' Value.
     This is the Value returned if the Key or the Value does not exist.
  4. The 'Write' routines now return a Boolean Value indicating success instead of
     throwing an execption of failiure.
  5. Ability to Read and Write "TStrings".
  6. Ability to Read and Write "Multi-sz" strings.
  7. Ability to read and write some "Object" (like "TEdits") contents. 
*)

(* Example:

  -- Without usage of TSimpleRegistry --

  function TRegistratieForm.readregister_date : integer;
  var TheReg: TRegistry;
  begin
    Result := 0;
    TheReg := TRegistry.Create;
    try
      TheReg.RootKey := HKEY_CURRENT_USER;
      if (TheReg.KeyExists(KeyName)) then
      begin { Key exists }
        if TheReg.OpenKey(KeyName, False)
        then begin
               if TheReg.ValueExists('Reg') then { Value exists }
                  Result := TheReg.ReadInteger('Reg');
               TheReg.Closekey;
             end;
      end;    
    finally
      TheReg.Free;
    end;
  end;

  -- With usage of TSimpleRegistry --

  function TRegistratieForm.readregister_date : integer;
  var TheReg: TSimpleRegistry;
  begin
    Result := 0;
    TheReg := TSimpleRegistry.Create;
    try
      TheReg.RootKey := HKEY_CURRENT_USER;
      Result         := TheReg.ReadInteger(Keyname, 'Reg', 0);
    finally
      TheReg.Free;
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

uses Windows, Registry, Classes;

type
  TSimpleRegistry =
    class
  private
    Reg: TRegistry;
    function GetRoot: HKEY;
    procedure SetRoot(r: HKEY);
    function OpenForRead(const Key, Name: string): Boolean;
  public
    constructor Create; Overload;
    constructor Create(AAccess:LongWord); overload;
    destructor Destroy; override;

    // Root Key
    property RootKey: HKEY read GetRoot write SetRoot;

    // Read functions
    function ReadString(const Key, Name, Default: string): string;
    function ReadBool(const Key, Name: string; Default: Boolean): Boolean;
    function ReadInteger(const Key, Name: string; Default: Integer): Integer;
    function ReadBinaryData(const Key, Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadCurrency(const Key, Name: string; Default: Currency): Currency;
    function ReadDate(const Key, Name: string; Default: TDateTime): TDateTime;
    function ReadDateTime(const Key, Name: string; Default: TDateTime): TDateTime;
    function ReadFloat(const Key, Name: string; Default: Double): Double;
    function ReadTime(const Key, Name: string; Default: TDateTime): TDateTime;
    procedure ReadTStrings(const Key, Name: string; Texts: TStrings);
    procedure ReadMultiString(const Key, Name: string; Texts: TStrings);
    function ReadWideString(const Key, Name: string; Default: WideString = ''): WideString;

    // Write functions
    function WriteString(const Key, Name, value: string): Boolean;
    function WriteExpandString(const Key, Name, value: string): Boolean;
    function WriteBool(const Key, Name: string; value: Boolean): Boolean;
    function WriteInteger(const Key, Name: string; value: Integer): Boolean;
    function WriteBinaryData(const Key, Name: string; var Buffer; BufSize: Integer): Boolean;
    function WriteCurrency(const Key, Name: string; value: Currency): Boolean;
    function WriteDate(const Key, Name: string; value: TDateTime): Boolean;
    function WriteDateTime(const Key, Name: string; value: TDateTime): Boolean;
    function WriteFloat(const Key, Name: string; value: Double): Boolean;
    function WriteTime(const Key, Name: string; value: TDateTime): Boolean;
    function WriteTStrings(const Key, Name: string; Texts: TStrings): Boolean;
    function WriteMultiString(const Key, Name: string; Texts: TStrings): boolean;
    function WriteWideString(const Key, Name: string; value: WideString): Boolean;

    // Miscellaneous functions
    function KeyExists(const Key: string): Boolean;
    function ValueExists(const Key, Name: string): Boolean;
    function DeleteKey(const Key: string): Boolean;
    function DeleteValue(const Key, Name: string): Boolean;
    function CreateKey(const Key: string): Boolean;
    function GetDataInfo(const Key, Name: string; var value: TRegDataInfo): Boolean;
    function GetDataSize(const Key, Name: string): Integer;
    function GetDataType(const Key, Name: string): TRegDataType;
    function GetKeyInfo(const Key: string; var value: TRegKeyInfo): Boolean;
    function GetKeyNames(const Key: string; Strings: TStrings): Boolean;
    function GetValueNames(const Key: string; Strings: TStrings): Boolean;
    function HasSubKeys(const Key: string): Boolean;
    function RenameValue(const Key, OldName, NewName: string): Boolean;
  end;


implementation

uses SysUtils, NLDRcsStrings;

{ ******************************* Create and Destroy ********************************* }

constructor TSimpleRegistry.Create;
begin
  Reg := TRegistry.Create;
end;

constructor TSimpleRegistry.Create(AAccess:LongWord);
begin
  Reg := TRegistry.Create(AAccess);
end;

destructor TSimpleRegistry.Destroy;
begin
  Reg.free;
  Reg := nil;
end;

{ ******************************* Root Key ********************************* }

function TSimpleRegistry.GetRoot: HKEY;
begin
  Result := Reg.RootKey;
end;

procedure TSimpleRegistry.SetRoot(r: HKEY);
begin
  Reg.RootKey := r;
end;

{ **************************** Common functions ****************************** }

function TSimpleRegistry.OpenForRead(const Key, Name: string): Boolean;
begin
  Result := False;
  if Reg.KeyExists(Key) then
  begin
    if Reg.OpenKey(Key, False) then
      Result := Reg.ValueExists(Name);
  end;
end;

{ **************************** Read functions ****************************** }

function TSimpleRegistry.ReadString(const Key, Name, Default: string): string;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadString(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadBool(const Key, Name: string; Default: Boolean):
  Boolean;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadBool(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadInteger(const Key, Name: string; Default: Integer):
  Integer;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadInteger(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadBinaryData(const Key, Name: string; var Buffer;
  BufSize: Integer): Integer;
begin
  try
    Result := 0;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadBinaryData(Name, Buffer, BufSize);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadCurrency(const Key, Name: string; Default:
  Currency): Currency;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadCurrency(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadDate(const Key, Name: string; Default: TDateTime):
  TDateTime;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadDate(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadDateTime(const Key, Name: string; Default:
  TDateTime): TDateTime;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadDateTime(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadFloat(const Key, Name: string; Default: Double):
  Double;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadFloat(Name);
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.ReadTime(const Key, Name: string; Default: TDateTime):
  TDateTime;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
      Result := Reg.ReadTime(Name);
  finally
    Reg.CloseKey;
  end;
end;

procedure TSimpleRegistry.ReadTStrings(const Key, Name: string; Texts: TStrings);
begin
  Texts.CommaText := ReadString(Key, Name, '');
end;

procedure TSimpleRegistry.ReadMultiString(const Key, Name: string; Texts: TStrings);
var Buff: array [0..10000] of Char;
    Number, I : Integer;
    S: String;
begin
  Texts.Clear;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Number := Reg.ReadBinaryData(Name, Buff, 10000);
        for I := 0 to Number - 3 do S := S + Buff[I];
        StringToTStrings(S, Texts, #0);
      except
        on Exception do ;
      end;
    end;
  finally
    Reg.CloseKey;
  end;
end;

(* WideString Routine from GolezTrol (NLDelphi) *)
function TSimpleRegistry.ReadWideString(const Key, Name: string; Default: WideString = ''): WideString;
var
  Size: Integer;
  Res : WideString;
begin
  try
    Result := Default;
    if OpenForRead(Key, Name) then
    begin
      Size := Reg.GetDataSize(Name);
      SetLength(Res, Size div SizeOf(WideChar));
      ZeroMemory(@Res[1], Size);
      Reg.ReadBinaryData(Name, Res[1], Size);
      SetLength(Res, Length(Res)-1);
      Result := Res;
    end;
  finally
    Reg.CloseKey;
  end;
end;


(* Original routine from GolezTrol (NLDelphi)

function RegReadWideStr(const APath, AItem: string; AKey: HKEY = HKEY_CURRENT_USER): WideString;
var
  Size: Integer;
  Res: WideString;
  Reg: TRegistry;
begin
  Res := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey(APath, False) and Reg.ValueExists(AItem) then
    begin
      Size := Reg.GetDataSize(AItem);
      SetLength(Res, Size div SizeOf(WideChar));
      ZeroMemory(@Res[1], Size);
      Reg.ReadBinaryData(AItem, Res[1], Size);
      SetLength(Res, Length(Res)-1);
    end;
  finally
    Reg.Free;
  end;
  Result := Res;
end;
*)

{ **************************** Write functions ****************************** }

function TSimpleRegistry.WriteString(const Key, Name, value: string): Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) {// open (and create if necessary) the Key}
    then
    begin
      try
        Reg.WriteString(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteExpandString(const Key, Name, value: string):
  Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) {// open (and create if necessary) the Key}
    then
    begin
      try
        Reg.WriteExpandString(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteBool(const Key, Name: string; value: Boolean):
  Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteBool(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteInteger(const Key, Name: string; value: Integer):
  Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteInteger(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteBinaryData(const Key, Name: string; var Buffer;
  BufSize: Integer): Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteBinaryData(Name, Buffer, BufSize)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteCurrency(const Key, Name: string; value:
  Currency): Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteCurrency(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteDate(const Key, Name: string; value: TDateTime):
  Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteDate(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteDateTime(const Key, Name: string; value:
  TDateTime): Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteDateTime(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteFloat(const Key, Name: string; value: Double):
  Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteFloat(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteTime(const Key, Name: string; value: TDateTime):
  Boolean;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        Reg.WriteTime(Name, value)
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.WriteTStrings(const Key, Name: string; Texts: TStrings): Boolean;
begin
  Result := WriteString(Key, Name, Texts.CommaText);
end;

function TSimpleRegistry.WriteMultiString(const Key, Name: string; Texts: TStrings): boolean;
var  RtKey, RegKey: HKEY;
     Buff, P : ^Char;
     S : String;
     I : Integer;
begin
  Result := false;
  RtKey := Rootkey;

  S := '';
  for I := 0 to Texts.Count - 1 do S := S + Texts[I] + #0;
  S := S + #0;

  GetMem(Buff, Length(S));
  P := Buff;
  for I := 1 to Length(S) do
  begin
    P^:= S[I];
    Inc(P);
  end;

  if RegOpenKeyEx(RtKey, PChar(Key), 0, KEY_WRITE, RegKey) = ERROR_SUCCESS then
    try
      if RegSetValueEx(RegKey, PChar(Name), 0, REG_MULTI_SZ, Buff, Length(S)) = ERROR_SUCCESS
      then Result := True;
    finally
      RegCloseKey(RegKey);
      FreeMem(Buff, Length(S));
    end
  else Result := false;
end;

(* WideString Routine from GolezTrol (NLDelphi) *)
function TSimpleRegistry.WriteWideString(const Key, Name: string; value: WideString): Boolean;
var S: WideString;
begin
  Result := True;
  try
    if Reg.OpenKey(Key, True) then
    begin
      try
        S := Value + #0;
        Reg.WriteBinaryData(Name, s[1], (Length(s)) * SizeOf(WideChar));
      except
        on Exception do
          Result := False;
      end;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

(* Original routine from GolezTrol (NLDelphi)

procedure RegWriteWideStr(const APath, AItem: string; const AValue: WideString; AKey: HKEY = HKEY_CURRENT_USER);
var
  Reg: TRegistry;
  s: WideString;
begin
  Reg := TRegistry.Create;
  try
    s := AValue + #0;
    if Reg.OpenKey(APath, True) then
      Reg.WriteBinaryData(AItem, s[1], (Length(s)) * SizeOf(WideChar));
  finally
    Reg.Free;
  end;
end;
*)


{ *********************** Miscellaneous functions ************************* }

function TSimpleRegistry.KeyExists(const Key: string): Boolean;
begin
  Result := Reg.KeyExists(Key);
end;

function TSimpleRegistry.ValueExists(const Key, Name: string): Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
      Result := Reg.ValueExists(Name)
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.DeleteKey(const Key: string): Boolean;
begin
  Result := Reg.DeleteKey(Key);
end;

function TSimpleRegistry.DeleteValue(const Key, Name: string): Boolean;
begin
  Result := False;
  try
    if Reg.KeyExists(Key) then
    begin
      if Reg.OpenKey(Key, False) then
      begin
        Result := Reg.DeleteValue(Name);
      end;
    end;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.CreateKey(const Key: string): Boolean;
var
  TempKey: string;
begin
  TempKey := Key;
  if TempKey[1] <> '\' then
    TempKey := '\' + TempKey;
  Result := Reg.CreateKey(TempKey);
end;

function TSimpleRegistry.GetDataInfo(const Key, Name: string; var value:
  TRegDataInfo): Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
    begin
      if Reg.ValueExists(Name) then
        Result := Reg.GetDataInfo(Name, value)
      else
        Result := False;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.GetDataSize(const Key, Name: string): Integer;
begin
  try
    if Reg.OpenKey(Key, False) then
    begin
      if Reg.ValueExists(Name) then
        Result := Reg.GetDataSize(Name)
      else
        Result := 0;
    end
    else
      Result := 0;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.GetDataType(const Key, Name: string): TRegDataType;
begin
  try
    if Reg.OpenKey(Key, False) then
    begin
      if Reg.ValueExists(Name) then
        Result := Reg.GetDataType(Name)
      else
        Result := rdUnknown;
    end
    else
      Result := rdUnknown;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.GetKeyInfo(const Key: string; var value: TRegKeyInfo):
  Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
      Result := Reg.GetKeyInfo(value)
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.GetKeyNames(const Key: string; Strings: TStrings):
  Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
    begin
      Reg.GetKeyNames(Strings);
      Result := True;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.GetValueNames(const Key: string; Strings: TStrings):
  Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
    begin
      Reg.GetValueNames(Strings);
      Result := True;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.HasSubKeys(const Key: string): Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
      Result := Reg.HasSubKeys
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

function TSimpleRegistry.RenameValue(const Key, OldName, NewName: string):
  Boolean;
begin
  try
    if Reg.OpenKey(Key, False) then
    begin
      Reg.RenameValue(OldName, NewName);
      Result := True;
    end
    else
      Result := False;
  finally
    Reg.CloseKey;
  end;
end;

end.




