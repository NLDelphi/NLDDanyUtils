unit NLDRcsStrings;

// Dany Rosseel

{-$DEFINE NoDebug}                                 // Disable debug possibilities and range checking (= faster)
// {$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{.$Define FastStrings}// Usage of the FastStrings version of PosN
                       // {$Define ...}  = Use the FastStrings version
                       // {.$Define ...} = Use the non FastStrings version

{ History of this unit:
06-10-2003: * Renamed the unit to 'RcsStrings' (was 'Strings')
            * Removed all unnecessary functions due to available in the RTL (now)
            * Made all functions work with strings in stead of shortstrings
            * Renamed functions and variables.. to meet naming conventions
05-02-2004: * Added the funtion 'StringIsIn'
16-05-2004: * Removed the functions 'LeftString', 'MidString' and 'RightString'
            * Added the procedures "StringToTStrings" and "TStringsToString"
29-05-2004: * Removed an error from the "TStringsToString" function.
30-05-2004: * Added the procedure 'TrimTStrings'
            * Removed the trimming from "TStringsToString" and "StringToTStrings"
06-06-2004: * Extended the procedure 'TrimTStrings' with a 'mode':
              It is possible now to trim, lefttrim, righttrim, toptrim, bottomtrim
              the TStrings and remove all empty lines by choosing the option settings.
12-06-2004: * Added the functions "BeginString", "EndString", "SplitString", "CopyString"
              "DeleteString" and "PosN". Most of the routines are overloaded to accept
              different parameters.
13-06-2004: * Optimized the "PosN" function, also removed an error (thanks D7EE!)
14-06-2004: * Optimized again the "PosN" function. Thanks "marcov".
            * Optimized "DeleteString" (the version with tags)
16-06-2004: * Added the "DeleteStringProc" (a procedure in stead of a function,
              similar like the "Delete" procedure)
            * Gone back to the non "StrPos" version of PosN. Reason: the handling of #0.
03-07-2004: * Added a "FastStrings" version of "PosN"
            * Speed Optimized "DeleteString" and "DeleteStringProc" (the "tags" version)
04-07-2004: * Optimized the non FastStrings version of PosN: used a very simple
              Boyer-Moore algorithm (only the part of it that i did understand)
05-07-2004: * Made the implementation of the "PosN" function more simple
10-07-2004: * Made an overloaded version of PosN: it accepts a skip table as parameter.
              This allows to make a skip table only once if more searches with the same
              search pattern are needed. Faster than the regular PosN.
11-07-2004: * Extended the "StringToTStrings", "TStringsToString" and the "StringIsIn"
              routines with the (defaulted to ',') parameter "Separator".
26-07-2004: * Added the function "ExcludeTrailingCrLf".
02-08-2004: * Added the function "MakeCommandLine".
10-10-2004: * Added the functions "QuotedString" and "UnquotedString"
25-01-2005: * The "StringIsIn" function now returns "True" if both strings are empty
06-02-2005: * Added function "ShortenedString".
09-02-2005: * Made "FillString" a lot faster.
10-02-2005: * Made "CenterString" a lot faster.
11-02-2005: * PosN can handle now a startindex outside the target string.
12-02-2005: * Made "TrimTStrings" with "trsTop" as mode much more faster.
19-02-2005: * Added "BeginUpdate" and "EndUpdate" to "TrimTStrings"
27-02-2005: * Added "TStringsToCSV" and "CSVToTStrings"
08-03-2005: * Made "PosN" a little bit more efficient with 1 character substrings
19-03-2005: * Added "CompareKeys", a procedure to compare 2 key groups of e.g. records to
              be sorted.
31-10-2005: * Removed the "$P+" and the "$H+" directives (OpenStrings and LongStrings on)
11-04-2006: * Removed all "const" qualifiers for string parameters of functions that return a string.
              Done to avoid problems with expressions like "AString := Fx(AString)"
            * Added the "$P+" and the "$H+" directives again
            * added the "$X+" directive (extended syntax)
19-06-2006: * Added "LeftPad" and "RightPad" functions
27-08-2006: * Added the "TRcsStrings" class with the following procedures added:
              "LoadFromFileInclude" which handles "$I ..." lines as Deplhi does.
              "ToCSV":       functions as "TStringsToCSV"
              "FromCSV":     functions as "CSVToTStrings"
              "ToString":    functions as "TStringsToString"
              "FromString":  functions as "StringToTStrings"
              "TrimStrings": functions as "TrimTStrings"
03-09-2006:  * Adapted "CompareKeys" as suggested by GolezTrol in
               http://www.nldelphi.com/Forum/showpost.php?p=162754&postcount=16
               The arrays can be of different length now.
19-11-2006:  * Added an overloaded version of "CompareKeys"for usage with TStrings.
03-12-2006:  * Made procedure "AddFromFileInclude" of "TRcsStrings" public.
22-07-2007:  * Added the function "FloatToString"
01-08-2007:  * Added the function "IsNumber"
16-02-2008:  * Added the following weeknumber related routines:
		- DateToWeekNo
		- WeekNoToDate
	       WeekNo is a string of the format "yyww.d"
20-07-2008:  * Added the function "NumericalString"
18-01-2011:  * Had to use the old version of fillstring again, the new one did not work under Delphi XE (widestrings)
             * Also went back to older versions of functions that used "CopyMemory"
}

{$X+}                                             // Extended Syntax
{$P+}                                             // Open Parameters
{$H+}                                             // Huge strings

{$IFDEF NoDebug}

{$O+}                                             // Optimisation ON
{$D-}                                             // Debug information OFF
{$I-}                                             // I/O checking OFF
{$L-}                                             // Local Symbols OFF
{$Q-}                                             // Overflow Checking OFF
{$R-}                                             // Range Checking OFF

{$ELSE}
{$O-}                                             // Optimisation OFF
{$D+}                                             // Debug information ON
{$I+}                                             // I/O checking ON
{$L+}                                             // Local Symbols ON
{$Q+}                                             // Overflow Checking ON
{$R+}                                             // Range Checking ON

{$ENDIF}

{$W-}                                             // Stack Frames OFF
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

interface

uses Classes,
  Windows;

type
  TrimTStringsMode = (trsTrim, trsLeft, trsRight, trsTop, trsBottom,
    trsEmptyLines);
  TrimTStringsModes = set of TrimTStringsMode;
  // Any combination of the following values can be used as "Modes":
  // trsTrim:  executes a "Trim" on every string in the stringlist
  // trsLeft:  executes a "TrimLeft" on every string in the stringlist
  // trsRight: executes a "TrimRight" on every string in the stringlist
  // trsTop:   deletes all empty strings (after trimming) at the top of the TStringList
  // trsBottom: deletes all empty strings (after trimming) at the bottom of the TStringList
  // trsEmptyLines: deletes all empty strings (after trimming)

  CenterMode = (cmLeft, cmMid, cmRight);

  TRcsStrings = class(TStringList)
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFileInclude(Filename: string);
    procedure AddFromFileInclude(FileName: string);
    function ToCSV: string;
    procedure FromCSV(const CSV: string);
    function ToString(const Separator: Char = ','): string;
    procedure FromString(const S: string; const Separator: Char = ',');
    procedure TrimStrings(const Modes: TrimTStringsModes = [trsTrim]);
  end;

function CompareKeys(KeysA: array of string; KeysB: array of string): integer; overload;
// First KeysA[0] is compared with KeysB[0]; if they are the same then
// KeysA[1] is compared with KeysB[1].. until a difference is encountered
// or the last string in each "keys" is processed.
// The function returns
//   zero if both key groups are entirely the same (A=B),
//   +1 if A > B, or -1 if B > A

function CompareKeys(KeysA: TStrings; KeysB: TStrings): integer; overload;
// See above function

function TStringsToCSV(T: TStrings): string;
// Returns a "comma separated value" string with stringlist "T" as contents

procedure CSVToTStrings(const CSV: string; T: TStrings);
// Converts a "comma separated value" string into in stringlist "T"

function ShortenedString(S: string; const L: Integer): string;
// Returns S if length(S) <= L; else S is returned shortened with dots (3) in the middle; the
// total length of the returned string is then L

function QuotedString(S: string; const Conditionally: Boolean = false): string;
// Returns S with leading and trailing double quotes added. If Conditionally is true,
// the double quotes are added only if the string contains spaces or is empty.

function UnquotedString(S: string): string;
// Returns S without its leading and trailing double quotes.

function MakeCommandLine(const Cmds: array of string): string;
// Makes a command line out of the command and parameters in "Cmds":
// surrounds command and parameters which contain spaces with double quotes

function ExcludeTrailingCrLf(S: string): string;
// Removes the trailing CR or LF or CRLF sequence at the end of a string
// after "Right" trimming first.

procedure StringToTStrings(const S: string; T: TStrings; const Separator: Char = ',');
// StringToTStrings converts a separator delimited string into a Stringlist

function TStringsToString(T: TStrings; const Separator: Char = ','): string;
// TStringsToString converts a Stringlist into a separator delimited string

procedure TrimTStrings(T: TStrings; const Modes: TrimTStringsModes = [trsTrim]);
// see type "TrimTStringsModes" above

function BeginString(Source: string; const P: Integer): string; overload;
// BeginString returns the part (of Source) left of position P

function BeginString(Source, Tag: string): string; overload;
// BeginString returns the part (of Source) before the Tag
// If the Tag is not found, an empty string is returned
// Only the first occurance of Tag in Source is taken into account

function EndString(Source: string; const P: Integer): string; overload;
// EndString returns the part (of Source) from position P onwards (the character on
// position P included

function EndString(Source, Tag: string): string; overload;
// EndString returns the part (of Source) after the Tag
// If the Tag is not found, an empty string is returned
// Only the first occurance of Tag in Source is taken into account

procedure SplitString(Source: string; const P: Integer; var Left, Right:
  string); overload;
// Returns in Left and Right respectively the "BeginString" and the "EndString"
// See the two functions above

procedure SplitString(Source: string; const Tag: string; var Left, Right:
  string);
overload;
// Returns in Left and Right respectively the "BeginString" and the "EndString"
// See the two functions above

function CopyString(Source: string; const Start, Stop: Integer): string;
overload;
// Returns the part of Source from position Start to Position Stop, the characters on
// both positions included

function CopyString(Source: string; const Tag1, Tag2: string): string;
overload;
// Returns the part of the Source between Tag1 and Tag2

function DeleteString(Source: string; const Start, Stop: Integer): string;
overload;
// Returns the Source with the part deleted from the Start to and including the Stop position

function DeleteString(Source, Tag1, Tag2: string; const All: Boolean =
  false): string; overload;
// Returns Source with the text between the 2 tags deleted (also the tags
// themselves are deleted). If "All" is true, then all occurances are deleted, if false only
// the first one is deleted. If Tag1 or Tag2 is not present, the Source is returned unchanged

procedure DeleteStringProc(var Source: string; const Start, Stop: Integer);
overload;
// Returns the Source with the part deleted from the Start to and including the Stop position

procedure DeleteStringProc(var Source: string; const Tag1, Tag2: string; const All:
  Boolean =
  false); overload;
// Returns Source with the text between the 2 tags deleted (also the tags
// themselves are deleted). If "All" is true, then all occurances are deleted, if false only
// the first one is deleted. If Tag1 or Tag2 is not present, the Source is returned unchanged

type
  TPosNSkipTable = array[Char] of Integer;

function PosN(const SubStr, Source: string; const P: Integer = 1): Integer; overload;
// Same as the "Pos" function, but the search starts at position P (1.. length(Source))

function PosN(const SubStr, Source: string; const Stable: TPosNSkipTable; const P:
  Integer = 1): Integer; overload;
// Same as the "PosN" function but with an external SkipTable passed as parameter
// Execute "PosNInit" (see below) to create the skiptable before calling this function!

procedure PosNInit(var Table: TPosNSkipTable; const SubStr: string);
// Initialises the PosNSkipTable before usage of the above version of PosN

procedure FillString(const SubString: string; var S: string; Position: Word);
// Fillstring returns "S" with "SubString" at place "Position" in it.
// The original characters as Position "Position" in "S" are overwritten.
// The Length of the string "S" is adapted if necessary.

function CenterString(S: string; const Mode: CenterMode; const Len: Word): string;
// Centerstring returns "S" placed in a string of Length "Len" at the
// place (cmLeft, cmMiddle or cmRight) defined by "CenterMode"

function SpaceString(const Len: Word): string;
// SpaceString returns a string of Length "Len" containing all spaces

function DayMonthYearString: string;
// DayMonthYearString returns a string containing the date of today in the
// format: dd-mm-yy (e.g. 20-12-49 for Dec 20th 1949)

function YYMMDDString: string;
// YYMMDDString returns a string containing the date of today in the
// format: yymmdd (e.g. 491220 for Dec 20th 1949)

function YYYYMMDDString: string;
// YYYYMMDDString returns a string containing the date of today in the
// format: yyyymmdd (e.g. 19491220 for Dec 20th 1949)

function TimeString: string;                      {hr:min:sec}
// TimeString rerurns the current time in the
// format: hh:mm:ss (e.g. 23:50:55 for 23 hrs, 50 minutes and 55 secs)

function IntString(const I: Longint; const Len: Byte): string;
// IntString returns a string of Length "Len",
// representing "I" right aligned

function StringIsIn(S1, S2: string; const Separator: Char = ','): Boolean;
// StringIsIn returns true if S1 is in the "Separator separated" strings in S2

function BinString(const B: Byte): string;
// BinString returns a string with the binary representation of "B".
// The two nibbles are separated with an underscore

function LeftPad(const S: string; Len: integer; Ch: Char = ' '): string;
// Generates a string consisting of "S" left padded with characters "Ch"
// till length "Len" is reached.

function RightPad(const S: string; Len: integer; Ch: Char = ' '): string;
// Generates a string consisting of "S" right padded with characters "Ch"
// till length "Len" is reached.

function FloatToString(const V: Real): string;
// FloatToString converts the floating-point value given by V to its string representation.
// The conversion uses general number format with 15 significant digits.
// The scientific notation ("E") is never used.

function IsNumber(S: string): boolean;
// IsNumber checks if S is a valid number (allows floating point non scientific notation)

function DateToWeekNo(Dat: string): string;
// Returns the weeknumber of date Dat. The format of the result is "yymm.d".
// "d" is the number of the day, 1 = monday
// only valid from year 2000 till 2099

function WeekNoToDate(WeekNo: string): string;
// Returns the date corresponding to the weeknumber "WeekNo"
// only valid from year 2000 till 2099

function NumericalString(S: string): string;
// Returns the argument with all non numerical characters removed

implementation

uses SysUtils, DateUtils
  ,
  StrUtils
{$IFDEF FastStrings}
  ,
  FastStrings
{$ENDIF}
  ;

constructor TRcsStrings.Create;
begin
  inherited Create;
end;

destructor TRcsStrings.Destroy;
begin
  inherited Destroy;
end;

procedure TRcsStrings.AddFromFileInclude(FileName: string);
var
  F: TextFile;
  L: string;
begin
  if FileExists(Filename) then
  begin
    AssignFile(F, FileName);
    Reset(F);
    while not Eof(F) do
    begin
      ReadLn(F, L);
      if LeftStr(L, 3) = '$I '
        then AddFromFileInclude(Trim(MidStr(L, 4, Length(L))))
      else Self.Add(L);
    end;
    CloseFile(F);
  end;
end;

procedure TRcsStrings.LoadFromFileInclude(Filename: string);
begin
  Self.Clear;
  AddFromFileInclude(FileName);
end;

function TRcsStrings.ToCSV: string;
begin
  Result := TStringsToCSV(Self);
end;

procedure TRcsStrings.FromCSV(const CSV: string);
begin
  CSVToTStrings(CSV, Self);
end;

function TRcsStrings.ToString(const Separator: Char = ','): string;
begin
  TStringsToString(Self, Separator);
end;

procedure TRcsStrings.FromString(const S: string; const Separator: Char = ',');
begin
  StringToTStrings(S, Self, Separator);
end;

procedure TRcsStrings.TrimStrings(const Modes: TrimTStringsModes = [trsTrim]);
begin
  TrimTStrings(Self, Modes);
end;

function CompareKeys(KeysA: array of string; KeysB: array of string): integer;
var
  I: Integer;
  S1, S2: string;
begin
  Result := 0;

  I := 0;
  while (I < Length(KeysA)) and (I < Length(KeysB)) do
  begin
    if I < Length(KeysA)
      then S1 := KeysA[I]
    else S1 := '';

    if I < Length(KeysB)
      then S2 := KeysB[I]
    else S2 := '';

    if S1 > S2 then
    begin
      Result := +1;
      Exit;
    end else
      if S1 < S2 then
      begin
        Result := -1;
        Exit;
      end;
    Inc(I);
  end;
end;

function CompareKeys(KeysA: TStrings; KeysB: TStrings): integer; overload;
var
  I: Integer;
  S1, S2: string;
begin
  Result := 0;

  I := 0;
  while (I < KeysA.Count) and (I < KeysB.Count) do
  begin
    if I < KeysA.Count
      then S1 := KeysA[I]
    else S1 := '';

    if I < KeysB.Count
      then S2 := KeysB[I]
    else S2 := '';

    if S1 > S2 then
    begin
      Result := +1;
      Exit;
    end else
      if S1 < S2 then
      begin
        Result := -1;
        Exit;
      end;
    Inc(I);
  end;
end;


(*
function TStringsToCSV(T: TStrings): string;
begin
  Result := T.Text;
end;
*)

function TStringsToCSV(T: TStrings): string;
var
  I: Integer;
  TmpS: string;
begin
  Result := '';
  for I := 0 to T.Count - 1 do
  begin
    TmpS := T[I];
    if TmpS > '' then
    begin
      // make double quotes from singlequotes
      TmpS := StringReplace(TmpS, '"', '""', [rfReplaceAll]);

      // add beginning and end quotes if necessary/desirable
      if (Pos(' ', TmpS) > 0) or                  // space
        (Pos(#9, TmpS) > 0) or                    // tab
        (Pos(',', TmpS) > 0) or                   // comma
        (Pos('"', TmpS) > 0) or                   // quote
        (Pos(#13, TmpS) > 0) or                   // CR
        (Pos(#10, TmpS) > 0)                      // LF
        then TmpS := '"' + TmpS + '"';            // Add double quotes
    end;
    Result := Result + TmpS;
    if I < T.Count - 1 then Result := Result + ',';
  end;
end;

procedure CSVToTStrings(const CSV: string; T: TStrings);
var
  I, J: Integer;
  TmpS: string;
  InsideQuotes: Boolean;
  Started: Boolean;
begin
  T.Clear;
  if trim(CSV) = '' then Exit;

  InsideQuotes := false;
  Started := false;
  TmpS := '';
  I := 1;

  while I <= Length(CSV) do
  begin
    if CSV[I] = ',' then
    begin                                         // comma detected
      if InsideQuotes then
      begin                                       // the comma is normal text
        TmpS := TmpS + CSV[I];
        Started := true;
      end
      else
      begin                                       // the comma indicates the end of a field
        T.Add(TmpS);                              // add to stringlist
        TmpS := '';                               // new string to be started
        InsideQuotes := false;
        Started := false;
      end;
    end
    else
      if CSV[I] = '"' then
      begin                                       // quote detected
        if (I = Length(CSV)) or (CSV[I + 1] <> '"') then
        begin                                     // single quote detected
          InsideQuotes := not InsideQuotes;
          Started := true;
        end
        else
        begin                                     // double quote detected, store one quote in the string and
          TmpS := TmpS + CSV[I];
          Started := true;
          Inc(I);                                 // ignore the next one
        end;
      end
      else
      begin                                       // other character than comma or quote.
        if InsideQuotes then
          TmpS := TmpS + CSV[I]
        else
          if ((CSV[I] <> ' ') and (CSV[I] <> #9)) then
          begin
            TmpS := TmpS + CSV[I];
            Started := true;
          end
          else
          begin                                   // character is space or tab outside quotes
            if Started then
            begin
          // find out if the next character is a comma or not
          // if it is (or the CVS string is at the end), ignore the space or tab
              J := I;
              while (J <= Length(CSV)) and ((CSV[J] = ' ') or (CSV[J] = #9)) do
                Inc(J);
              if (J <= Length(CSV)) and (CSV[J] <> ',') then TmpS := TmpS + CSV[I];
            end
            else
              ;                                   // not started yet, ignore leading white space
          end;
      end;
    Inc(I);                                       // get next character in the CSV string
  end;
  if (TmpS > '') or                               // there is already a string content assembled
    (RightStr(TRim(CSV), 1) = ',') then T.Add(TmpS); // the last character is a comma
end;

function ShortenedString(S: string; const L: Integer): string;
begin
  if length(S) <= L then
    Result := S
  else
    Result := LeftStr(S, (L div 2) - 2) + '...' + RightStr(S, (L div 2) - 1);
end;

function UnquotedString(S: string): string;
begin
  Result := S;
  while (length(Result) > 0) and (Result[1] = '"') do
    Delete(Result, 1, 1);
  while (length(Result) > 0) and (Result[length(Result)] = '"') do
    Delete(Result, length(Result), 1);
end;

function QuotedString(S: string; const Conditionally: Boolean = false): string;
begin
  Result := UnquotedString(S);
  if (not conditionally) or
    ((Pos(' ', Result) > 0) or (Result = '')) then Result := '"' + Result + '"';
end;

function MakeCommandLine(const Cmds: array of string): string;
var
  I: Integer;
  Tmp: string;
begin
  Result := '';
  for i := Low(Cmds) to High(Cmds) do
  begin
    Tmp := UnquotedString(Trim(Cmds[I]));
    Tmp := QuotedString(Tmp, true);
    Result := Result + Tmp + ' ';
  end;
  Result := Trim(Result);
end;

function ExcludeTrailingCrLf(S: string): string;
begin
  Result := TrimRight(S);                         // do "Right" Trim first
  if Result = '' then Exit;
  while (Result[Length(Result)] = #10) or         // line feed
  (Result[Length(Result)] = #13) do               // carriage return
    Delete(Result, Length(Result), 1);
end;

procedure StringToTStrings(const S: string; T: TStrings; const Separator: Char = ',');
var
  P, StartPos: Integer;
begin
  T.Clear;
  if Length(S) = 0 then Exit;

  StartPos := 1;
  P := PosN(Separator, S, Startpos);
  while P > 0 do
  begin
    T.Add(Copy(S, StartPos, P - StartPos));
    StartPos := P + 1;
    P := PosN(Separator, S, StartPos);
  end;
  if P = 0 then T.Add(Copy(S, StartPos, MaxInt));
end;

{ Old Version
procedure StringToTStrings(S: string; T: TStrings; const Separator: Char = ',');
var
  P: Integer;
begin
  T.Clear;
  P := PosN(Separator, S);
  while P > 0 do
  begin
    T.Add(copy(S, 1, P - 1));
    Delete(S, 1, P);
    P := PosN(Separator, S);
  end;
  if S > '' then T.Add(S);
end;
}

function TStringsToString(T: TStrings; const Separator: Char = ','): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to T.Count - 1 do
    Result := Result + Separator + T[I];
  if length(Result) > 0 then delete(Result, 1, 1);
end;

procedure TrimTStrings(T: TStrings; const Modes: TrimTStringsModes);
var
  I: Integer;
begin

  if T.Count = 0 then exit;

  T.BeginUpdate;

  if trsTop in Modes then                         // trim the top of the TStringList
  begin
    for I := 0 to ((T.Count div 2) - 1) do
      T.Exchange(I, T.Count - 1 - I);
    while (T.Count > 0) and (trim(T[T.Count - 1]) = '') do
      T.Delete(T.Count - 1);
    for I := 0 to ((T.Count div 2) - 1) do
      T.Exchange(I, T.Count - 1 - I);
  end;

  (* Old Version
     if trsTop in Modes then // trim the top of the TStringList
      while (T.Count > 0) and (trim(T[0]) = '') do
        T.Delete(0);
  *)

  if trsBottom in Modes then                      // trim the bottom of the TStringList
    while (T.Count > 0) and (trim(T[T.Count - 1]) = '') do
      T.Delete(T.Count - 1);

  if trsEmptyLines in Modes then                  // remove all "empty" lines (after trimming)
  begin
    I := T.Count - 1;
    while I >= 0 do
    begin
      if trim(T[I]) = '' then T.Delete(I);
      dec(I);
    end;
  end;

  if (not (trsTrim in Modes)) and (not (trsLeft in Modes)) and
    (not (trsRight in Modes)) then
  begin
    T.EndUpdate;
    exit;                                         // no more trimming needed
  end;

  for I := 0 to T.Count - 1 do                    // trim all lines
  begin
    begin
      if trsTrim in Modes then
        T[I] := Trim(T[I])
      else
      begin
        if trsLeft in Modes then T[I] := TrimLeft(T[I]);
        if trsRight in Modes then T[I] := TrimRight(T[I]);
      end;
    end;
  end;

  T.EndUpdate;

end;

function BeginString(Source: string; const P: Integer): string;
begin
  Result := LeftStr(Source, P - 1);
end;

function BeginString(Source, Tag: string): string;
var
  P: Integer;
begin
  Result := '';
  P := PosN(Tag, Source);
  if P < 1 then Exit;
  Result := LeftStr(Source, P - 1);
end;

function EndString(Source: string; const P: Integer): string;
begin
  Result := RightStr(Source, Length(Source) - P + 1);
end;

function EndString(Source, Tag: string): string;
var
  P: Integer;
begin
  Result := '';
  P := PosN(Tag, Source);
  if P < 1 then Exit;
  Result := Copy(Source, P + length(Tag), MaxInt);
end;

procedure SplitString(Source: string; const P: Integer; var Left, Right:
  string);
begin
  Left := LeftStr(Source, P - 1);                 // BeginString
  Right := RightStr(Source, Length(Source) - P + 1); // EndString
end;

procedure SplitString(Source: string; const Tag: string; var Left, Right:
  string);
var
  P: Integer;
begin
  Left := '';
  Right := '';
  P := PosN(Tag, Source);
  if P < 1 then Exit;
  Left := Copy(Source, 1, P - 1);
  Right := Copy(Source, P + length(Tag), MaxInt);
end;

function CopyString(Source: string; const Start, Stop: Integer): string;
begin
  Result := Copy(Source, Start, Stop - Start + 1);
end;

function CopyString(Source: string; const Tag1, Tag2: string): string;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := PosN(Tag1, Source);
  P2 := PosN(Tag2, Source, P1 + 1);
  if (P1 < 1) or (P2 < 1) then exit;
  Result := CopyString(Source, P1 + length(Tag1), P2 - 1);
end;

function DeleteString(Source: string; const Start, Stop: Integer): string;
begin
  Result := Source;
  Delete(Result, Start, Stop - Start + 1);
end;

{
function DeleteString(Source, Tag1, Tag2: string; const All: Boolean = false): string; overload;
var
  P1, P2, Lt2: Integer;
  P, NewLen, L: Integer;
  T1, T2: TPosNSkipTable;
begin
  Result := Source;                               // default result
  Lt2 := Length(Tag2);

  PosNInit(T1, Tag1);
  PosNInit(T2, Tag2);

  if not All then
  begin
    P1 := PosN(Tag1, Source, T1);
    P2 := PosN(Tag2, Source, T2, P1 + 1);
    if (P1 < 1) or (P2 < 1) then Exit;
    Delete(Result, P1, (P2 + Lt2) - P1);
  end

  else                                            // delete All occurances
  begin
    NewLen := 0;                                  // new length of Result
    P := 1;                                       // position to copy from in the source;

    P1 := PosN(Tag1, Source, T1);
    while (P1 > 0) do
    begin
      P2 := PosN(Tag2, Source, T2, P1 + 1);
      if P2 < 1 then break;

      L := P1 - P;                                // number of bytes to copy
      CopyMemory(@Result[NewLen + 1], @Source[P], L);
      NewLen := NewLen + L;                       // new length of the result

      P := P2 + Lt2;                              // new position to copy from in the source
      // (P points to AFTER the part to delete)
      P1 := PosN(Tag1, Source, T1, P);
    end;

    if P = 1 then exit;

    L := Length(Source) - P + 1;
    if L > 0 then
    begin
      CopyMemory(@Result[NewLen + 1], @Source[P], L);
      NewLen := NewLen + L;
    end;

    SetLength(Result, NewLen);
  end;
end;
}


// Original version
function DeleteString(Source, Tag1, Tag2: string; const All: Boolean = false): string; overload;
var
  P1, P2, Lt2: Integer;
begin
  Result := Source;
  P1 := PosN(Tag1, Result);
  P2 := PosN(Tag2, Result, P1 + 1);
  if (P1 < 1) or (P2 < 1) then exit;

  Lt2 := Length(Tag2);

  Delete(Result, P1, (P2 + Lt2) - P1 );

  if All then
  begin
    P1 := PosN(Tag1, Result, P1);
    while (P1 > 0) do
    begin
      P2 := PosN(Tag2, Result, P1 + 1);
      if P2 = 0 then break;
      Delete(Result, P1, (P2 + Lt2) - P1 );
      P1 := PosN(Tag1, Result, P1);
    end;
  end;
end;

procedure DeleteStringProc(var Source: string; const Start, Stop: Integer);
begin
  Delete(Source, Start, Stop - Start + 1);
end;

{
procedure DeleteStringProc(var Source: string; const Tag1, Tag2: string; const All:
  Boolean =
  false);
var
  P1, P2, Lt2: Integer;
  P, NewLen, L: Integer;
  Tmp: string;
  T1, T2: TPosNSkipTable;
begin
  Tmp := Source;                                  // default result
  Lt2 := Length(Tag2);

  PosNInit(T1, Tag1);
  PosNInit(T2, Tag2);

  if not All then
  begin
    P1 := PosN(Tag1, Tmp, T1);
    P2 := PosN(Tag2, Tmp, T2, P1 + 1);
    if (P1 < 1) or (P2 < 1) then Exit;
    Delete(Tmp, P1, (P2 + Lt2) - P1);
  end

  else                                            // delete All occurances
  begin
    NewLen := 0;                                  // new length of Result
    P := 1;                                       // position to copy from in the source;

    P1 := PosN(Tag1, Source, T1);
    while (P1 > 0) do
    begin
      P2 := PosN(Tag2, Source, T2, P1 + 1);
      if P2 < 1 then break;

      L := P1 - P;                                // number of bytes to copy
      CopyMemory(@Tmp[NewLen + 1], @Source[P], L);
      NewLen := NewLen + L;                       // new length of the result

      P := P2 + Lt2;                              // new position to copy from in the source
      // (P points to AFTER the part to delete)
      P1 := PosN(Tag1, Source, T1, P);
    end;

    if P = 1 then
    begin
      Source := Tmp;
      Exit;
    end;

    L := Length(Source) - P + 1;
    if L > 0 then
    begin
      CopyMemory(@Tmp[NewLen + 1], @Source[P], L);
      NewLen := NewLen + L;
    end;

    SetLength(Tmp, NewLen);
  end;

  Source := Tmp;                                  // give back result
end;
}

 // Original version
procedure DeleteStringProc(var Source: string; const Tag1, Tag2: string; const All: Boolean = false);
var
  P1, P2, L: Integer;
begin
  P1 := PosN(Tag1, Source);
  P2 := PosN(Tag2, Source, P1 + 1);
  if (P1 < 1) or (P2 < 1) then exit;

  L := Length(Tag2);
  Delete(Source, P1, P2 - P1 + L);

  if All then
  begin
    P1 := PosN(Tag1, Source, P1);
    P2 := PosN(Tag2, Source, P1 + 1);
    while (P1 > 0) and (P2 > 0) do
    begin
      Delete(Source, P1, P2 - P1 + L);
      P1 := PosN(Tag1, Source, P1);
      P2 := PosN(Tag2, Source, P1 + 1);
    end;
  end;
end;


{$IFDEF FastStrings}

function PosN(const SubStr, Source: string; const P: Integer = 1): Integer;
begin
  Result := FastPos(Source, SubStr, Length(Source), Length(SubStr), P);
end;

function PosN(const SubStr, Source: string; const Stable: TPosNSkipTable; const P:
  Integer = 1): Integer; overload;
begin
  Result := FastPos(Source, SubStr, Length(Source), Length(SubStr), P);
end;

procedure PosNInit(var Table: TPosNSkipTable; const SubStr: string);
begin
end;

{$ELSE}

function PosN(const SubStr, Source: string; const P: Integer = 1): Integer;
var
  I, J, K, L, E, Lp: Integer;
  Skip: TPosNSkipTable;
  C: Char;
begin
  L := length(Source);
  Lp := length(SubStr);

  if (Lp = 0) or (L < Lp) or (P > L) then
  begin
    Result := 0;
    Exit;
  end;

  // Fast algorithm for a 1 character substring
  if Lp = 1 then
  begin
    C := SubStr[1];
    I := P;
    repeat
      if C = Source[I] then
      begin
        Result := I;                              // match
        Exit;
      end;
      Inc(I);
    until I > L;
    Result := 0;
    Exit;                                         // no match
  end;

  // SubString is more than 1 character, use SkipTable
  // Initialize skip table
  for C := Low(Char) to High(Char) do
    Skip[C] := Lp;

  // Make skip table
  for I := Lp - 1 downto 1 do
  begin
    if Skip[SubStr[I]] = Lp then Skip[SubStr[I]] := Lp - I;
  end;

  I := P;
  E := L - Lp + 1;

  while I <= E do
  begin
    J := Lp;
    K := I + Lp - 1;

    // Check if there is a match
    while (J > 0) and (SubStr[J] = Source[K]) do
    begin
      if (J = 1) then
      begin
        Result := I;                              // Match
        Exit;
      end;
      J := J - 1;
      K := K - 1;
    end;

    // no match, do skip
    I := I + Skip[Source[I + Lp - 1]];

  end;
  Result := 0;
end;

function PosN(const SubStr, Source: string; const STable: TPosNSkipTable; const P:
  Integer = 1): Integer; overload;
var
  I, J, K, L, E, Lp: Integer;
  C: Char;
begin
  L := length(Source);
  Lp := length(SubStr);

  if (Lp = 0) or (L < Lp) then
  begin
    Result := 0;
    Exit;
  end;

  // Fast algorithm for a 1 character substring, do not use the skipTable
  if Lp = 1 then
  begin
    C := SubStr[1];
    I := P;
    repeat
      if C = Source[I] then
      begin
        Result := I;                              // match
        Exit;
      end;
      Inc(I);
    until I > L;
    Result := 0;                                  // no match
    Exit;
  end;

  // SubString is more than 1 character, use SkipTable
  I := P;
  E := L - Lp + 1;

  while I <= E do
  begin
    J := Lp;
    K := I + Lp - 1;

    // Check if there is a match
    while (J > 0) and (SubStr[J] = Source[K]) do
    begin
      if (J = 1) then
      begin
        Result := I;                              // Match
        Exit;
      end;
      J := J - 1;
      K := K - 1;
    end;

    // no match, do skip
    I := I + STable[Source[I + Lp - 1]];

  end;
  Result := 0;
end;

procedure PosNInit(var Table: TPosNSkipTable; const SubStr: string);
var
  C: Char;
  Lp, I: Integer;
begin
  Lp := length(SubStr);
  if Lp = 0 then Exit;

  // Initialize skip table
  for C := Low(Char) to High(Char) do
    Table[C] := Lp;

  // Make skip table
  for I := Lp - 1 downto 1 do
  begin
    if Table[SubStr[I]] = Lp then Table[SubStr[I]] := Lp - I;
  end;
end;

// non optimized version
{
function PosN(const SubStr, Source: string; P: Integer = 1): Integer;
begin
  if P < 1 then
  begin
    Result := 0;
    Exit;
  end;
  Result := PosN(SubStr, Copy(Source, P, MaxInt));
  If Result > 0 then Result := Result + P - 1;
end;
}

{$ENDIF}
{
procedure FillString(const SubString: string; var S: string; Position: Word);
var
  L1, L2, Tmp: Integer;
begin
  L2 := length(SubString);
  if L2 = 0 then exit;

  if (Position = 0) then
    Position := 1;

  L1 := length(S);
  Tmp := Position + L2 - 1;                       // minimal final length of the string
  if L1 < (Tmp) then S := S + StringOfChar(' ', Tmp - L1); // make the string long enough, pad with spaces
  CopyMemory(@S[Position], @SubString[1], L2);
end;
}

// Old (slow) version of FillString
procedure FillString(const SubString: string; var S: string; Position: Word);
begin
  if (Position = 0) then
    Position := 1;
  Delete(S, Position, Length(SubString));
  while Length(S) < (Position - 1) do
    S := S + ' ';
  Insert(SubString, S, Position);
end;


function CenterString(S: string; const Mode: CenterMode; const Len: Word): string;
var
  I: Word;
begin
  if Length(S) > Len then
    Result := Copy(S, 1, Len)
  else
  begin
    case Mode of
      cmLeft: I := 1;
      cmMid: I := ((Len - Length(S)) div 2) + 1;
      cmRight: I := (Len - Length(S)) + 1;
    else
      I := 1;
    end;                                          {case}
    Result := SpaceString(Len);
    FillString(S, Result, I);
  end;
end;

(* Old Version
function CenterString(const S: string; const Mode: CenterMode; const Len: Word): string;
var
  I: Word;
  Str: string;
begin
  if Length(S) > Len then
    Result := Copy(S, 1, Len)
  else
  begin
    Str := '';
    case Mode of
      cmLeft: I := 1;
      cmMid: I := ((Len - Length(S)) div 2) + 1;
      cmRight: I := (Len - Length(S)) + 1;
    else
      I := 1;
    end; {case}
    FillString(S, Str, I);
    while (Length(Str) < Len) do
      Str := Str + ' ';
    if Length(Str) > Len then
      Str := Copy(Str, 1, Len);
    Result := Str;
  end;
end;
*)

function SpaceString(const Len: Word): string;
begin
  SpaceString := StringOfChar(' ', Len);
end;

function YYMMDDString: string;
var
  OldShortDateFormat: string; // was string[15];
begin
  OldShortDateFormat := ShortDateFormat;
  ShortDateFormat := 'yymmdd';
  Result := DateToStr(Date);
  ShortDateFormat := OldShortDateFormat;
end;

function YYYYMMDDString: string;
var
  OldShortDateFormat: string; // was string[15];
begin
  OldShortDateFormat := ShortDateFormat;
  ShortDateFormat := 'yyyymmdd';
  Result := DateToStr(Date);
  ShortDateFormat := OldShortDateFormat;
end;

function DayMonthYearString: string;
var
  OldShortDateFormat: string; // was string[15];
begin
  OldShortDateFormat := ShortDateFormat;
  ShortDateFormat := 'dd-mm-yy';
  Result := DateToStr(Date);
  ShortDateFormat := OldShortDateFormat;
end;

function TimeString: string;
var
  OldLongTimeFormat: string; // was string[15];
begin
  OldLongTimeFormat := LongTimeFormat;
  LongTimeFormat := 'hh:nn:ss';
  Result := TimeToStr(Time);
  LongTimeFormat := OldLongTimeFormat;
end;

function IntString(const I: Longint; const Len: Byte): string;
var
  S: string;
begin
  Str(I: Len, S);
  Result := S;
end;

function StringIsIn(S1, S2: string; const Separator: Char = ','): Boolean;
// checks if S1 is in the comma separated list S2
var
  P: Integer;
  H: string;
begin
  S1 := trim(S1);
  S2 := trim(S2);
  Result := false;

  if S1 = S2 then
  begin
    Result := True;
    Exit;
  end;

  while (length(S2) > 0) and (not Result) do
  begin
    P := posN(Separator, S2);
    if P > 0 then
    begin                                         // comma found, get text before comma and delete it.
      H := trim(copy(S2, 1, P - 1));
      Delete(S2, 1, P);
      if S1 = H then Result := true;              // test split up text
    end
    else
    begin                                         // no comma found any more
      if S1 = trim(S2) then Result := true;
      S2 := '';
    end;
  end;
end;

function LeftPad(const S: string; Len: integer; Ch: Char = ' '): string;
// Generates a string consisting of "S" left padded with characters "Ch"
// till length "Len" is reached.
begin
  Result := S;
  while Length(Result) < Len do Result := Ch + Result;
end;

function RightPad(const S: string; Len: integer; Ch: Char = ' '): string;
// Generates a string consisting of "S" right padded with characters "Ch"
// till length "Len" is reached.
begin
  Result := S;
  while Length(Result) < Len do Result := Result + Ch;
end;

function BinString(const B: Byte): string;
var
  Tmp: string; // was string[9]
  Divider: Byte;
begin
  Divider := 128;
  Tmp := '';
  while (Divider > 0) do
  begin
    if ((B and Divider) > 0) then
      Tmp := Tmp + '1'
    else
      Tmp := Tmp + '0';
    if (Divider = 16) then
      Tmp := Tmp + '_';
    Divider := Divider div 2;
  end;
  Result := Tmp;
end;

function FloatToString(const V: real): string;
var S: string;
  Exponent, P, Dp: Integer;
begin

  S := Uppercase(FloatToStr(V));
  P := pos('E', S);

  if P = 0 then
  begin
    Result := S;
    Exit;
  end;

  Result := LeftStr(S, P - 1);
  Exponent := StrToInt(Copy(S, P + 1, maxint));

  if Exponent = 0 then Exit;

  Dp := Pos(DecimalSeparator, Result);

  //if Dp > 0 then // delete trailing zeros
  //while Result[Length(Result)] = '0' do Delete(Result, Length(Result), 1);

  if Dp > 0 then Delete(Result, Dp, 1);           // delete decimal separator

  if Exponent < 0 then
  begin
    Result := DupeString('0', Abs(Exponent)) + Result;
    Insert(DecimalSeparator, Result, 2);          // insert new decimalseparator
  end else                                        // Exponent > 0
  begin
    Result := Result + DupeString('0', Exponent + 1 - Length(Result));
  end;

end;

function IsNumber(S: string): boolean;
var ValidChars, ValidLeadingChars: set of Char;
  I: integer;
begin
  ValidChars := ['0'..'9'] + [DecimalSeparator];
  ValidLeadingChars := ['0'..'9', '+', '-'];

  S := Trim(S);

  Result := true;

  // test leading character
  if (S = '') or
    (not (S[1] in ValidLeadingChars)) then
  begin
    Result := false;
    Exit;
  end;

  // test following characters
  for I := 2 to Length(S) do
    if (not (S[I] in ValidChars)) then
    begin
      Result := false;
      Exit;
    end;
end;

function DateToWeekNo(Dat: string): string;
var Wk, Yr, Dy, Mnth: integer;
    WkS, YrS, DyS: string;
    Tmp : string;
begin
  Tmp := LeftStr(Trim(Dat),10);
  Tmp := StringReplace(Tmp, '/', DateSeparator, [rfReplaceAll]);
  Tmp := StringReplace(Tmp, '-', DateSeparator, [rfReplaceAll]);

  Yr  := YearOf(StrToDate(Tmp));
  Wk := WeekOf(StrToDate(Tmp));
  Mnth := StrToInt(Copy(Tmp, 6, 2));

  // Corrections
  if (Mnth = 1) and  (Wk > 50) then Dec(Yr)
  else if (Mnth = 12) and (Wk = 1) then Inc(Yr)
  else if (Wk = 52) or (Wk = 53) and (DayOfTheWeek(StrToDate(IntToStr(Yr) + '-12-31')) <= 3) then
  begin
    Wk := 1;
    Inc(Yr);
  end;

  YrS := RightStr(IntToStr(Yr), 2);

  WkS := IntToStr(Wk);
  while Length(WkS) < 2 do WkS := '0'+ WkS;

  Dy := DayOfTheWeek(StrToDate(Tmp));
  DyS := '.' + IntToStr(Dy);

  Result := YrS + WkS + DyS;
end;

function WeekNoToDate(WeekNo: string): string;
var Yr, Wk, Dy: Word;
    Tmp: string;
begin
  Tmp := Trim(WeekNo);
  Tmp := StringReplace(Tmp, ',', '.', [rfReplaceAll]);

  if pos('.', Tmp) = 0 then Tmp := Tmp + '.1';

  Tmp := RightStr(Tmp, 6);

  while Length(Tmp) < 6 do Tmp := '0' + Tmp;

  Yr := StrToInt('20'+ LeftStr(Tmp, 2));
  Wk := StrToInt(MidStr(Tmp, 3, 2));
  Dy := StrToInt(RightStr(Tmp,1));

  Result := DateToStr(EncodeDateWeek(Yr, Wk, Dy));
end;

function NumericalString(S: string): string;
var I : word;
begin
  Result := '';
  for I := 1 to Length(S) do
    if (S[I] in ['0'..'9', '.']) then Result := Result + S[I];
end;

end.
