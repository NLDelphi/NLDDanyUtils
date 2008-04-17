unit NLDRcsCSV;

// Dany Rosseel 

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit:
27-02-2005: * Initial version
29-05-2005: * Added function "AddCalculatedField"
22-06-2005: * Removed warnings "... might not been initialized"
02-07-2005: * Added operators "CumulSum" and "AVG" to function "AddCalculatedField"
03-07-2005: * Added "MovingAVG" to function "AddCalculatedField"
08-07-2005: * Added "%" to function "AddCalculatedField"
09-07-2005: * Adapted the "MovingAVG" function of "AddCalculatedField":
              Now the number of "passed" and "future" numbers for the
              moving average calculations can be set separately
11-07-2005: * "AVG" and "MovingAVG" can handle reals now
21-04-2006: * Added the "Delta" possibility to the "AddCalculatedField" function
25-05-2006: * Added the "DateToWeekNo", "DateToWeekNoOnly" and "WeekNoToDate"
              operators to the "AddCalculatedField" function
25-02-2005: * Changed the internal data structure of the "TCommaSeparatedValues" class to make it more
              speed efficient.
11-03-2005: * Removed an error in "LoadFromFile" (FLineCount was not set to zero at the beginning of it)
18-06-2007: * Used TStringListList (unit RCSLists) for the implementation of TCommaSeparatedValues
29-06-2007: * Used TSimpleEval now to calculate numerical "Calculated Fields"
10-09-2007: * Corrected an error in the "calculated fields" function.
16-02-2008: * Corrected an error in the "calculated fields", functions
                DATETOWEEKNO, DATETOWEEKNOONLY and WEEKNOTODATE
}

{$P+} // Open Strings ON
{$H+} // Long Strings ON

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

uses Classes, RcsLists, RcsEval;

type

  TCommaSeparatedValues = class(TObject)
  private
    FIndex: Integer;
    FLines: TStringListList;
    FLineCount: LongInt;
    FFileName: string;
    FChanged: Boolean;
    procedure SetIndex(No: Integer);
    function IndexValid: boolean;
    function FieldNumberValid(No: Integer): boolean;
    procedure ReadFields; // at position Index
    procedure WriteFields; // at position Index
  public
    FieldNames, Fields, NewEntry: TStrings;
    function Count: Integer;
    function FieldCount: Integer;
    function FieldIndex(const Name: string): Integer;
    property Index: Integer read FIndex write SetIndex;
    procedure LoadFromFile(const Fn: string);
    procedure SaveToFile(const Fn: string);
    constructor Create(const fn: string = '');
    destructor Destroy; override;
    procedure Clear;
    function GetField(const No: Integer): string; overload; // data = "Fields"
    function GetField(const Name: string): string; overload; // data = "Fields"
    procedure SetField(const No: Integer; const Fld: string); overload;
      // data = "Fields"
    procedure SetField(const Name: string; const Fld: string); overload;
      // data = "Fields"
    procedure Add; // data = "NewEntry"
    procedure Insert(const No: Integer); // data = "NewEntry"
    procedure Delete(const No: Integer);
    procedure AddField(const Name: string; const Txt: string = '');
    procedure InsertField(const No: Integer; const Fld: string; const Txt: string
      = '');
    function AddCalculatedField(const FieldDef: string): integer;
    procedure DeleteField(const No: Integer);
    procedure CheckIndex(No: Integer);
    procedure CheckFieldNumber(No: Integer);
  end;

implementation

uses SysUtils,
  StrUtils,
  DateUtils,
  RcsStrings;

function TCommaSeparatedValues.IndexValid: boolean;
begin
  Result := (Index >= 0) and (Index < FLineCount);
end;

procedure TCommaSeparatedValues.ReadFields;
var I: Integer;
begin
  if IndexValid then
    for I := 0 to FieldCount - 1 do Fields[I] := FLines[I][Index];
end;

procedure TCommaSeparatedValues.WriteFields;
var I: Integer;
begin
  if IndexValid then
    for I := 0 to FieldCount - 1 do Flines[I][Index] := Fields[I];
end;

procedure TCommaSeparatedValues.CheckIndex(No: Integer);
begin
  if not ((No >= 0) and (No < FLineCount)) then
    raise ERangeError.CreateFmt(
      'Index %d is not within the valid range of %d..%d',
      [No, 0, FLineCount - 1]);
end;

function TCommaSeparatedValues.FieldNumberValid(No: Integer): boolean;
begin
  Result := (No >= 0) and (No < FieldNames.Count);
end;

procedure TCommaSeparatedValues.CheckFieldNumber(No: Integer);
begin
  if not FieldNumberValid(No) then
    raise ERangeError.CreateFmt(
      'FieldNumber %d is not within the valid range of %d..%d',
      [No, 0, FieldNames.Count - 1]);
end;

procedure TCommaSeparatedValues.Add;
var I: Integer;
begin
  for I := 0 to FieldCount - 1 do FLines[I].Add(NewEntry[I]);
  Inc(FLineCount);
  Index := FLineCount - 1;
  FChanged := true;
end;

procedure TCommaSeparatedValues.Insert(const No: Integer);
var I: Integer;
begin
  CheckIndex(No);
  for I := 0 to FieldCount - 1 do FLines[I].Insert(No, NewEntry[I]);
  Inc(FLineCount);
  Index := No;
  FChanged := true;
end;

procedure TCommaSeparatedValues.Delete(const No: Integer);
var I: Integer;
begin
  CheckIndex(No);
  if Index = No then for I := 0 to FieldCount - 1 do Fields[I] := '';
  for I := 0 to FieldCount - 1 do FLines[I].Delete(No);
  Dec(FLineCount);

  if FLineCount > 0 then
  begin
    if Index < FLineCount
      then Index := Index // keep index
    else Index := FLineCount - 1 // index was passed end
  end
  else
    Index := -1; // no records left
  FChanged := true;
end;

function TCommaSeparatedValues.GetField(const No: Integer): string;
begin
  Result := '';
  CheckIndex(Index);
  CheckFieldNumber(No);
  Result := Fields[No];
end;

function TCommaSeparatedValues.GetField(const Name: string): string;
var
  I: Integer;
begin
  Result := '';
  CheckIndex(Index);
  I := FieldIndex(Name);
  CheckFieldNumber(I);
  Result := Fields[I];
end;

procedure TCommaSeparatedValues.SetField(const No: Integer; const Fld: string);
begin
  CheckIndex(Index);
  CheckFieldNumber(No);
  Fields[No] := Fld;
  WriteFields;
  FChanged := true;
end;

procedure TCommaSeparatedValues.SetField(const Name: string; const Fld: string);
var
  I: Integer;
begin
  CheckIndex(Index);
  I := FieldIndex(Name);
  CheckFieldNumber(I);
  Fields[I] := Fld;
  WriteFields;
  FChanged := true;
end;

function TCommaSeparatedValues.FieldIndex(const Name: string): Integer;
begin
  Result := FieldNames.IndexOf(Name);
end;

procedure TCommaSeparatedValues.AddField(const Name: string; const Txt: string =
  '');
var I: Integer;
begin
  //InsertField(FieldNames.Count, Name, Txt);

  FieldNames.Add(Name); // add fieldname
  NewEntry.Add('');
  Fields.Add('');

  // create an extra stringlist in FLines
  FLines.Add;
  for I := 0 to FLineCount - 1 do // add empty field in each record
    FLines[FLines.Count - 1].Add(Txt);

  ReadFields;
  FChanged := true;
end;

procedure TCommaSeparatedValues.InsertField(const No: Integer; const Fld:
  string; const Txt: string = '');
var
  I: Integer;
begin
  if (No >= 0) and (No <= FieldNames.Count) then
  begin
    FieldNames.Insert(No, Fld); // insert fieldname
    NewEntry.Insert(No, '');
    Fields.Insert(No, '');

    // create an extra stringlist in FLines
    FLines.Insert(No);

    for I := 0 to FLineCount - 1 do // insert empty field in each record
      FLines[No].Add(Txt);

    ReadFields;

    FChanged := true;
  end
  else
    raise ERangeError.CreateFmt(
      'FieldNumber %d is not within the valid range of %d..%d',
      [No, 0, FieldNames.Count]);
end;

procedure TCommaSeparatedValues.DeleteField(const No: Integer);
begin
  CheckFieldNumber(No);

  FieldNames.Delete(No);
  NewEntry.Delete(No);
  Fields.Delete(No);

  ReadFields;
  FChanged := true;
end;

{
AddCalculatedField adds a new field (or updated an existing field)
with values calculated from already existing fields.

The "FieldDef" has the following formats:
  Newfield = Oldfield1 + OldField2              (sum of two fields)
  Newfield = Oldfield1 - OldField2              (difference btw two fields)
  Newfield = Oldfield1 * OldField2              (product of two fields)
  Newfield = Oldfield1 / OldField2              (quotient of two fields)
  NewField = OldField1 % OldField2  		(percentage: 100 * OldField1 / OldField2)
  Newfield = DaySpan(Oldfield1, OldField2)      (days between date fields)
  Newfield = HourSpan(Oldfield1, OldField2)     (hours between date fields)
  Newfield = CumulSum(Oldfield)                 (cumulative sum of a field)
  Newfield = AVG(Oldfield)                      (cumulative average of a field)
  NewField = MovingAVG(OldField, Prev, Next)    (moving average of a field)
  NewField = Delta(OldField)                    (Difference with the OldField value in the previous record)
  NewField = DateToWeekNo(OldField)             (converts a date to a weeknumber format "yww.d")
  NewField = DateToWeekNoOnly(OldField)         (converts a date to a weeknumber format "yww")
  NewField = WeekNoToDate(OldField)             (converts a weeknumber format "yww.d" or "yww" to a date)
                                                ( a Weeknumber has the format "yww.d")

The result of the function:
 0 = all fine
 1 = "OldField1" does not exist
 2 = "OldField2" does not exist
 3 = operator not recognized
}

function TCommaSeparatedValues.AddCalculatedField(const FieldDef: string):
  Integer;
var
  FieldName, Formula: string;
  IndexNew: Integer;
  Value1: real;
  DateTime1, DateTime2: TDateTime;
  Operator: char;

  function GetFieldIndex(const Field: string): Integer;
  begin
    Result := FieldIndex(Trim(Field));
  end;

  function GetNumericValue(const FieldNo: Integer; var V: real): boolean;
  var
    TmpS: string;
  begin
    Result := true;
    if Fields[FieldNo] = '' then
    begin
      Result := false;
      exit;
    end;
    TmpS := StringReplace(Fields[FieldNo], '.', DecimalSeparator, []);
    TmpS := StringReplace(TmpS, ',', DecimalSeparator, []);
    try
      V := StrToFloat(TmpS);
    except
      on Exception do Result := false;
    end;
  end;

  function GetDateValue(FieldNo: integer; var D: TDateTime): boolean;
  var
    TmpS: string;
  begin
    Result := true;
    if Fields[FieldNo] = '' then
    begin
      Result := false;
      exit;
    end;
    TmpS := Fields[FieldNo];
    try
      D := StrToDateTime(TmpS);
    except
      on Exception do Result := false;
    end;
  end;

  procedure HandleMath(Formula: string);
  var Res: string;
    E: TSimpleEval;
    I, J: integer;
    TmpR: real;

  begin
    E := TSimpleEval.Create;
    E.Formula := Formula;

    for I := 0 to Count - 1 do
    begin
      Index := I;
      E.ClearVars;
      for J := 0 to FieldNames.Count - 1 do
        if GetNumericValue(J, TmpR)
          then E.SetVar(Fieldnames[J], TmpR);
      try
        Res := FloatToStr(E.Result);
        SetField(IndexNew, Res);
      except
        on Exception do SetField(IndexNew, '');
      end;
    end;

    E.Free;
  end;

  procedure HandleSpan(Formula: string; Operator: char);
  var
    Field1, Field2, Res, TmpS: string;
    Index1, Index2, I: integer;
  begin

    TmpS := CopyString(Formula, '(', ')');
    SplitString(TmpS, ',', Field1, Field2);

    Index1 := GetFieldIndex(Field1);
    Index2 := GetFieldIndex(Field2);

    if Index1 < 0 then
    begin
      Result := 1;
      Exit;
    end;

    if Index2 < 0 then
    begin
      Result := 2;
      Exit;
    end;

    for I := 0 to Count - 1 do
    begin
      Index := I;
      Res := '';

      if GetDateValue(Index1, DateTime1) and
        GetDateValue(Index2, DateTime2) then
      begin
        try
          case Operator of
            '1': Res := FloatToStr(DaySpan(DateTime1, DateTime2));
            '2': Res := FloatToStr(HourSpan(DateTime1, DateTime2));
          end;
        except
          on Exception do ;
        end;
        SetField(IndexNew, Res);
      end;
    end;
  end;

  procedure HandleCumul(Formula: string; Operator: char);
  var
    Field1, Res: string;
    Index1, I, Cnt: integer;
    CSum: real;
  begin

    Field1 := CopyString(Formula, '(', ')');
    Index1 := GetFieldIndex(Field1);
    if Index1 < 0 then
    begin
      Result := 1;
      Exit;
    end;

    CSum := 0.0;
    Cnt := 0;
    for I := 0 to Count - 1 do
    begin
      Index := I;
      Res := '';

      if GetNumericValue(Index1, Value1) then
      try
        CSum := CSum + Value1;
        Inc(Cnt);
        case Operator of
          '3': Res := FloatToStr(CSum);
          '4': Res := FloatToStr(CSum / Cnt);
        end;
      except
        on Exception do ;
      end;
      SetField(IndexNew, Res);
    end;
  end;

  procedure HandleDelta(Formula: string; Operator: Char);
  var
    Field1, Res: string;
    Index1, I: integer;
    Prev: Real;
  begin
    Field1 := CopyString(Formula, '(', ')');
    Index1 := GetFieldIndex(Field1);
    if Index1 < 0 then
    begin
      Result := 1;
      Exit;
    end;

    Prev := 0;
    for I := 0 to Count - 1 do
    begin
      Index := I;
      Res := '';
      if GetNumericValue(Index1, Value1) then
      try
        if I = 0
          then Prev := Value1
        else begin
          Res := FloatToStr(Value1 - Prev);
          Prev := Value1;
        end;
      except
        on Exception do ;
      end;
      SetField(IndexNew, Res);
    end;
  end;

  procedure HandleMovingAverage(Formula: string; Operator: char);
  var
    Index1, I, J, L, R, Left, Right, TmpI: Integer;
    TmpS, Res: string;
    TmpR: real;
    TmpSL: TStringList;
  begin // the operator is "Moving Average", one, two or three operands

    TmpSL := TStringList.Create;
    try
      TmpS := CopyString(Formula, '(', ')');
      StringToTStrings(TmpS, TmpSL);
      TrimTStrings(TmpSL);

      if TmpSL.Count = 0 then
      begin
        Result := 1;
        Exit;
      end;

      Index1 := GetFieldIndex(TmpSl[0]);
      if Index1 < 0 then
      begin
        Result := 1;
        Exit;
      end;

      try
        L := StrToInt(TmpSl[1]);
      except
        on Exception do L := 1;
      end;
      
      try
        R := StrToInt(TmpSl[2]);
      except
        on Exception do R := 0;
      end;

      if L < 0 then L := 0;
      if R < 0 then R := 0;

      for I := 0 to Count - 1 do
      begin
        Index := I;
        Res := '';

        Left := Index - L;
        Right := Index + R;

        if (Left >= 0) and (Right < Count) then
        begin
          TmpR := 0.0;
          TmpI := 0;
          for J := Left to Right do
          begin
            Index := J;
            try
              if GetNumericValue(Index1, Value1) then
              begin
                TmpR := TmpR + Value1;
                Inc(TmpI);
              end;
            except
              on Exception do Continue;
            end;
          end;

          if TmpI > 0 then
          begin
            try
              Res := FloatToStr(TmpR / TmpI);
            except
              on Exception do ;
            end;
          end;
        end;

        Index := I; // restore index
        SetField(IndexNew, Res);
      end;

    finally
      TmpSL.Free;
    end;
  end;

  procedure HandleDateWeek(Formula: string; Operator: Char);
  var
    Field1, Res: string;
    Index1, I, P: integer;
    TmpS: string;
  begin
    Field1 := CopyString(Formula, '(', ')');
    Index1 := GetFieldIndex(Field1);

    if Index1 < 0 then
    begin
      Result := 1;
      Exit;
    end;

    for I := 0 to Count - 1 do
    begin
      Index := I;
      Res := '';
      TmpS := Fields[Index1];
      case Operator of
        '7', '8': // DateToWeekNo
          begin
            P := Pos(' ', TmpS);
            if P > 0 then TmpS := Copy(TmpS, 1, P - 1);
            try
              Res := DateToWeekNo(TmpS);
              if Operator = '8' // excluding the daynumber
              then Res := LeftStr(Res, length(Res) - 2);
            except
              on Exception do ;
            end;
          end;
        '9': // WeekNo to Date
          begin
            try
              Res := WeekNoToDate(TmpS);
            except
              on Exception do ;
            end;
          end;
      end;
      SetField(IndexNew, Res);
    end;
  end;

begin

  Result := 0; // success

  SplitString(FieldDef, '=', FieldName, Formula);
  FieldName := trim(FieldName);
  Formula := Trim(Formula);

  if Pos('DAYSPAN', UpperCase(Formula)) > 0 then Operator := '1'
  else
  if Pos('HOURSPAN', UpperCase(Formula)) > 0 then Operator := '2'
  else
  if Pos('CUMULSUM', UpperCase(Formula)) > 0 then Operator := '3'
  else
  if (Pos('AVG', UpperCase(Formula)) > 0) and
     (Pos('MOVINGAVG', UpperCase(Formula)) = 0)  then Operator := '4'
  else
  if (Pos('AVERAGE', UpperCase(Formula)) > 0) and
     (Pos('MOVINGAVERAGE', UpperCase(Formula)) = 0) then Operator := '4'
  else
  if Pos('MOVINGAVG', UpperCase(Formula)) > 0 then Operator := '5'
  else
  if Pos('MOVINGAVERAGE', UpperCase(Formula)) > 0 then Operator := '5'
  else
  if Pos('DELTA', UpperCase(Formula)) > 0 then Operator := '6'
  else
  if (Pos('DATETOWEEKNO', UpperCase(Formula)) > 0) and
     (Pos('DATETOWEEKNOONLY', UpperCase(Formula)) = 0) then Operator := '7'
  else
  if Pos('DATETOWEEKNOONLY', UpperCase(Formula)) > 0 then Operator := '8'
  else
  if Pos('WEEKNOTODATE', UpperCase(Formula)) > 0 then Operator := '9'
  else
  Operator := '+';

  if FieldIndex(FieldName) < 0 then AddField(FieldName);
  IndexNew := FieldIndex(FieldName);

  case Operator of
    '+': HandleMath(Formula);
    '1', '2': HandleSpan(Formula, Operator);
    '3', '4': HandleCumul(Formula, Operator);
    '5': HandleMovingAverage(Formula, Operator);
    '6': HandleDelta(Formula, Operator);
    '7', '8', '9': HandleDateWeek(Formula, Operator);
  end; {case}

end;

constructor TCommaSeparatedValues.Create(const fn: string = '');
begin
  inherited Create;

  FieldNames := TStringList.Create;
  Fields := TStringList.Create;
  NewEntry := TStringList.Create;
  FLines := TStringListList.Create;
  FIndex := -1;
  FFileName := fn;
  if FFileName > '' then Self.LoadFromFile(FFileName);
  FChanged := false;

end;

destructor TCommaSeparatedValues.Destroy;
begin
  if (FFileName > '') and FChanged then Self.SaveToFile(FFileName);

  FieldNames.Free;
  Fields.Free;
  NewEntry.Free;
  FLines.Free;

  inherited Destroy;
end;

procedure TCommaSeparatedValues.Clear;
begin
  FLines.Clear;
  FLineCount := 0;
  FieldNames.Clear;
  Fields.Clear;
  NewEntry.Clear;
  FIndex := -1;
end;

function TCommaSeparatedValues.Count: Integer;
begin
  Result := FLineCount;
end;

function TCommaSeparatedValues.FieldCount: Integer;
begin
  Result := FieldNames.Count;
end;

procedure TCommaSeparatedValues.SetIndex(No: Integer);
begin
  CheckIndex(No);
  FIndex := No;
  ReadFields;
  while Fields.Count < FieldCount do
    Fields.Add('');
  FIndex := No;
end;

procedure TCommaSeparatedValues.LoadFromFile(const Fn: string);
var I: LongInt;
  F: TextFile;
  TmpS: string;
begin
  FLines.Clear;

  FLineCount := 0;
  FIndex := -1;

  FieldNames.Clear;
  Fields.Clear;
  NewEntry.Clear;

  if FileExists(Fn) then
  begin
    AssignFile(F, Fn);
    Reset(F);
    while not eof(F) do
    begin
      ReadLn(F, TmpS);
      TmpS := Trim(TmpS);
      if TmpS > '' then
      begin
        if FieldNames.Count = 0 then
        begin
          CSVToTStrings(TmpS, FieldNames);
          for I := 0 to FieldNames.Count - 1 do
          begin
            FLines.Add;
            Fields.Add('');
            NewEntry.Add('');
          end;
        end else
        begin
          CSVToTStrings(TmpS, NewEntry);
          while NewEntry.Count < FieldNames.Count do NewEntry.Add('');
          Add;
        end;
      end;
    end;
    CloseFile(F);
    if FLineCount > 0 then Index := 0;
  end
  else
    raise ERangeError.CreateFmt('File %s does not exist.', [Fn]);
  FChanged := false;
end;

procedure TCommaSeparatedValues.SaveToFile(const Fn: string);
var
  Tmp: TStrings;
  TmpS: string;
  I: Integer;
begin
  Tmp := TStringList.Create;
  if FileExists(Fn) then DeleteFile(Fn);
  Tmp.Add(TStringsToCSV(FieldNames));
  for I := 0 to FLineCount - 1 do
  begin
    Index := I;
    TmpS := TStringsToCSV(Fields);
    Tmp.Add(TmpS);
  end;
  Tmp.SaveToFile(Fn);
  Tmp.Free;
end;

end.
