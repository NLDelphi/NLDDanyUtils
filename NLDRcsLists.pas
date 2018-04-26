unit NLDRcsLists;

// Dany Rosseel

{-$DEFINE NoDebug}// Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit
  16-06-2007: * Initial version, holding "IntegerList" and "StringListList"
  28-02-2017: Made things simpler: Interface changed, overloaded procedures eliminated.
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

uses Classes;

type

  TIntegerList = class
  private
    FList: array of Integer;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetVal(Index: Integer; Value: Integer);
    function GetVal(Index: Integer): Integer;
    procedure AddCapacity;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Val: Integer);
    procedure Insert(Index, Val: Integer);
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    property Count: Integer read FCount write FCount;
    property Value[Index: Integer]: Integer read GetVal write SetVal; default;
  end;

  TStringListList = class
  private
    FStringListList: array of TStringList; // the stringlists
    FCount: Integer; // number of Stringlists
    function GetList(ListIndex: Integer): TStringList;
    procedure SetList(ListIndex: Integer; Val: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Val: TStrings);
    procedure Insert(Index: Integer; Val: TStrings);
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    property Count: Integer read FCount write FCount;
    property StringList[ListIndex: Integer]: TStringList read GetList write SetList; default;
  end;


implementation

//-----------------------------------------------------------------//
//                                                                 //
//                      IntegerList                                //
//                                                                 //
//-----------------------------------------------------------------//

const IntegerListInitialCapacity = 10; // do not make this value less than 4!

constructor TIntegerList.Create;
begin
  inherited Create;
  FCapacity := IntegerListInitialCapacity;
  SetLength(FList, FCapacity);
  FCount := 0;
end;

destructor TIntegerList.Destroy;
begin
  SetLength(FList, 0);
  inherited Destroy;
end;

procedure TIntegerList.Clear;
begin
  FCapacity := IntegerListInitialCapacity;
  SetLength(FList, FCapacity);
  FCount := 0;
end;

procedure TIntegerList.SetVal(Index: Integer; Value: Integer);
begin
  if (Index >= 0) and (Index < Length(FList))
    then FList[Index] := Value;
end;

function TIntegerList.GetVal(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < Length(FList))
    then Result := FList[Index]
  else Result := 0;
end;

procedure TIntegerList.AddCapacity;
begin
  if FCapacity <= FCount then
  begin // extra capacity needed
    FCapacity := FCapacity + (FCapacity div 4);
    SetLength(FList, FCapacity);
  end;
end;

procedure TIntegerList.Add(Val: Integer);
begin
  Inc(FCount); // make list longer;
  AddCapacity;
  FList[FCount - 1] := Val;
end;

procedure TIntegerList.Insert(Index, Val: Integer);
var I: Integer;
begin
  if (Index < 0) or (Index > FCount) then exit; // no valid index

  Inc(FCount); // make list longer;
  AddCapacity;

  if Index < (FCount - 1) // "move upwards" operation needed
    then for I := (FCount - 1) downto (Index + 1) do FList[I] := FList[I - 1];

  FList[Index] := Val; // Insert the value
end;

procedure TIntegerList.Delete(Index: Integer);
var I: Integer;
begin
  if (Index < 0) or (Index >= FCount) then exit; // no valid index

  if Index < (FCount - 1) // "move downwards" operation needed
    then for I := Index to (FCount - 2) do FList[I] := FList[I + 1];

  Dec(FCount);
end;

procedure TIntegerList.Exchange(Index1, Index2: Integer);
var Tmp: Integer;
begin
  if (Index1 < 0) or (Index2 < 0) or
    (Index1 >= FCount) or (Index2 >= FCount) then Exit;
  Tmp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Tmp;
end;


//-----------------------------------------------------------------//
//                                                                 //
//                      StringListList                             //
//                                                                 //
//-----------------------------------------------------------------//

constructor TStringListList.Create;
begin
  inherited Create;
  SetLength(FStringListList, 0);
end;

destructor TStringListList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringListList.Clear;
var I: Integer;
begin
  for I := 0 to Length(FStringListList) - 1 do FStringListList[I].Free;
  SetLength(FStringListList, 0);
  FCount := 0;
end;

function TStringListList.GetList(ListIndex: Integer): TStringList;
begin
  Result := nil;
  if (ListIndex >= 0) and (ListIndex < FCount)
    then Result := FStringListList[ListIndex];
end;

procedure TStringListList.SetList(ListIndex: Integer; Val: TStringList);
begin
  if (ListIndex >= 0) and (ListIndex < FCount)
    then FStringListList[ListIndex].Assign(Val);
end;

procedure TStringListList.Add(Val: TStrings);
begin
  // create an extra stringlist in FStringListList
  SetLength(FStringListList, Length(FStringListList) + 1);
  FStringListList[Length(FStringListList) - 1] := TStringList.Create;
  Inc(FCount);

  // fill the string
  FStringListList[FCount - 1].Assign(Val);
end;

procedure TStringListList.Insert(Index: Integer; Val: TStrings);
var Ind: integer;
    Lst: TStrings;
begin
  // insert an extra entry
  if (Index < 0) or (Index > FCount) then exit;

  // create an extra stringlist in FStringListList
  SetLength(FStringListList, Length(FStringListList) + 1);
  FStringListList[Length(FStringListList) - 1] := TStringList.Create;
  Inc(FCount);

  Lst := TStringList.Create;

  for Ind := (FCount - 2) downto Index do // move up in the list
  begin
    Lst.Assign(FStringListList[Ind]);
    FStringListList[Ind + 1].Assign(Lst);
  end;

  Lst.Free;

  FStringListList[Index].Assign(Val);
end;

procedure TStringListList.Delete(Index: Integer);
var Ind: integer;
    Lst: TStrings;
begin
  if (Index < 0) or (Index >= FCount) then exit;

  Lst := TStringList.Create;

  for Ind := Index to FCount - 2 do // move down in the list
  begin
    Lst.Assign(FStringListList[Ind + 1]);
    FStringListList[Ind].Assign(Lst);
  end;

  Lst.Free;

  Dec(FCount);
end;

procedure TStringListList.Exchange(Index1, Index2: Integer);
var Lst: TStrings;
begin
  Lst := TStringList.Create;

  Lst.Assign(FStringListList[Index1]); // exchange
  FStringListList[Index1].Assign(FStringListList[Index2]);
  FStringListList[Index2].Assign(Lst);

  Lst.Free;

end;

end.
