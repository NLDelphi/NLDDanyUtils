unit NLDRcsEval;

// Dany Rosseel

{.$DEFINE NoDebug}// Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit
  25-06-2007: * Initial version.
  29-06-2007: * Changed the name of the class to "TSimpleEval".
  30-06-2007: * Changed the implementation of "Parenthesis" from procedure to function.
              * Simplified "SetVar": used "IndexOfName" now.
  01-07-2007: * Added the error "Operand Missing" (gave a "range check error" before)
  08-07-2007: * Added a "functions" possibility
  11-07-2007: * Corrected a (harmless) error in the "HasFunction" routine,
                an empty function argument list is allowed now
  14-07-2007: * Added a whole bunch of functions (but tried to leave out those
                than can be obtained using the ones that are in now)
              * Added the possibility to disable function evaluation
                (see property "NoFunctions") to make the evaluator faster if
                no function evaluation is needed.
  17-07-2007: * Added a meaningfull exception when too little parameters are
                given to a function. Too much parameters are ignored.
              * Added also a meaningfull exception executing a division by zero
                or illegally formatted values
              * Added a test (and an exception) now for unmatched opening function parenthesis also
              * Added an exception for illformatted parameter values
              * Added exceptions for missing function arguments
              * Taken into account the remarks of Jelmer Vos, see NLDelphi
                http://www.nldelphi.com/Forum/showpost.php?p=232921&postcount=34
  17-07-2007: * Made the execption for missing right function parenthesis more correct
  18-07-2007: * Made nested functions with more than 1 argument work
              * Added the "IfThen" function
       * Added the "=", "<" and ">" operators
       * Cleaned up somewhat and made all local routines (private) TSimpleEval methods
  19-07-2007: * Added the "not" function
  20-07-2007: * Added the "|" and the "&" boolean operators
                "|" as logical "or" and
                "&" as logical "and" for boolean values in "IfThen", so
                (a > b) | (c > d) : means (a > b) or (c > d), while
                (a > b) & (c > d) : means (a > b) and (c > d)
  21-07-2007: * Solved a precision issue
  22-07-2007: * Used "FloatToString" now in stead of "FloatToStrF"
  01-08-2007: * Solved an inconvenience ( "a - -b" does not give an error any more) 
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

uses Classes, SysUtils;

const
  SimpleEvalOperators = ['+', '-', '*', '/', '^', '%', '=', '>', '<', '|', '&'];

  SimpleEvalFunctions: array[0..28] of string =
  ('abs', 'int', 'frac', 'round', 'ceil', 'floor', 'sum', 'avg',
    'min', 'max', 'sqrt', 'pi', 'logn', 'log', 'power', 'sign',
    'ifthen', 'not', 'rad', 'deg', 'cmp', 'ln', 'exp',
    'sin', 'cos', 'tan', 'arcsin', 'arccos', 'arctan');

type
  TSimpleEval = class
  private
    FFormula: string;
    FVars: TStringList;
    FNoFunctions: boolean;
    function GetResult: real;
    function EvalNumerical(S: string): real;
    function HasParenthesis(S: string; var LeftParenthesis, RightParenthesis: Integer; var Arg_: string; StartPos: Integer = 1): boolean;
    function HasFunction(S: string; var Func_, Arg_: string; var B, E: Integer): boolean;
    procedure ReplaceFunctionCalls(var S: string);
    procedure ProcessBetweenParenthesis(var S: string);
    function SolveFunction(Func_, Args_: string): real;
    procedure RemoveOuterParenthesis(var S: string);
    function GetValue(S: string): real;
  public
    property Formula: string read FFormula write FFormula;
    procedure SetVar(Name: string; Value: real); overload;
    procedure SetVar(Name: string; Value: string); overload;
    procedure ClearVars;
    property Result: real read GetResult;
    property NoFunctions: boolean read FNoFunctions write FNoFunctions;
    constructor Create;
    destructor Destroy; override;
  end;

type ESimpleEvalException = class(Exception);


implementation

uses NLDRcsStrings, NLDRcsLists, StrUtils, Math;

// -------------------------------------------------------------------
// "GetResult" is the "read" procedure for the property "Result"
// -------------------------------------------------------------------

function TSimpleEval.GetResult: real;
begin
  Result := EvalNumerical(FFormula);
end;


// -------------------------------------------------------------------
// "EvalNumerical" evaluates the expression in string S and returns the numerical result
// This is the actual main expression evaluation routine.
// -------------------------------------------------------------------

function TSimpleEval.EvalNumerical(S: string): real;
var TmpS: string;
  Tussen: real;
  Op: Char;
  Ops: TIntegerList;
  I: Integer;
  LastCharWasOp: boolean;

begin
  Result := 0;
  Tussen := 0; // to avoid warning
  S := Trim(S);

  // first remove outer matching parenthesis (if any)
  RemoveOuterParenthesis(S);

  if S = '' then exit; // nothing to process, result is zero

  if IsNumber(S) then // take a shortcut if the expression represents a number only
  begin
    Result := StrToFloat(S);
    Exit;
  end;

  // First process any functions present in expression S
  if not FNoFunctions // only if "functions" are not disabled
    then ReplaceFunctionCalls(S);

  // Secondly process expressions between Parenthesis present in expression S
  ProcessBetweenParenthesis(S);

  // Check for unmatched Parenthesis (none should be left here, all should be processes already)
  if (Pos('(', S) > 0) or
    (Pos(')', S) > 0) then
  begin
    raise ESimpleEvalException.CreateFmt('Unmatched parenthesis in "%s"', [s]);
  end;

  // Add the default first operator
  if not (S[1] in SimpleEvalOperators) then S := '+' + S;

  Ops := TIntegerList.Create;
  try

  // Make a list of operators (store their position in the expression)
  // 2 operators after each other: only the first is an operator,
  // the second one is a sign character (+ or -)
    LastCharWasOp := false;
    for I := 1 to Length(S) do
    begin
      if S[I] in SimpleEvalOperators then // char is an operator
      begin
        if (not LastCharWasOp) then
        begin
          Ops.Add(I);
          LastCharWasOp := true; // "first" operator detected
        end else
        begin // second operator detected, should always be "+" or "-"
          if not (S[I] in ['+', '-'])
            then raise ESimpleEvalException.CreateFmt('Not allowed unary operator "%s" in expression "%s"', [S[I], trim(S)]);
        end;
      end else // char is not an operator
      begin // ignore spaces or tabs, all others are marked as "non operators"
        if not (S[I] in [' ', #9]) then LastCharWasOp := false;
      end;
    end;

    Ops.Add(Length(S) + 1); // Add the sentinel

  //--------------------------------------------//
  // Evaluate the expression from left to right //
  //--------------------------------------------//

  // the "starting" value of the expression is always zero
    Tussen := 0;

    for I := 0 to Ops.Count - 2 do
    begin
    // get operator
      Op := S[Ops[I]];

    // get operand ( = string between the current operator and the next one )
      TmpS := trim(Copy(S, Ops[I] + 1, Ops[I + 1] - Ops[I] - 1));

    // do the calculation
      try
        case Op of
          '+': Tussen := Tussen + GetValue(TmpS);
          '-': Tussen := Tussen - GetValue(TmpS);
          '*': Tussen := Tussen * GetValue(TmpS);
          '/': Tussen := Tussen / GetValue(TmpS);
          '^': Tussen := Power(Tussen, GetValue(TmpS));
          '%': Tussen := Tussen / GetValue(TmpS) * 100;
          '=': if Tussen = GetValue(TmpS)
            then Tussen := 1
            else Tussen := 0;
          '>': if Tussen > GetValue(TmpS)
            then Tussen := 1
            else Tussen := 0;
          '<': if Tussen < GetValue(TmpS)
            then Tussen := 1
            else Tussen := 0;
          '|': if (Round(Tussen) >= 1) or (Round(GetValue(TmpS)) >= 1)
            then Tussen := 1
            else Tussen := 0;
          '&': if (Round(Tussen) >= 1) and (Round(GetValue(TmpS)) >= 1)
            then Tussen := 1
            else Tussen := 0;
        end;
      except
        on EConvertError do
          raise ESimpleEvalException.CreateFmt('Value is not well formatted: "%s" in expression part "%s"', [TmpS, trim(S)]);
        on EZeroDivide do
          raise ESimpleEvalException.CreateFmt('Divider is zero: "%s" in expression part "%s"', [TmpS, trim(S)]);
        on EInvalidOp do
          raise ESimpleEvalException.CreateFmt('Invalid operation in expression part "%s"', [trim(S)]);
      end;

    end;
  finally
    Ops.Free;
  end;
  Result := Tussen;
end;


// -------------------------------------------------------------------
// "HasParenthesis" finds (starting from "StartPos") the position of the first parenthesis in string S,
// its matching closing one, and returns the string in between
// -------------------------------------------------------------------

function TSimpleEval.HasParenthesis(S: string; var LeftParenthesis, RightParenthesis: Integer; var Arg_: string; StartPos: Integer = 1): boolean;
var I, P, Cnt: Integer;
begin
  LeftParenthesis := 0; // = 'not found'
  RightParenthesis := 0; // = 'not found'
  Result := false; // = 'not found'
  Arg_ := ''; // the string between the parenthesis, initially empty (not found)

  P := PosN('(', S, StartPos);
  if P > 0 then
  begin
    LeftParenthesis := P;
    Cnt := 1;
    for I := P + 1 to length(S) do
    begin
      if S[I] = '(' then Inc(Cnt);
      if S[I] = ')' then Dec(Cnt);
      if Cnt = 0 then
      begin // match between left and right parenthesis
        RightParenthesis := I;
        Arg_ := Copy(S, LeftParenthesis + 1, RightParenthesis - LeftParenthesis - 1);
        Result := true;
        Exit;
      end;
    end;
  end;
end;


// -------------------------------------------------------------------
// "ProcessBetweenParenthesis" replaces expressions between Parenthesis by their actual value
// -------------------------------------------------------------------

procedure TSimpleEval.ProcessBetweenParenthesis(var S: string);
var Lp, Rp: Integer;
  TmpS: string;
  TmpR: real;
begin
  while HasParenthesis(S, Lp, Rp, TmpS) do
  begin // replace the parenthesis contents by its numerical value
    TmpR := EvalNumerical(TmpS);
    S := LeftStr(S, Lp - 1) + ' ' + FloatToString(TmpR) + ' ' + Copy(S, Rp + 1, MaxInt);
  end;
end;


// -------------------------------------------------------------------
// "HasFunction" returns data about the first function call found in expression S:
// Func_ = functionname
// Arg_ = the contents between the parenthesis (the argument of the function)
// B    = the startposition in S of the function call (1st char of the function name)
// E    = the endposition in S of the function call   (closing parenthesis)
// -------------------------------------------------------------------

function TSimpleEval.HasFunction(S: string; var Func_, Arg_: string; var B, E: Integer): boolean;
var P, LeftPar, RightPar, I: Integer;
  TmpS, TmpS1: string;
  Tmp1: integer;
begin
  Result := false;
  Func_ := '';
  B := 0;
  E := 0;
  Arg_ := '';

  TmpS := LowerCase(S);

  for I := 0 to Length(SimpleEvalFunctions) - 1 do
  begin
    P := Pos(SimpleEvalFunctions[I], TmpS); // find first function call

    while P > 0 do // function name found, check if it is indeed a function
    begin

      Tmp1 := P + Length(SimpleEvalFunctions[I]); // position right after the function name

      // Test the characters before and after the keyword found

      if // Before the keyword
        (
        (P = 1) or // none
        (S[P - 1] in [' ', #9] + SimpleEvalOperators) // space, tab or Operator
        )
        and // After the keyword
        (
        (Tmp1 > Length(TmpS)) or // nothing after the keyword
        (S[Tmp1] in [' ', #9, '(']) // space, tab or opening parenthises
        )

      then // a function name has actually been detected
      begin

        // test if an argument for the function is there
        if HasParenthesis(S, LeftPar, RightPar, Arg_, Tmp1) then

        begin
          // get string between function name and opening parenthesis
          TmpS1 := Trim(Copy(TmpS, Tmp1, LeftPar - Tmp1));

          if (TmpS1 = '') then // no characters between function name and opening parenthesis
          begin
          // function call is really there
            Result := true;
            Func_ := SimpleEvalFunctions[I];
            B := P;
            E := RightPar;
            Exit; // try to find only the first function call in string S
          end else

          begin // Error: some characters are there between function name and opening parenthesis
            raise ESimpleEvalException.CreateFmt('"(" expected but found "%s" for function "%s" in expression "%s"', [TmpS1, SimpleEvalFunctions[I], TmpS]);
          end;
        end else

        begin // Error: no (matching) parenthesis found!
          if (LeftPar = 0) // left parenthesis is missing (or both are)
            then raise ESimpleEvalException.CreateFmt('"(" expected but none found for function "%s" in expression "%s"', [SimpleEvalFunctions[I], TmpS])
          else raise ESimpleEvalException.CreateFmt('")" expected but none found for function "%s" in expression "%s"', [SimpleEvalFunctions[I], TmpS])
        end;

      end;

      P := PosN(SimpleEvalFunctions[I], TmpS, Tmp1); // check reminder of S for more function names
    end;
  end;

end;


// -------------------------------------------------------------------
// "ReplaceFunctionCalls" replaces all function calls in expression S
// by their actual (numerical) value
// -------------------------------------------------------------------

procedure TSimpleEval.ReplaceFunctionCalls(var S: string);
var Fnc, FncArg: string;
  FncBegin, FncEnd: Integer;
  TempR: real;
begin
  while HasFunction(S, Fnc, FncArg, FncBegin, FncEnd) do
  begin // replace the function part by its numerical value
    TempR := SolveFunction(Fnc, FncArg);
    S := LeftStr(S, FncBegin - 1) + ' ' + FloatToString(TempR) + ' ' + Copy(S, FncEnd + 1, MaxInt);
  end;
end;


// -------------------------------------------------------------------
// "SolveFunction" returns the numerical result of the
// function execution on its argument(s)
// -------------------------------------------------------------------

function TSimpleEval.SolveFunction(Func_, Args_: string): real;
var Argument: array of real;
  ArgS: TStringList;
  I: Integer;
begin
  Result := 0.0;
  Args_ := trim(Args_);

    {
    At this point "Args_" can have the following formats
     "a,b,c"  : simple list of arguments
     "(a,b,c)"  : simple list of arguments within matching parenthesis
     "a, func(b),c" :  some arguments are function calls
     "a, func(b,c,d),e" :  some arguments are function calls with more than 1 argument
     "(a, func(b,(c / 20),d),(e % f))" : all of the above with expressions in parenthesis
    }

  // first remove outer matching parenthesis (if any)
  RemoveOuterParenthesis(Args_);

  // then resolve function calls in the argument(s) (nested function calls)
  ReplaceFunctionCalls(Args_);

    {
    At this point "Args_" can have only the following formats
     "a,b,c"  : simple list of arguments
     "a,b,(c / 20),(e % f)" : all of the above with expressions in parenthesis
    }

  ArgS := TStringList.Create;

  try
    StringToTStrings(trim(Args_), ArgS);
    SetLength(Argument, ArgS.Count);

    // Calculate the actual value of the parameters
    for I := 0 to ArgS.Count - 1 do Argument[I] := EvalNumerical(ArgS[I]);

    {
    At this point all arguments have been resolved to their numerical value
    }


    // ------------------------------ //
    // Execute the actual function. //
    // ------------------------------ //

    // Its arguments are in Argument[0]..Argument[n-1]
    // (n = being the number of arguments)

    try

    // "LogN" returns the log base (Argument[0]) of (Argument[1]).
      if Func_ = 'logn' then
      begin
        Result := LogN(Argument[0], Argument[1]);
        Exit;
      end;

    // "Log" returns the log base 10 of Argument[0]
      if Func_ = 'log' then
      begin
        Result := Log10(Argument[0]);
        Exit;
      end;

    // "Power" returns the value of Argument[0] raised to the power of Argument[1],
      if func_ = 'power' then
      begin
        Result := Power(Argument[0], Argument[1]);
        Exit;
      end;

    // "Sum" returns the sum of its arguments
      if func_ = 'sum' then
      begin
        for I := 0 to Length(Argument) - 1 do
          Result := Result + Argument[I];
        Exit;
      end;

    // "Sqrt" returns the square root of Argument[0]
      if func_ = 'sqrt' then
      begin
        Result := sqrt(Argument[0]);
        Exit;
      end;

    // "Abs" returns the absolute value of Argument[0]
      if func_ = 'abs' then
      begin
        Result := abs(Argument[0]);
        Exit;
      end;

    // "Int" returns the integer part of Argument[0]
      if func_ = 'int' then
      begin
        Result := int(Argument[0]);
        Exit;
      end;

    // "Frac" returns the fractional part of Argument[0]
      if func_ = 'frac' then
      begin
        Result := frac(Argument[0]);
        Exit;
      end;

    // "Round" rounds Argument[0] to an integer-type value
      if func_ = 'round' then
      begin
        Result := round(Argument[0]);
        Exit;
      end;

    // "Ceil" returns the lowest integer greater than or equal to Argument[0]
      if func_ = 'ceil' then
      begin
        Result := ceil(Argument[0]);
        Exit;
      end;

    // "Floor" returns the highest integer less than or equal to
      if func_ = 'floor' then
      begin
        Result := floor(Argument[0]);
        Exit;
      end;

    // "Avg" returns the average of its arguments
      if func_ = 'avg' then
      begin
        for I := 0 to Length(Argument) - 1 do
          Result := Result + Argument[I];
        Result := Result / Length(Argument);
        Exit;
      end;

    // "Min" returns the smallest value of its arguments
      if func_ = 'min' then
      begin
        Result := Argument[0];
        for I := 1 to Length(Argument) - 1 do
          if Argument[I] < Result then Result := Argument[I];
        Exit;
      end;

    // "Max" returns the largest value of its arguments
      if func_ = 'max' then
      begin
        Result := Argument[0];
        for I := 1 to Length(Argument) - 1 do
          if Argument[I] > Result then Result := Argument[I];
        Exit;
      end;

    // "Pi" returns the value of PI (approximated as 3.1415926535897932385)
      if func_ = 'pi' then
      begin
        Result := Pi;
        Exit;
      end;

    // "Sign" returns
    //  0 if Argument[0] is zero;
    // +1 if Argument[0] is greater than zero and
    // -1 if Argument[0] is less than zero.
      if func_ = 'sign' then
      begin
        Result := sign(Argument[0]);
        Exit;
      end;

    // "Rad" returns Argument[0] as "radians"
      if func_ = 'rad' then
      begin
        Result := Pi / 180 * Argument[0];
        Exit;
      end;

    // "Deg" returns Argument[0] as "degrees"
      if func_ = 'deg' then
      begin
        Result := Argument[0] / Pi * 180;
        Exit;
      end;

    // "Cmp" compares two arguments and returns
    // -1 if Argument[0] < Argument[1]
    //  0 if Argument[0] = Argument[1]
    // +1 if Argument[0] > Argument[1]
      if func_ = 'cmp' then
      begin
        Result := CompareValue(Argument[0], Argument[1]);
        Exit;
      end;

    // "Ln" returns the natural logarithm (Ln(e) = 1) of Argument[0]
      if func_ = 'ln' then
      begin
        Result := ln(Argument[0]);
        Exit;
      end;

    // "Exp" returns the value of e raised to the power of Argument[0], where e is the base of the natural logarithms
      if func_ = 'exp' then
      begin
        Result := exp(Argument[0]);
        Exit;
      end;

    // "Sin" returns the sine of the angle Argument[0] in radians
      if func_ = 'sin' then
      begin
        Result := sin(Argument[0]);
        Exit;
      end;

    // "Cos" returns the cosine of the angle Argument[0] in radians
      if func_ = 'cos' then
      begin
        Result := cos(Argument[0]);
        Exit;
      end;

    // "Tan" returns the tangent of the angle Argument[0] in radians
      if func_ = 'tan' then
      begin
        Result := tan(Argument[0]);
        Exit;
      end;

    // "ArcSin" returns the arcsine of Argument[0]
      if func_ = 'arcsin' then
      begin
        Result := arcsin(Argument[0]);
        Exit;
      end;

    // "ArcCos" returns the arccosine of Argument[0]
      if func_ = 'arccos' then
      begin
        Result := arccos(Argument[0]);
        Exit;
      end;

    // "ArcTan" returns the arctangent of Argument[0]
      if func_ = 'arctan' then
      begin
        Result := arctan(Argument[0]);
        Exit;
      end;

    // "IfThen" checks the expression passed as Argument[0] and
    // returns Argument[1] if it evaluates to +1 or more,
    // or Argument[2] if it evaluates to 0
      if func_ = 'ifthen' then
      begin
        if Round(Argument[0]) >= 1
          then Result := Argument[1]
        else Result := Argument[2];
        Exit;
      end;

    // "Not" returns 1 when Argument[0] is 0 and vice versa
      if func_ = 'not' then
      begin
        if Round(Argument[0]) >= 1
          then Result := 0
        else Result := 1;
        Exit;
      end;

    // -----------------------------------------
    // End of the "Execute Actual Function" part
    // -----------------------------------------

    except
      on ERangeError
        do raise ESimpleEvalException.CreateFmt('Number of arguments for function "%s(%s)" too low (%d)', [func_, Args_, Length(Argument)]);
      on EInvalidOp
        do raise ESimpleEvalException.CreateFmt('Error in arguments for function "%s(%s)"', [func_, Args_, Length(Argument)]);
    end;

  finally
    ArgS.Free;
    SetLength(Argument, 0);
  end;
end;


// -------------------------------------------------------------------
// "RemoveOuterParenthesis" removes matching parenthesis if
// they are at the beginning and end of expression S
// -------------------------------------------------------------------

procedure TSimpleEval.RemoveOuterParenthesis(var S: string);
var Lp, Rp: Integer;
  TmpS: string;
begin
  while HasParenthesis(S, Lp, Rp, TmpS) and
    (Lp = 1) and
    (Rp = Length(S)) do S := Trim(TmpS);
end;


// -------------------------------------------------------------------
// "GetValue" returns the numerical value of the string S,
// or returns the value of the "variable" in S
// -------------------------------------------------------------------

function TSimpleEval.GetValue(S: string): real;
begin
  S := Trim(S);
  if S = '' then raise ESimpleEvalException.CreateFmt('Operand missing', []);

  if IsNumber(S)
    then Result := StrToFloat(S) // simple numerical value

  else
  if S[1] in ['+', '-'] then Result := EvalNumerical(S) // still an expression of some kind

  else
  begin  // named value (Variable)
    try
      Result := StrToFloat(FVars.Values[S]);
    except
      raise ESimpleEvalException.CreateFmt('Variable "%s" not defined', [S]);
    end;
  end;

end;




// -------------------------------------------------------------------
// "SetVar" sets the value of a variable used in an expression
// -------------------------------------------------------------------

procedure TSimpleEval.SetVar(Name: string; Value: real);
var I: Integer;
begin
  Name := Trim(Name);

  if not IsValidIdent(Name) then
  begin // not a variable valid name
    raise ESimpleEvalException.CreateFmt('Not a proper variable name: "%s"', [Name]);
    exit;
  end;

  I := FVars.IndexOfName(Name); // get index of the variable

  if I < 0 // variable does not exist, add it
    then FVars.Add(Name + '=' + FloatToString(Value))

  else // variable already exists,
       // replace the old value by the new one
  begin
    // replace the old value by the new one
    FVars[I] := Name + '=' + FloatToString(Value);
  end;
end;

procedure TSimpleEval.SetVar(Name: string; Value: string);
begin
  Value := trim(Value);
  if Value = '' then exit;
  try
    SetVar(Name, StrToFloat(value));
  except
    on EConvertError do raise ESimpleEvalException.CreateFmt('Parameter "%s" is not a proper numerical value: "%s"', [Name, Value]);
  end;
end;

// -------------------------------------------------------------------
// "ClearVars" removes all variables that were defined by "SetVar"
// -------------------------------------------------------------------

procedure TSimpleEval.ClearVars;
begin
  FVars.Clear;
end;

// -------------------------------------------------------------------

constructor TSimpleEval.Create;
begin
  inherited Create;
  FVars := TStringList.Create;
  FNoFunctions := false; // functions are default enabled
end;

destructor TSimpleEval.Destroy;
begin
  FVars.Free;
  inherited Destroy;
end;

end.
