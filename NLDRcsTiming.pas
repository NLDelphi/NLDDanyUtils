unit NLDRcsTiming;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit:
12-02-2005: * Initial version
19-02-2005: * Added the Minimum and Maximum functions
13-05-2005: * Added a name to the TPerformanceTimer class
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

type
TPerformanceTimer = Class
  private
    FName  : string; // Timer Name
    FTimer : Int64;  // total timer value
    FStart : Int64;  // interval start moment
    FCount : Int64;  // # of start/stop intervals
    FMin   : Int64;  // minimum interval time
    FMax   : Int64;  // maximum interval time
    FFreq  : Int64;  // # of counts per second
  public
    constructor Create(Name: string = '');
    procedure Reset;
    procedure Start;
    procedure Stop;
    function Value: Extended;
    function Count: Int64;
    function Average: Extended;
    function Maximum: Extended;
    function Minimum: Extended;
    property Name: string read FName write FName;
end;

implementation

uses Windows, SysUtils, Math;

constructor TPerformanceTimer.Create(Name: string = '');
begin
  if Name = ''
  then FName  := 'PerformanceTimer x'
  else FName  := Name;
  FTimer := 0;
  FCount := 0;
  FMin   := 0;
  FMax   := 0;
  QueryPerformanceFrequency(FFreq);
  inherited Create;
end;

procedure TPerformanceTimer.Reset;
begin
  FTimer := 0;
  FCount := 0;
end;

procedure TPerformanceTimer.Start;
begin
  Inc(FCount);
  QueryPerformanceCounter(FStart);
end;

procedure TPerformanceTimer.Stop;
var V : Int64;
begin
  QueryPerformanceCounter(V);
  V := V - FStart;
  Inc(FTimer, V);                                // Increment timer value
  if V > FMax then FMax := V;                    // Current Interval = max?
  if (V < FMin) or (FMin = 0) then FMin := V;    // Current Interval = min?
end;

function TPerformanceTimer.Value: Extended;
begin
  if FFreq > 0
  then Result := FTimer / FFreq
  else Result := 0;
end;

function TPerformanceTimer.Count: Int64;
begin
  Result := FCount;
end;

function TPerformanceTimer.Average: Extended;
begin
  if FCount > 0
  then Result := Value / FCount
  else Result := 0;
end;

function TPerformanceTimer.Maximum: Extended;
begin
  if FFreq > 0
  then Result := FMax / FFreq
  else Result := 0;
end;

function TPerformanceTimer.Minimum: Extended;
begin
  if FFreq > 0
  then Result := FMin / FFreq
  else Result := 0;
end;

end.
