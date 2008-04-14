unit NLDRcsI2c;

// Dany Rosseel

{.$DEFINE NoDebug}// Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ Parallel port:
  SCL out = "control output" bit 0 (inverted)     = pin 1  via invertor (transistor)
  SDA out = "control output" bit 1 (inverted)     = pin 14 via invertor (transistor)
  SCL in =  "status input"   bit 3 (not inverted) = pin 15
  SDA in =  "status input"   bit 4 (not inverted) = pin 13

  See shematic diagram.
}

{ History of this unit:
2008-01-20: * Initial version
}

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

{ Property "Error":
  0 = No error
  1 = I2c bus not present
  2 = No Acknowledge from slave
  3 = SCL Low timeout (SCL held low too long, > property SCLTimeOut)
}

interface
uses ExtCtrls, Forms;

const
  // Error Definitions
  I2C_NO_ERROR = 0;
  I2C_NO_I2C_BUS = 1;
  I2C_NO_ACK_FROM_SLAVE = 2;
  I2C_SCL_TIMEOUT = 3;

type TLevel = 0..1;

  TI2c = class
    // private data

    FBaseAddress, StatusRegister, ControlRegister: word;
    FI2cBusPresent: boolean;
    FSCLSDAOut: byte;
    FRepeats: word;
    FSCLTimer: TTimer;
    FSCLTimeOut: Word;                            // SCL timeout in milliseconds
    FInitialSpeed, FSpeed: real;
    FError: Integer;

    // private procedures
    procedure SetSCL(Level: TLevel);
    procedure SetSDA(Level: TLevel);
    function GetSCL: TLevel;
    function GetSDA: TLevel;
    procedure SCLTimerOut(Sender: TObject);
    function WaitForSclHigh: TLevel;              // 1 = ok, 0 is SCL timeout
    procedure SendStart;
    procedure SendStop;
    procedure SendBit(Level: TLevel);
    function GetBit: TLevel;
    procedure SendAck(Ack: boolean);
    function GetAck: boolean;
    function SendByte(B: byte): boolean;          // acknowledge returned by function
    function GetByte(Ack: boolean): byte;         // acknowkedge is sent by function

    public
    // public procedures and properties
    constructor Create(Base: word);
    destructor Destroy; override;
    procedure SendSingleByte(Addr, Value: byte);
    function GetSingleByte(Addr: byte): byte;
    function SendBytes(Addr: byte; Values: array of byte; Count: Integer = -1; Stop: boolean = true): word; // function returns No of bytes sent
    function GetBytes(Addr: byte; var Values: array of byte; Count: Integer = -1): word; // function returns No of bytes received
    procedure SendReadBytes(Addr: byte; SendBuff: array of byte; var ReadBuff: array of byte; SendCount: Integer = -1; ReadCount: Integer = -1);

    property InitialSpeed: real read FInitialSpeed; // initial bus speed before correction for the PC speed
    property Speed: real read FSpeed;             // bus speed after correction for the PC speed
    property I2cBusPresent: boolean read FI2cBuspresent;
    property SCLTimeOut: word read FSCLTimeOut write FSCLTimeOut; // SCL timeout in milliseconds
    property Error: Integer read FError;
  end;

implementation

uses NLDRcsTiming;

function Inp32(portaddr: word): byte; stdcall; external 'InpOut32.dll';

procedure Outp32(portaddr: word; data: byte); stdcall; external 'InpOut32.dll' name 'Out32';

const
  // Parallel Port Status bits
  SCLIn = 8;                                      // bit 3
  SDAIn = 16;                                     // bit 4

  // Parallel Port Control bits
  SCLOut = 1;                                     // bit 0
  SDAOut = 2;                                     // bit 1

var Timer1: TPerformanceTimer;

constructor TI2c.Create(Base: word);

  function GetSpeed: real;                        // bus speed in KHz
  var I: byte;
    Tmp: real;
  begin
    Result := 0;
    for I := 1 to 10 do
    begin
      Timer1.Reset;
      Timer1.Start;
      SendByte(255);
      Timer1.Stop;
      Tmp := 1 / (Timer1.Value * 1000 / 9);       // KHz
      if Tmp > Result then Result := Tmp;
    end;
  end;

begin
  FError := I2C_NO_ERROR;

  FBaseAddress := Base;
  StatusRegister := FBaseAddress + 1;                       // status register
  ControlRegister := FBaseAddress + 2;                       // Control Register

  FSCLSDAOut := $FF;

  // Create the SCL timeout timer
  FSCLTimer := TTimer.Create(nil);
  FSCLTimeOut := 1000;                            // default SCL Timeout value
  FSCLTimer.Enabled := false;
  FSCLTimer.Interval := FSCLTimeOut;
  FSCLTimer.OnTimer := SCLTimerOut;

  // Test if the I2c bus behaves normal
  FI2cBusPresent := true;
  FRepeats := 100;                                 // start (very) slow

  SetSCL(0);
  if GetSCL = 1 then
  begin
    FI2cBusPresent := false;
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  SetSCL(1);
  if WaitForSclHigh = 0 then
  begin
    FI2cBusPresent := false;
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;


  // measure and adjust the speed of the bus (if too fast)

  Timer1 := TPerformanceTimer.Create();
  FRepeats := 0;                                  // start fastest, no repeats
  FSpeed := GetSpeed;
  FInitialSpeed := FSpeed;
  while (FSpeed > 100.0) and (FError = I2C_NO_ERROR) do
  begin
    Inc(FRepeats);
    FSpeed := GetSpeed;
  end;
  Timer1.Free;
  SendStop;
end;

destructor TI2c.Destroy;
begin
  if FI2cBusPresent then SendStop;
  FSCLTimer.Enabled := false;
  FSCLTimer.Free;
end;

procedure TI2c.SetSCL(Level: TLevel);
var I: word;
begin
  case Level of
    0: FSCLSDAOut := FSCLSDAOut and lo(not SCLOut);
    1: FSCLSDAOut := FSCLSDAOut or SCLOut;
  end;
  for I := 0 to FRepeats do Outp32(ControlRegister, FSCLSDAOut);
end;

procedure TI2c.SetSDA(Level: TLevel);
var I: word;
begin
  case Level of
    0: FSCLSDAOut := FSCLSDAOut and lo(not SDAOut);
    1: FSCLSDAOut := FSCLSDAOut or SDAOut;
  end;
  for I := 0 to FRepeats do Outp32(ControlRegister, FSCLSDAOut);
end;

function TI2c.GetSCL: TLevel;
var I: word;
begin
  Result := 0;
  for I := 0 to FRepeats do if Inp32(StatusRegister) and SCLIn > 0
    then Result := 1
    else Result := 0;
end;

function TI2c.GetSDA: TLevel;
var I: word;
begin
  Result := 1;
  for I := 0 to FRepeats do if Inp32(StatusRegister) and SDAIn > 0
    then Result := 1
    else Result := 0;
end;

function TI2c.WaitForSclHigh: TLevel;             // 1 = ok, 0 is SCL timeout
begin
  Result := GetSCL;
  if Result = 1 then Exit;                        // SCL is high now

  // start the SCL timeout timer
  FSCLTimer.Enabled := True;                      // start the SCL Timer
  repeat
    Application.HandleMessage;
  until (GetSCL = 1) or (not FSCLTimer.Enabled);

  FSCLTimer.Enabled := False;                     // stop timer

  Result := GetSCL;                               // get current SCL
  if Result = 0 then FError := I2C_SCL_TIMEOUT;   // scl timeout
end;

procedure TI2c.SendStart;
begin
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  // make both SCL and SDA high
  SetSDA(1);
  SetSCL(1);
  if WaitForSclHigh = 0 then Exit;                // SCL could not be made high, abort

  // make SDA low
  SetSDA(0);
end;

procedure TI2c.SCLTimerOut(Sender: TObject);
begin
  FSCLTimer.Enabled := false;
end;

procedure TI2c.SendStop;
begin
  if not FI2cBusPresent then Exit;

  SetSCL(0);                                      // make SCL low
  SetSDA(0);                                      // make SDA low
  SetSCL(1);                                      // make SCL high
  if WaitForSclHigh = 0 then Exit;                // SCL could not be made high, abort
  SetSDA(1);                                      // make SDA high
end;

procedure TI2c.SendBit(Level: TLevel);
begin
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  SetScl(0);                                      // make SCL low
  SetSDA(Level);
  SetScl(1);                                      // make SCL high
  if WaitForSclHigh = 0 then Exit;                // SCL could not be made high, abort
  SetScl(0);                                      // make SCL low
end;

function TI2c.GetBit: TLevel;
begin
  Result := 0;
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  SetScl(0);                                      // make SCL low
  SetScl(1);                                      // make SCL high
  if WaitForSclHigh = 0 then Exit;                // SCL could not be made high, abort
  Result := GetSDA;
  SetScl(0);                                      // make SCL low
end;

procedure TI2c.SendAck(Ack: boolean);
begin
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  if Ack
    then SendBit(0)                               // ack
  else SendBit(1);                                // nack
  SetSDA(1);
end;

function TI2c.GetAck: boolean;
begin
  Result := false;
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  SetSDA(1);
  Result := GetBit = 0;
end;

function TI2c.SendByte(B: byte): boolean;         // acknowledge returned
var I: byte;
  Mask: word;
begin
  Result := false;
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  Mask := 128;
  for I := 0 to 7 do                              // send 8 bits
  begin
    if B and Mask > 0
      then SendBit(1)
    else SendBit(0);
    Mask := Mask div 2;                           // next bit
  end;
  Result := GetAck;                               // get acknowledge
end;

function TI2c.GetByte(Ack: boolean): byte;        // acknowkedge is sent
var I: byte;
begin
  Result := $FF;
  if not FI2cBusPresent then
  begin
    FError := I2C_NO_I2C_BUS;
    Exit;
  end;

  Result := 0;
  for I := 0 to 7 do                              // get 8 bits
  begin
    Result := Result shl 1;
    Result := Result + GetBit;
  end;
  SendAck(Ack);                                   // send ack or nack
end;

// -------------------- Public Procedures ------------------- //

procedure TI2c.SendSingleByte(Addr, Value: byte);
var Ack: boolean;
begin
  if FI2cBusPresent then
  begin
    FError := I2C_NO_ERROR;
    SendStart;
    Ack := SendByte(Addr and $FE);                // send address
    if Ack
      then SendByte(Value)
    else FError := I2C_NO_ACK_FROM_SLAVE;
    SendStop;
  end
  else FError := I2C_NO_I2C_BUS;
end;

function TI2c.GetSingleByte(Addr: byte): byte;
var Ack: boolean;
begin
  Result := $FF;
  if FI2cBusPresent then
  begin
    FError := I2C_NO_ERROR;
    SendStart;
    Ack := SendByte(Addr or 1);                   // read address
    if Ack
      then Result := GetByte(false)
    else FError := I2C_NO_ACK_FROM_SLAVE;
    SendStop;
  end
  else FError := I2C_NO_I2C_BUS;
end;

function TI2c.SendBytes(Addr: byte; Values: array of byte; Count: Integer = -1; Stop: boolean = true): word;
var Ack: boolean;
  I, Hgh: Integer;
begin
  Result := 0;
  if FI2cBusPresent then
  begin
    FError := I2C_NO_ERROR;
    if Count = 0 then Exit;

    if Count = -1                                 // no number of bytes given, send the whole array
      then Hgh := High(Values)
    else Hgh := Count - 1;

    if Hgh > High(Values) then Hgh := High(Values);

    I := 0;
    SendStart;
    Ack := SendByte(Addr and $FE);                // send address
    if not Ack then FError := I2C_NO_ACK_FROM_SLAVE;

    while Ack and (I <= Hgh) do
    begin
      Ack := SendByte(Values[I]);
      Inc(I);
      Inc(Result);
    end;
    if Stop then SendStop;
  end
  else FError := I2C_NO_I2C_BUS;
end;

function TI2c.GetBytes(Addr: byte; var Values: array of byte; Count: Integer = -1): word; // function returns the number of bytes received
var Ack: boolean;
  I, Hgh: Integer;
begin
  Result := 0;
  if FI2cBusPresent then
  begin
    FError := I2C_NO_ERROR;

    if Count = 0 then Exit;

    if Count = -1                                 // no number of bytes given, fill the whole array
      then Hgh := High(Values)
    else Hgh := Count - 1;

    if Hgh > High(Values) then Hgh := High(Values);

    SendStart;
    Ack := SendByte(Addr or 1);                   // read address

    if Ack then
    begin
      for I := 0 to Hgh do
      begin
        if I < Hgh
          then Values[I] := GetByte(true)
        else Values[I] := GetByte(false);
        Inc(Result);
      end;
    end else FError := I2C_NO_ACK_FROM_SLAVE;
    SendStop;
  end
  else FError := I2C_NO_I2C_BUS;
end;

procedure TI2c.SendReadBytes(Addr: byte; SendBuff: array of byte; var ReadBuff: array of byte; SendCount: Integer = -1; ReadCount: Integer = -1);
begin
  SendBytes(Addr, SendBuff, SendCount, ReadCount = 0); // only stop condition if no bytes are to be read
  if Ferror = I2C_NO_ERROR
    then GetBytes(Addr, ReadBuff, ReadCount)
  else SendStop;
end;

end.
