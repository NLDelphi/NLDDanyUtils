unit NLDExecute;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use


{ History of this unit
  13-07-2004: * Initial version, with thanks for the idea's to:
              	- PsychoMark for "CaptureConsoleOutput" and
              	- many others (a.o. Google) for "WinExec32AndWait"
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

function CaptureConsoleOutput(const ACommand: string): string;
// Executes console application "ACommand" and returns its output in the function result

function WinExec32AndWait(const Cmd: string; const CmdShow: Integer): Cardinal;
// Executes "Cmd" and waits until it is finished. Returns the exitcode of "Cmd" or
// $FFFFFFFF if not successfull.


{ Possible values for the "CmdShow" parameter of "WinExec32AndWait":
SW_HIDE	           Hides the window and activates another window.
SW_MAXIMIZE	   Maximizes the specified window.
SW_MINIMIZE	   Minimizes the specified window and activates the next top-level window in the Z order.
SW_RESTORE	   Activates and displays the window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when restoring a minimized window.
SW_SHOW	           Activates the window and displays it in its current size and position.
SW_SHOWDEFAULT	   Sets the show state based on the SW_ flag specified in the STARTUPINFO structure passed to the CreateProcess function by the program that started the application.
SW_SHOWMAXIMIZED   Activates the window and displays it as a maximized window.
SW_SHOWMINIMIZED   Activates the window and displays it as a minimized window.
SW_SHOWMINNOACTIVE Displays the window as a minimized window. The active window remains active.
SW_SHOWNA	   Displays the window in its current state. The active window remains active.
SW_SHOWNOACTIVATE  Displays a window in its most recent size and position. The active window remains active.
SW_SHOWNORMAL	   Activates and displays a window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when displaying the window for the first time.
}

implementation

uses Windows, Forms;

function CaptureConsoleOutput(const ACommand: string): string;
const
  CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array[0..CReadBuffer] of Char;
  dRead: DWord;
  total: DWord;
  dRunning: DWord;

begin
  Result := '';

  with saSecurity do
  begin
    nLength := SizeOf(TSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
  begin
    FillChar(suiStartup, SizeOf(TStartupInfo), #0);

    with suiStartup do
    begin
      cb := SizeOf(TStartupInfo);
      hStdInput := hRead;
      hStdOutput := hWrite;
      hStdError := hWrite;
      dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow := SW_HIDE;

      if CreateProcess(nil, PChar(ACommand), @saSecurity, @saSecurity, True,
        NORMAL_PRIORITY_CLASS, nil, nil, suiStartup,
        piProcess) then
      begin
        WaitForInputIdle(piProcess.hProcess, 2000);

        repeat
          dRunning := WaitForSingleObject(piProcess.hProcess, 100);
          Application.ProcessMessages();
          repeat
            PeekNamedPipe(hRead, nil, 0, nil, @total, nil);
            if (total > 0) then
            begin
              ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
              pBuffer[dRead] := #0;
              OemToAnsi(pBuffer, pBuffer);
              Result := Result + string(pBuffer);
            end;
          until (total = 0);
        until (dRunning <> WAIT_TIMEOUT);

        CloseHandle(piProcess.hProcess);
        CloseHandle(piProcess.hThread);
      end;
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
  end;
end;

function WinExec32AndWait(const Cmd: string; const CmdShow: Integer): Cardinal;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  Running : DWord;
begin
  Result := Cardinal($FFFFFFFF);
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := CmdShow;
  if CreateProcess(nil, PChar(Cmd), nil, nil, False, NORMAL_PRIORITY_CLASS,
    nil, nil, StartupInfo, ProcessInfo) then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    Repeat
    Running := WaitForSingleObject(ProcessInfo.hProcess, 100);
    Application.ProcessMessages();
    until Running = WAIT_OBJECT_0;
      if not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
        Result := Cardinal($FFFFFFFF);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

end.
