unit NLDExecProcedure;

// Dany Rosseel

{$DEFINE NoDebug} // Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use


{ History of this unit
  07-06-2002: * Initial version, mamed "exec_pro"
  06-10-2003: * Renamed the unit to 'ExecProcedure'
              * Renamed functions, variables etc. to meet naming conventions
  17-12-2003: * Optimized with respect to directory switching
  24-07-2004: * Added "ExecuteProcedureInPath"
  28-08-2005: * Added the possibility to also dive into Hidden and System
                Directories with "ExecuteProcedure".
  06-11-2005: * Added an overloaded version of "ExecuteProcedure" that takes as parameter a
                procedure of the signature "procedure(var cont, Brk: Boolean)". The extra "Brk" 
                var parameter allows to prevent going down further in the directory tree.
  12-11-2009: * "ExecProcedure" also executes recursively in R/O directories now
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
  TProcedureToExecute = procedure(var cont: Boolean);
  TProcedureToExecute1 = procedure(var cont, Brk: Boolean);

procedure ExecuteProcedure(Proc: TProcedureToExecute; const Recursive: Boolean; var
  Cont: Boolean; const HiddenDirs: boolean = false; const SystemDirs: boolean = false); overload;
// Executes a user defined procedure in the current directory and if wanted also in
// all subdirectories until it returns cont(inue) = false (or all directories are done).

procedure ExecuteProcedure(Proc: TProcedureToExecute1; const Recursive: Boolean; var
  Cont: Boolean; var Brk: Boolean; const HiddenDirs: boolean = false; const SystemDirs: boolean = false); overload;
// Executes a user defined procedure in the current directory and if wanted also in
// all subdirectories until it returns cont(inue) = false (or all directories are done).
// "Brk" can be set to "true" by the executed procedure to prevent going down further in
// the directory tree.

procedure ExecuteProcedureInPath(Proc: TProcedureToExecute; var Cont: Boolean);
// Same as the above, but the user defined procedure is executed in every directory
// of the "PATH" environment variable. The current directory is always first.

implementation

uses NLDRcsFileUtils, SysUtils, Classes, NLDRcsEnvironmentVars;

// The procedure below executes the procedure "Proc" in the current directory
// (and recursively in its subdirs if "Recursive" is true) as long as "Cont" is true.
// The procedure "Proc" is supposed to set "Cont" to false if further execution down
// the subdirectorystructure is not needed any more.

// To use this procedure You
//   - have to create a boolean variable to hold "Cont"
//   - have to create your procedure to be executed("Proc"):
//     it is of signature "TProcedureToExecute"
//   - have to change the current directory to the one to start the execution of "Proc" in
//   - have to call "ExecuteProcedure" with as
//      * first parameter: the name of the procedure to execute ("Proc"),
//      * the second parameter set to "true" (recursive execution) or "false"
//        (only execution in the current directory)
//      * your boolean variable as the third parameter with the value set to "true".

procedure ExecuteProcedure(Proc: TProcedureToExecute; const Recursive: Boolean; var
  Cont: Boolean; const HiddenDirs: boolean = false; const SystemDirs: boolean = false);
var
  List: TFileList;
  I, N: Word;
  Rec: TSearchrec;
  ParentDirectory: string;
  Attr: byte;
begin
  if Cont then
  begin
    Proc(Cont); // execute the "procedure" in the current directory

    if (Recursive and Cont) then // If it has to be executed in the subdirs then
    begin
      ParentDirectory := GetCurrentDir; // Get current directory name
      List := TFileList.Create;
      try
        with List do
        begin
          Attr := faDirectory or faNormal or faReadOnly; // we seek for "normal" directories, can be R/O
          if HiddenDirs then Attr := Attr or faHidden;   // also hidden ones
          if SystemDirs then Attr := Attr or faSysFile;  // or System ones

          MakeFileList('*.*', Attr, acRelaxed); // Make list of directories

          N := Count;
          I := 1;

          while (I <= N) and Cont do // For all subdirs (and as long as "Cont" is valid)
          begin
            GetEntry(I, Rec);
            Chdir(IncludeTrailingBackslash(ParentDirectory) + Rec.Name);     // Change to the subdir
            ExecuteProcedure(Proc, Recursive, Cont, HiddenDirs, SystemDirs); // Execute the "procedure" in the subdir
            Inc(I); // next directory in the list
          end;

        end;
      finally
        List.Free;
      end;
      Chdir(ParentDirectory); { change back to the original one }
    end;
  end;
end;

procedure ExecuteProcedure(Proc: TProcedureToExecute1; const Recursive: Boolean; var
  Cont: Boolean; var Brk: Boolean; const HiddenDirs: boolean = false; const SystemDirs: boolean = false);
var
  List: TFileList;
  I, N: Word;
  Rec: TSearchrec;
  ParentDirectory: string;
  Attr: byte;
begin
  if Cont then
  begin
    Brk := false;
    Proc(Cont, Brk); // execute the "procedure" in the current directory

    if Recursive and
       Cont and
       (not Brk) then // If it has to be executed in the subdirs then
    begin
      ParentDirectory := GetCurrentDir; // Get current directory name
      List := TFileList.Create;
      try
        with List do
        begin
          Attr := faDirectory or faReadOnly or faNormal; // we seek for "normal" directories, can be R/O
          if HiddenDirs then Attr := Attr or faHidden;   // also hidden ones
          if SystemDirs then Attr := Attr or faSysFile;  // or System ones

          MakeFileList('*.*', Attr, acRelaxed); // Make list of directories

          N := Count;
          I := 1;

          while (I <= N) and Cont do // For all subdirs (and as long as "Cont" is valid)
          begin
            GetEntry(I, Rec);
            Chdir(IncludeTrailingBackslash(ParentDirectory) + Rec.Name);     // Change to the subdir
            {if (not (ParentDirectory = 'C:\Documents and Settings\DanyR')) then}
            ExecuteProcedure(Proc, Recursive, Cont, Brk, HiddenDirs, SystemDirs); // Execute the "procedure" in the subdir
            Inc(I); // next directory in the list
          end;

        end;
      finally
        List.Free;
      end;
      Chdir(ParentDirectory); { change back to the original one }
    end;
  end;
end;

procedure ExecuteProcedureInPath(Proc: TProcedureToExecute; var Cont: Boolean);
var
  Paths: TStrings;
  I: Integer;
  Tmp: string;
  OrgDir: string;
begin
  OrgDir := GetCurrentDir;
  Paths := TStringList.Create;
  try
    GetPATHEnvironmentVar(Paths);
    Paths.Insert(0, '.');
    I := 0;
    while (I < (Paths.Count)) and Cont do
    begin
      Tmp := ExpandFileName(Paths[I]);
      if DirectoryExists(Tmp) then
      begin
        ChDir(Tmp);
        Proc(Cont); // execute the "procedure" in the current directory
      end;
      inc(I); // next path
    end;
  finally
    Paths.Free;
    ChDir(OrgDir);
  end;
end;

end.
