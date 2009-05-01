unit NLDRcsFileUtils;

// Dany Rosseel

{.$DEFINE NoDebug}// Disable debug possibilities and range checking (= faster)
// {.$Define NoDebug}: During debugging
// {$Define NoDebug} : During "normal" use

{ History of this unit:
  03-11-2002: * Initial version
  12-10-2003: * Adaptions made to meet coding conventions
  09-11-2003: * Small (non functional) text adaptations
              * Left out the usage of 'RcsStrings'
  11-01-2004: * Extended several procedures
                (TFileList.MakeFileList, FileNameExists, GetFileNames)
                - with recursivity (actions in subdirs also)
                - to provide a full path (starting with 'C;\...') or a relative path
                  (with respect to the starting directory) in the filenames:
              * Removed the function 'ExtendedFileAttribute' from the interface section
  08-05-2004: * Made the 'GetfileNames' procedure also accept more paths
                (file names in a TStrings) to search for.
  16-05-2004: * Called "ProcessMessages" in "MakefileList" for every directory
  06-06-2004: * Used a "TList" as parent class now for the "TFileList" class
  19-06-2004: * Splitted up the file "attributes" in a "Kind" and a "Status"
                (Makes it more logical to define those attributes)
  14-07-2004: * Changed "FileNameExists": returns now the found filename.
                Uses also "MakeFileList" now in stead of copying that code.
              * The function "ExistingFileName" has been removed (see above).
              * "MakeFileList" accepts one more (defaulted to MaxInt) parameter: MaxNumber.
                The parameter indicates the maximum number of entries allowed in the list
              * Removed the "NumberOfEntries" function, use "Count" instead.
                Indexes still range from 1 to Count (as before)!!!!!
              * Added the "NoDebug" define (see above)
  18-07-2004: * An explicite "Chdir" before calling "MakeFileList", "GetFileNames"
                and "FileNameExists" is not needed any longer:
                the (root) directory can be added in the filename (parameter "Fn") now.
                So:
                  MakeFileList ('xxx\*.pas', ...   // new way
                is exactly the same as:
                  ChDir('xxx');                    // old way
                  MakeFileList('*.pas', ...
                The "old" way still works however.
  21-07-2004: * Added the function "DirectoryNameExists".
                It resolves wildcards in a directoryname.
                All other functions in this unit make use of it now,
                so the (root) directory can contain wildcards now.
  24-07-2004: * Added the function "ExpandToLongPathName", the opposite of
                "ExtractShortPathName".
  27-07-2004: * Made "MakeFileList" a little faster.
  29-07-2004: * The recursive search is made also in hidden en system directories again.
  06-02-2005: * Added "const" in a few function/procedure parameterlists
  10-02-2005: * Changed "TFileList.Create" to start with place for 1000 entries
  04-12-2005: * Added an overloaded procedure for "MakeFileList": the first
                parameter (the file to be found) is a "list" of files to be found
                (TStrings) here.
  14-12-2005: * Extended the overloaded version of "MakeFileList"and "GetFileNames"
                with parameter "Excluded" (TStrings). All filenames (and directorynames)
                in this list will be ignored.
  26-07-2006: * Added filekind "faAnyKind" and FileStatus "faAnyStatus"
  05-08-2007: * Added a "RootDir" parameter to most procedures and/or functions: is the "root" the
                action of the procedure/function starts in (e.g. "GetFileNames").
                If "RootDir" is not defined it is derived from eg the filename
                to find, and if that fails it is set to the current directory.
}

(*
  The wanted file "Attr" parameter you can use to search for files are:

  1. The "Kind" of the files:
     - faFile       : "Normal" file (no directory or volumeid)
     - faDirectory  : Directories
     - faVolumeId   : VolumeId
     - faAnyKind    : all of the above
     The four kinds are mutual exclusive.

  2. The "Status" of the files:
     - faNormal     : "Normal" file (no Sysfile, not Hidden en not ReadOnly)
     - faReadOnly   : the file has the ReadOnly Status
     - faArchive    : the file has its archive bit set (to be backupped)
     - faHidden     : the file is a Hidden file
     - faSysFile    : the file is a SystemFile
     - faAnyStatus  : all of the above

  3: Special case : faAnyKind + faAnyFile
     - faAnyFile    : any file

  To search for files with combination faFile and faNormal (the "normal" files),
  the constant "faNormalFile" can be used instead of "faFile + faNormal".

  The file attribute you give with e.g. the "MakeFileList" procedure is an Integer
  with a combination of the above values:
  e.g. "faFile + faDirectory + faHidden + faSysFile".

  The check happens on both the "Kind" and the "Status" part separately: to
  be selected, both must be applicable. See however below for the strictness
  of the check done.
*)

(*
  The "Check" parameter you can use to search for files are:

  1. acStrict  : both the "Kind" and the "Status" part of the file attributes
                 and the wanted attributes must be an exact match

  2. acRelaxed : both the "Kind" and the "Status" part of the file attributes
                 must be a subset of those of the wanted attributes.
                 In the acRelaxed case the "faArchive" attribute is always ignored.
                 If one really wants to check on the "archive" attribute, then
                 the "acStrict" check should be applied.

  In case the wanted "attributes" is "faAnyFile", the attributecheck is
  always "acRelaxed".
*)

(*
  The "Includepath" parameter defines what format the filenames found have:
  -  ipNone     = no path information included, only the file name is returned
  -  ipRelative = the filename(s) include the path relative to the starting directory
  -  ipFull     = the filename(s) include the full path (e.g. 'C:\aaa\bbb\ccc....')
*)

(* The "Mode" the class TFileList is working in the separate functions work in has the following values:
  -  mdMask     = the normal file path/name mask is used (like in "*.pas" for pascal files)
  -  mdRegExp   = the regular expression mode is used (like in "\.pas$" for pascal files)
   The default is "mdMask"
*)

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

uses SysUtils,
  Classes;

const
  (* Kind of file *)
  faFile = $80;                                   {NOT VolumeID, NOT Directory}
  //faVolumeID  = $08;	Volume ID files
  //faDirectory	= $10;	Directory files
  //faAnyFile   = $7F;  Any File
  faAnyKind = faFile or faDirectory or faVolumeId;

  (* Status of file *)
  faNormal = $40;                                 {NOT ReadOnly, NOT Hidden, NOT SysFile}
  //faReadOnly  = $01;	Read-only files
  //faHidden    = $02;	Hidden files
  //faSysFile   = $04;	System files
  //faArchive	= $20;	Archive files
  faAnyStatus = faNormal or faReadOnly or faArchive or faHidden or faSysFile;

  (* "NormalFile" definition *)
  faNormalFile = faFile or faNormal;

  (* Bitsdefinition *)
  faFileKindBits = faFile or faDirectory or faVolumeId;
  faFileStatusBits = faNormal or faReadOnly or faArchive or faHidden or faSysFile;

type
  TAttributeCheck = (acStrict, acRelaxed);
  TIncludePath = (ipNone, ipRelative, ipFull);
  //TFileListMode = (mdMask, MdRegExp);

  TFileList =
    class(TList)
  private
    //TMode: TFileListMode;
    procedure DisposeFileList;
  public

    constructor Create;
    destructor Destroy; override;

    procedure MakeFileList(
      Fn: string;                                 // filename to find (rootdir is derived from it)
      Attr: Integer;                              //file attributes to find
      Check: TAttributeCheck;                     // strict or relaxed attribute check
      SubDirs: Boolean = false;                   // search in subdirs too
      IncludePath: TIncludePath = ipNone;         // include full path in the filenames found
      MaxNumber: Integer = MaxInt); overload;     // maximum number of entries in the list fo names found

    procedure MakeFileList(
      RootDir: string;                            // start directory
      Fn: string;                                 // filename to find
      Attr: Integer;                              // file attributes to find
      Check: TAttributeCheck;                     // strict or relaxed attribute check
      SubDirs: Boolean = false;                   // search in subdirs too
      IncludePath: TIncludePath = ipNone;         // include full path in the filenames found
      MaxNumber: Integer = MaxInt); overload;     // maximum number of entries in the list fo names found

    procedure MakeFileList(
      RootDir: string;                            // start directory
      FNames,                                     // list of filenames to find
      Excluded: Tstrings;                         // list of filenames to exclude
      Attr: Integer;                              // file attributes to find
      Check: TAttributeCheck;                     // strict or relaxed attribute check
      SubDirs: Boolean = false;                   // search in subdirs too
      IncludePath: TIncludePath = ipNone;         // include full path in the filenames found
      MaxNumber: Integer = MaxInt); overload;     // maximum number of entries in the list fo names found

    procedure AddToFileList(const Rec: TSearchrec);
    procedure GetEntry(const Index: Integer; var Rec: TSearchrec);
    procedure SortFileList;

    //property Mode: TFileListMode read TMode write TMode;
  end;

  {
  TFileList.MakeFileList:
   This procedure builds a list of 'TSearchRec' members, with attributes defined in the
   parameters. The "Attr" part of the TSearchRec members is extended with the extra
   attributes "faFile" and "faNormal".

   TFileList.MakeFileList parameters:
   - 'RootDir'     : The (top/root)directory the action starts in
                     if it is empty, an attempt is made to extract it from 'Fn',
                     if that fails then the current directory is used as RootDir
                     In functions/procedures where the parameter "RootDir" is not
                     present an attempt is made to derived from "Fn".
   - 'Fn'      (1) : The filename to be found, can contain wildcards
   - 'FNames'  (2) : a list of filenames to be found, can contain wildcards
   - 'Excluded'(2) : a list of filesnames (or directorynames) to exclude from the list
   - 'Attr'        : The file-attributes to be found
   - 'Check'       : Relaxed or strict checking of the file attributes (see the function 'Match')
   - 'SubDirs'     : True = also go into subdirs
   - 'IncludePath' : The path included in the filenames (see type 'TIncludepath')
   - 'MaxNumber'   : The maximum number of entries allowed in the list (default MaxInt)

      (1) and (2) are mutual exclusive
  }

{ FileNameExists
This function checks if file 'Fn' with attributes 'Attr' exists and returns its name.
If it does not exist, it returns ''.
All parameters have the same meaning as in 'TFileList.MakeFileList' (see above)
}
function FileNameExists(
  Fn: string;                                     // filename to check
  Attr: Integer;                                  // file attributes to find
  Check: TAttributeCheck;                         // strict or relaxed attribute check
  SubDirs: Boolean = false;                       // search in subdirs too
  IncludePath: TIncludePath = ipNone              // include full path in the filenames found
  ): string;


{ GetFileNames
This procedure gets filenames in a TStrings object (like a TMemo.Lines).
The directory to start in is extracted from "Fn". If that fails then the
current directory is used as RootDir.
If 'RemoveExtension' is set to 'True', the extensions are removed from the filenames
All other parameters have the same meaning as in 'TFileList.MakeFileList' (see above)
}
procedure GetFileNames(
  Fn: string;                                     // filename to find (the rootdir is derived from it)
  Attr: Integer;                                  // file attributes to find
  Check: TAttributeCheck;                         // strict or relaxed attribute check
  RemoveExtension: Boolean;                       // the extention of the filenames found is to be removed in the list
  List: TStrings;                                 // the search results (found filenames)
  Subdirs: Boolean = false;                       // search in subdirs too
  IncludePath: TIncludePath = ipNone;             // include full path in the filenames found
  MaxNumber: Integer = MaxInt                     // maximum number of entries in the list fo names found
  ); overload;


{ GetFileNames
This procedure gets filenames in a TStrings object (like a TMemo.Lines).
The filename search action starts in "RootDir". If Rootdir is empty then an attempt
is made to extract RootDir from "Fn". If that fails then the
current directory is used as RootDir.
If 'RemoveExtension' is set to 'True', the extensions are removed from the filenames
All other parameters have the same meaning as in 'TFileList.MakeFileList' (see above)
}
procedure GetFileNames(
  RootDir: string;                                // start directory
  Fn: string;                                     // filename to find
  Attr: Integer;                                  // file attributes to find
  Check: TAttributeCheck;                         // strict or relaxed attribute check
  RemoveExtension: Boolean;                       // the extention of the filenames found is to be removed in the List
  List: TStrings;                                 // the search results (found filenames)
  Subdirs: Boolean = false;                       // search in subdirs too
  IncludePath: TIncludePath = ipNone;             // include full path in the filenames found
  MaxNumber: Integer = MaxInt                     // maximum number of entries in the list fo names found
  ); overload;


{ GetFileNames
This procedure is identical to the above one, but one can give more paths to search
for in de TStrings variable 'FNames'.
Here if Rootdir is empty then the current directory is used as RootDir.
Additionally the files in "Excluded" are not in the filename list.
}
procedure GetFileNames(
  RootDir: string;                                // start directory
  FNames,                                         // list of filenames to find
  Excluded: Tstrings;                             // list of filenames to exclude
  Attr: Integer;                                  // file attributes to find
  Check: TAttributeCheck;                         // strict or relaxed attribute check
  RemoveExtension: Boolean;                       // the extention of the filenames found is to be removed in the List
  List: TStrings;                                 // the search results (found filenames)
  Subdirs: Boolean = false;                       // search in subdirs too
  IncludePath: TIncludePath = ipNone;             // include full path in the filenames found
  MaxNumber: Integer = MaxInt                     // maximum number of entries in the list fo names found
  ); overload;


{ DirectoryNameExists
This function checks if "DirName" exists ("DirName" can contain wildcards in any
part of it) and returns its (with resolved wildcards) name (with or without trailing backslash).
If the directory "DirName" does not exist, the function returns an empty string.
}
function DirectoryNameExists(DirName: string; const IncludeBackSlash: Boolean =
  true): string;


{ ExpandToLongPathName
This functions returns the "long" pathname of "ShortPath", with or without trailing backslash.
See "ExtractShortPathName" (in SysUtils) for the opposite function.
}
function ExpandToLongPathName(ShortPath: string; const IncludeBackSlash: Boolean
  = true): string;



implementation

uses Forms,
  NLDRcsStrings,
  ShellAPI,
  Masks;

{
the function below tests if fileattributes 'Wanted' and 'Actual' match
taken into account strict or relaxed checking
}

function Match(Wanted, Actual: Integer; Check: TAttributeCheck; var
  ExtendedAttr: Integer): Boolean;
var
  KindWanted, StatusWanted, KindActual, StatusActual: Integer;
begin

  Actual := Actual and $7F;                       // filter out any illegal attribute bits

  // filter out the "kind" of file and the "Status" of the file
  KindWanted := Wanted and faFileKindBits;
  KindActual := Actual and faFileKindBits;

  StatusWanted := Wanted and faFileStatusBits;
  StatusActual := Actual and faFileStatusBits;

  // add the faNormal and the faFile bits (default values)
  if KindWanted = 0 then KindWanted := faFile;
  if KindActual = 0 then KindActual := faFile;

  ExtendedAttr := 0;

  if check = acRelaxed then
  begin
    // faArchive does not matter, reset bits
    ExtendedAttr := StatusActual and faArchive;   // remember the archive bit
    StatusWanted := StatusWanted and (not Lo(faArchive));
    StatusActual := StatusActual and (not Lo(faArchive));
  end;

  if StatusWanted = 0 then StatusWanted := faNormal;
  if StatusActual = 0 then StatusActual := faNormal;

  ExtendedAttr := ExtendedAttr or KindActual or StatusActual;

  Result := (Wanted = faAnyFile);
  if Result then exit;                            // faAnyFile is always accepted

  // do the check for a match
  if check = acRelaxed then
  begin
    // test if the actual value is a subset of the wanted value,
    // ("Wanted" is to be treated as "Allowed")
    // both for the "Kind" of file and the "Status" of the file
    Result := ((KindActual and KindWanted) = KindActual) and
      ((StatusActual and StatusWanted) = StatusActual);
    Exit;
  end;

  // Check = acStrict
  // Test if the actual value matches exactly the wanted value,
  // both for the "Kind" of file and the "Status" of the file
  Result := (KindActual = KindWanted) and
    (StatusActual = StatusWanted);
end;

function FileNameExists(Fn: string; Attr: Integer; Check: TAttributeCheck;
  SubDirs: Boolean = false; IncludePath: TIncludePath = ipNone): string;
var
  List: TFileList;
  Rec: TSearchRec;
begin
  Result := '';
  List := TFileList.Create;
  try
    List.MakeFileList('', Fn, Attr, Check, Subdirs, IncludePath, 1);
    // only one file name to search for
    if List.Count > 0 then
    begin                                         // one filename found that matches the criteria
      List.GetEntry(1, Rec);
      Result := Rec.Name;
    end;
  finally
    List.Free;
  end;
end;

{ ***** routines to make and access a table of file entries ***** }

// the procedure below creates an empty filelist

constructor TFileList.Create;
begin
  inherited Create;
  Self.Capacity := 1000;
  //TMode := mdMask;
end;

// the procedure below frees the memory of all items pointed to in the list
// the result is an empty filelist again

procedure TFileList.DisposeFileList;
var
  I: Integer;
  P: ^TSearchrec;
begin
  for I := 0 to Count - 1 do
  begin
    P := Self[I];                                 // get the pointer out of the list
    Dispose(P);                                   // free memory of object pointed to
  end;
  Self.Clear;
end;

// the procedure below destroys the filelist

destructor TFileList.Destroy;
begin
  DisposeFileList;                                // dispose of all the memory pointed to by the pointers
  // and clear the list itself
  inherited Destroy;
end;

// the procedure below gets the item with index 'Index' from the filelist
// Index ranges from 1 to Count !!!!!!!!

procedure TFileList.GetEntry(const Index: Integer; var Rec: TSearchrec);
var
  Tmp: ^TSearchrec;
begin
  if (Index >= 1) and
    (Index <= Count) then
  begin
    Tmp := Self[Index - 1];
    Rec := Tmp^;
  end
  else
  begin                                           // Entry not found: return an empty one
    Rec.Name := '';
    Rec.Attr := 0;
    Rec.Size := 0;
    Rec.Time := 0;
  end;
end;

// the procedure below adds one record to the filelist

procedure TFileList.AddToFileList(const Rec: TSearchrec);
var
  P: ^TSearchrec;
begin
  New(P);                                         // create a new TSearchRec space, P points to it
  P^ := Rec;                                      // fill it with the contents of "Rec"
  Add(P);                                         // Add the pointer to the list
  Expand;
end;

// the procedure below fills the filelist with all filenames matching the
// name, attribute and acStrict or acRelaxed

procedure TFileList.MakeFileList(
  RootDir: string;                                // Start directory
  FNames,                                         // list of filenames to find
  Excluded: Tstrings;                             // list of filenames to exclude
  Attr: Integer;                                  // file atrributes to find
  Check: TAttributeCheck;                         // Strict or Relaxed check
  SubDirs: Boolean = false;                       // search also into subdirs
  IncludePath: TIncludePath = ipNone;             // include full path in the filename found
  MaxNumber: Integer = MaxInt);                   // maximum entries in the filename list allowed
var

  I: integer;

  function ToBeIgnored(Fn: string): boolean;
  var I: integer;
  begin
    Result := false;
    if Excluded <> nil then
    begin
      Fn := ExcludeTrailingBackSlash(Fn);
      for I := 0 to Excluded.Count - 1 do
      begin
        Result := MatchesMask(Fn, Excluded[I]);
        if Result then break;
      end;
    end;
  end;

  procedure AddFilesToList(Directory: string);
  var
    SRec: TSearchrec;
    Res: Integer;
    Dir: string;
    R: Integer;
    Rec: TSearchrec;
    ActualAttr, I: Integer;
  begin
    Application.ProcessMessages();

    Directory := IncludeTrailingBackslash(Directory);

    Dir := '';
    if IncludePath <> ipNone then
    begin
      Dir := Directory;
      if IncludePath <> ipFull then
        Dir := Stringreplace(Dir, RootDir, '', [rfIgnoreCase]);
    end;

    for I := 0 to FNames.Count - 1 do
    begin
      Res := FindFirst(Directory + FNames[I], faAnyFile, SRec);
      while (Res = 0) and (Self.Count < MaxNumber) do
      begin
        if Match(Attr, SRec.Attr, Check, ActualAttr) and
          (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          SRec.Name := Dir + SRec.Name;
          if not ToBeIgnored(SRec.Name) then
          begin
            SRec.Attr := ActualAttr;              // store the extended attribute value
            AddToFileList(SRec);
          end;
        end;
        Res := FindNext(SRec);
      end;
      FindClose(SRec);
    end;

    if SubDirs and (Self.Count < MaxNumber) and
      (not ToBeIgnored(Directory)) then
      // Check for files in subdirectories also
    begin
      R := FindFirst(Directory + '*.*', faAnyFile, Rec);
      // find subdirectories (also the hidden ones and the system ones)
      while (R = 0) and (Self.Count < MaxNumber) do
      begin
        if ((Rec.Attr and faDirectory) > 0) and
          (Rec.Name <> '.') and
          (Rec.Name <> '..') then
          AddFilesToList(Directory + Rec.Name);   // add file names to the list
        R := FindNext(Rec);                       // get next subdirectory
      end;
      FindClose(Rec);
    end;

  end;

begin
  DisposeFileList;                                { clear the List }

  if (FNames <> nil) and (FNames.Count > 0) then
  begin

    if RootDir = ''
      then RootDir := IncludeTrailingBackslash(GetCurrentDir);
    RootDir := DirectoryNameExists(RootDir);      // resolve any wildcards
    if RootDir > '' then
      RootDir := IncludeTrailingBackslash(RootDir);

    if RootDir > '' then
    begin                                         // the rootdirectory exists
      for I := 0 to FNames.Count - 1 do
        FNames[I] := ExtractFileName(FNames[I]);
      // get filenames in the current directory
      // and in subdirectories if requested
      AddFilesToList(RootDir);
    end;

  end;
end;

procedure TFileList.MakeFileList(
  RootDir: string;
  Fn: string;                                     // filename to find
  Attr: Integer;
  Check: TAttributeCheck;
  SubDirs: Boolean = false;
  IncludePath: TIncludePath = ipNone;
  MaxNumber: Integer = MaxInt);
var Names: TStrings;
begin
  if RootDir = '' then RootDir := ExtractFilePath(Fn);
  Names := TStringList.Create;
  try
    Names.Add(Fn);
    MakeFileList(RootDir, Names, nil, Attr, Check, SubDirs, IncludePath, MaxNumber);
  finally
    Names.Free;
  end;
end;

procedure TFileList.MakeFileList(
  Fn: string;
  Attr: Integer;
  Check: TAttributeCheck;
  SubDirs: Boolean = false;
  IncludePath: TIncludePath = ipNone;
  MaxNumber: Integer = MaxInt);
begin
  MakeFileList(
    '',
    Fn,
    Attr,
    Check,
    SubDirs,
    IncludePath,
    MaxNumber);
end;

// the procedure below sorts the items in the filelist according name

function Compare(Item1, Item2: Pointer): Integer;
var
  R1, R2: TSearchrec;
begin
  R1 := TSearchRec(Item1^);
  R2 := TSearchRec(Item2^);
  if R1.Name > R2.Name then
    Result := 1
  else if R1.Name < R2.Name then
    Result := -1
  else
    Result := 0;
end;

procedure TFileList.SortFileList;
begin
  Sort(Compare);
end;

// the procedure below puts all filenames into 'List' that match the filename
// the attributes and acStrict or acRelaxed

procedure GetFileNames(
  RootDir: string;
  FNames,
  Excluded: Tstrings;
  Attr: Integer;
  Check: TAttributeCheck;
  RemoveExtension: Boolean;
  List: TStrings;
  SubDirs: Boolean = false;
  IncludePath: TIncludePath = ipNone;
  MaxNumber: Integer = MaxInt);

  procedure MakeList;
  var
    FileList: TFileList;
    I: Word;
    SRec: TSearchRec;
    Name: string;
  begin
    FileList := TFileList.Create;
    try
      // Make a new filelist
      FileList.MakeFileList(RootDir, FNames, Excluded, Attr, Check, SubDirs, IncludePath, MaxNumber);
      for I := 1 to FileList.Count do             // Do all entries in the list
      begin
        FileList.GetEntry(I, SRec);               // Get the entry data
        Name := SRec.Name;
        if RemoveExtension then Name := ChangeFileExt(Name, '');
        List.Add(Name);
      end;
    finally
      FileList.free;
    end;
  end;

begin
  List.Clear;
  MakeList;
end;

procedure GetFileNames(
  RootDir: string;
  Fn: string;
  Attr: Integer;
  Check: TAttributeCheck;
  RemoveExtension: Boolean;
  List: TStrings;
  SubDirs: Boolean = false;
  IncludePath: TIncludePath = ipNone;
  MaxNumber: Integer = MaxInt);
var Names: TStrings;
begin
  if RootDir = '' then RootDir := ExtractFilePath(Fn);
  Names := TStringList.Create;
  try
    Names.Add(Fn);
    GetFileNames(RootDir, Names, nil, Attr, Check, RemoveExtension, List, SubDirs, IncludePath, MaxNumber);
  finally
    Names.Free;
  end;
end;

procedure GetFileNames(Fn: string; Attr: Integer; Check: TAttributeCheck;
  RemoveExtension: Boolean; List: TStrings; Subdirs: Boolean = false;
  IncludePath: TIncludePath = ipNone; MaxNumber: Integer = MaxInt); overload;
begin
  GetFileNames(
    '',
    Fn,
    Attr,
    Check,
    RemoveExtension,
    List,
    SubDirs,
    IncludePath,
    MaxNumber);
end;

function DirectoryNameExists(DirName: string; const IncludeBackSlash: Boolean =
  true): string;
var
  Tmp: TStrings;
  I, Res: Integer;
  SRec: TSearchRec;
begin
  DirName := Trim(DirName);
  if DirName = '' then DirName := '.';
  DirName := ExpandFileName(DirName);

  if DirectoryExists(DirName) then
  begin                                           // no wildcard resolution needed
    if IncludeBackSlash then
      Result := IncludeTrailingBackslash(DirName)
    else
      Result := ExcludeTrailingBackslash(DirName);
    Exit;
  end;

  Tmp := TStringList.Create;
  try
    StringToTStrings(DirName, Tmp, '\');
    TrimTStrings(Tmp, [trsTrim, trsEmptyLines]);

    if DirName[1] = '\' then Tmp[0] := '\' + Tmp[0];

    Result := '';
    for I := 0 to Tmp.Count - 1 do
    begin                                         // go along the path and check if every subdir exists
      // try to resolve any wildcards

      if DirectoryExists(Result + IncludeTrailingBackSlash(Tmp[I])) then
        Result := Result + IncludeTrailingBackSlash(Tmp[I])
      else

      begin                                       // search for the directory (and resolve wildcards)
        Res := FindFirst(Result + Tmp[I], faAnyfile, SRec);

        while (Res = 0) and
          ((Srec.Name = '.') or
          (SRec.Name = '..') or
          ((SRec.Attr and faDirectory) <> faDirectory)
          ) do
          Res := FindNext(SRec);

        if (Res = 0) then                         // directory name found
        begin
          Result := Result + IncludeTrailingBackSlash(SRec.Name); // actual name
          FindClose(SRec);
        end
        else
        begin
          FindClose(SRec);
          Result := '';                           // (part of the) directoryname does not exist
          Exit;
        end;
      end;
    end;
  finally
    Tmp.Free;
  end;
  if IncludeBackSlash then
    Result := IncludeTrailingBackslash(Result)
  else
    Result := ExcludeTrailingBackslash(Result);
end;

function ExpandToLongPathName(ShortPath: string; const IncludeBackSlash: Boolean
  = true): string;
var
  J: Integer;
  Info: _SHFILEINFOA;
  DisplayName: string;
  Tmp: TStrings;
begin
  Result := '';
  if ShortPath = '' then exit;
  ShortPath := ExpandFileName(ShortPath);

  Tmp := TStringList.Create;
  try
    StringToTStrings(ShortPath, Tmp, '\');
    TrimTStrings(Tmp, [trsTrim, trsEmptyLines]);

    for J := 0 to Tmp.Count - 1 do
    begin
      if SHGetFileInfo(PChar(Result + Tmp[J]), 0, Info, SizeOf(Info),
        SHGFI_DISPLAYNAME) > 0 then
      begin                                       // SHGetFileInfo succeeded
        DisplayName := string(Info.szDisplayName);
        Result := Result + IncludeTrailingBackslash(DisplayName);
      end                                         // SHGetFileInfo failed
      else
        Result := Result + IncludeTrailingBackslash(Tmp[J]);
    end;
  finally
    Tmp.Free;
  end;

  if IncludeBackSlash then
    Result := IncludeTrailingBackslash(Result)
  else
    Result := ExcludeTrailingBackslash(Result);
end;

end.
