(****************************************************************************
 * WANT - A build management tool.                                          *
 * Copyright (c) 2001-2003 Juancarlo Anez, Caracas, Venezuela.              *
 * All rights reserved.                                                     *
 *                                                                          *
 * This library is free software; you can redistribute it and/or            *
 * modify it under the terms of the GNU Lesser General Public               *
 * License as published by the Free Software Foundation; either             *
 * version 2.1 of the License, or (at your option) any later version.       *
 *                                                                          *
 * This library is distributed in the hope that it will be useful,          *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        *
 * Lesser General Public License for more details.                          *
 *                                                                          *
 * You should have received a copy of the GNU Lesser General Public         *
 * License along with this library; if not, write to the Free Software      *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA *
 ****************************************************************************)
{
    @brief Cvs tasks

    @author Juancarlo Añez
    @author Radim Novotny <radimnov@seznam.cz>
    @author Bob Arnson <sf@bobs.org>
}

{ Notes:
    Code idea (and fragments) are taken from Ant java classes

    Cvs             attributes "error" and "append" are not implemented.
                    attributes "passfile" and "port" are not implemented too,
                    because CVSNT does not use these environment variables
                    (http://ant.apache.org/manual/CoreTasks/cvs.html)
    CvsTagDiff      attribute  "rootdir" is not implemented
                    (http://ant.apache.org/manual/CoreTasks/cvstagdiff.html)
    CvsPass         Added property emptypassword for login to CVS servers without
                    password specified (for example sourceforge CVS).
                    Only acceptable value for "emptypassword" is "true".
                    if CVSNT is used, default password file is not regular
                    file, but passwords are stored in Windows registry key
                    HKEY_CURRENT_USER/Software/Cvsnt/cvspass
                    This task (on MsWindows) is trying to write password to
                    registry and when it fails, then write password
                    to $HOME/.cvspass
                    On Linux it tries only $HOME/.cvspass
                    (http://ant.apache.org/manual/CoreTasks/cvspass.html)
    CvsChangeLog    Added two properties - dateformat and timeformat
                    These properties are used to format date or time written
                    into changelog (XML file). Default: yyyy-mm-dd and hh:mm
                    Format string is compatible with Delphi date-time format
                    string (see "Date-Time values, formatting" in Delphi help)
                    (http://ant.apache.org/manual/CoreTasks/changelog.html)

    DELPHI 5 COMPATIBILITY NOTE -----
    Note that because Delphi 5 lacks a AnsiToUtf8, characters that would be
    translated to UTF-8 will be output as-is. Anyone care to contribute a
    Delphi 5-compatible AnsiToUtf8? --Bob Arnson
}

unit CVSTasks;

interface

{$IFDEF VER130}
{$DEFINE MSWINDOWS}
{$ENDIF VER130}

uses
  SysUtils,
  Classes,
{$IFNDEF VER130}
  DateUtils,
{$ENDIF VER130}

  {$IFDEF MSWINDOWS}
  JclRegistry,
  Windows,
  {$ENDIF}
  JclSysInfo,
  JclFileUtils,

  XPerlRE,

  WildPaths,
  ExecTasks,
  WantClasses,
  IniFiles, {Hashed stringlist}
  Contnrs;  {TObjectList}

type
  {$IFDEF MSWINDOWS}
	THashedStringList = TStringList;
  {$ENDIF}

  // used in CvsChangelog
  TRCSFile = class
  private
    FFile         :string;
    FRevision     :string;
    FPrevRevision :string;
  public
    constructor Create(AName, ARevision: string);                overload;
    constructor Create(AName, ARevision, APrevRevision: string); overload;

    property FileName     :string  read FFile         write FFile;
    property Revision     :string  read FRevision     write FRevision;
    property PrevRevision :string  read FPrevRevision write FPrevRevision;
  end;

  // used in CvsChangelog
  TCvsEntry = class
  private
    FDate     :TDateTime;
    FAuthor   :string;
    FComment  :string;
    FFiles    :TObjectList;
  public
    constructor Create(ADate: TDateTime; AAuthor, AComment: string);
    destructor  Destroy; override;

    procedure AddFile(AFile, ARevision: string);                    overload;
    procedure AddFile(AFile, ARevision, APreviousRevision: string); overload;
    
    function  OutAsXML(ADateFormat, ATimeFormat: string): string;
  end;

  // used in CvsTagDiff
  TCvsTagEntry = class
  private
    FFileName     :string;
    FPrevRevision :string;
    FRevision     :string;
  public
    constructor Create(AFileName: string);                           overload;
    constructor Create(AFileName, ARevision: string);                overload;
    constructor Create(AFileName, ARevision, APrevRevision: string); overload;

    function ToString : string;

    property FileName      :string read FFileName      write FFileName;
    property PrevRevision  :string read FPrevRevision  write FPrevRevision;
    property Revision      :string read FRevision      write FRevision;
  end;

  // used in CvsChangelog
  TCvsChangeLogParser = class
  private
    FFile              :string;
    FDate              :string;
    FAuthor            :string;
    FComment           :string;
    FRevision          :string;
    FPreviousRevision  :string;

    FEntries :THashedStringList;
    FStatus  :integer;

    procedure ProcessComment(ALine: string);
    procedure ProcessFile(ALine: string);
    procedure ProcessDate(ALine: string);
    procedure ProcessGetPreviousRevision(ALine: string);
    procedure ProcessRevision(ALine: string);
    procedure SaveEntry;
    procedure Reset;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure ProcessInputFile(AFile: string);

    class function  Parse(AInputFile: string): THashedStringList;
    class procedure OutputEntriesToXML(AOutputFile: string;
      AEntries: THashedStringList; ADateFormat, ATimeFormat: string);
  end;


  // used in CvsChangelog
  TCvsChangeLogUserElement = class(TScriptElement)
  protected
    FUserId       :string;
    FDisplayName  :string;
  published
    property userid      :string read FUserID      write FUserID;
    property displayname :string read FDisplayName write FDisplayName;
  public
    procedure Init;                 override;
    class function TagName: string; override;
  end;

  // Custom CVS Task - base class for other Cvs Tasks
  TCustomCVSTask = class(TCustomExecTask)
  protected
    FCompression      :boolean;
    FCompressionLevel :integer;
    FTag              :string;
    FCvsRoot          :string;
    FAlias: string;
    FCvsRsh           :string;
    FDate             :string;
    FPackage          :string;
    FCommand          :string;
    FDest             :string;
    FNoexec           :boolean;
    FQuiet            :boolean;

    function AddOption(AOption: string; AValue: string = '';
      AForceQuote: boolean = False): string;
    function BuildArguments: string; override;

    function BuildArgumentsGlobal   :string; virtual;
    function BuildArgumentsCommand  :string; virtual; abstract;
    function BuildArgumentsSpecific :string; virtual;
  public
    procedure Init; override;
  protected
    property command          :string  read FCommand          write FCommand;
    property compression      :boolean read FCompression      write FCompression;
    property compressionlevel :integer read FCompressionLevel write FCompressionLevel;
    property cvsroot          :string  read FCvsRoot          write FCvsRoot;
    property cvsrsh           :string  read FCvsRsh           write FCvsRsh;
    property dest             :string  read FDest             write FDest;
    property alias            :string  read FAlias            write FAlias;
    property package          :string  read FPackage          write FPackage;
    property tag              :string  read FTag              write FTag;
    property date             :string  read FDate             write FDate;
    property quiet            :boolean read FQuiet            write FQuiet;
    property noexec           :boolean read FNoexec           write FNoexec;
    property output;
    property failonerror;
  end;

  // this class is used internally to log most recent module tag
  // it is not globally visible Task (not registered task)
  TCvsMostRecentTag = class(TCustomCvsTask)
  private
    FMostRecentTag  :string;
    FModuleName     :string;
  protected
    function BuildArgumentsCommand  :string; override;
    function BuildArgumentsSpecific :string; override;
  public
    procedure Execute; override;

    property modulename    :string read FModuleName     write FModuleName;
    property mostrecenttag :string read FMostRecentTag;
  end;

  TCvsTask = class(TCustomCvsTask)
  public
    procedure Execute; override;
    function  BuildArgumentsCommand: string; override;
  published
    property alias;
    property command;
    property compression;
    property compressionlevel;
    property cvsroot;
    property cvsrsh;
    property dest;
    property package;
    property tag;
    property date;
    property quiet;
    property noexec;
    property output;
    property failonerror;
  end;

  TCvsTagDiffTask = class(TCustomCvsTask)
  private
    FStartTag             :string;
    FStartDate            :string;
    FEndTag               :string;
    FEndDate              :string;
    FDestFile             :string;
    FMostRecentModuleName :string;

    function  CopyToEnd(AString: string; AFrom: integer): string;
    function  ParseRDiffOutput(AOutput: string;
       var AParsedOutput: TObjectList): boolean;
    procedure WriteTagDiff (const AParsedOutput: TObjectList);
    function  WriteTagEntry(const AEntry: TCvsTagEntry): string;
    function  FindMostRecentTag: string;
  public
    procedure Execute; override;
    procedure Init;    override;
  protected
    function  BuildArgumentsCommand  :string; override;
    function  BuildArgumentsSpecific :string; override;
  published
    property compression;
    property compressionlevel;
    property cvsroot;
    property cvsrsh;
    property package;
    property quiet;
    property failonerror;

    property starttag  :string read FStartTag  write FStartTag;
    property startdate :string read FStartDate write FStartDate;
    property endtag    :string read FEndTag    write FEndTag;
    property enddate   :string read FEndDate   write FEndDate;
    property destfile  :string read FDestFile  write FDestFile;
    {
      this property is required only when starttag or endtag contains
      text "MOST RECENT", because of I don't know how to find most recent tag
      across all modules. This shoul be set to module (filename) which is
      in repository from project start (for example DPR file)
    }
    property mostrecentmodulename: string read fMostRecentModuleName
                                          write fMostRecentModuleName;
  end;

  TCvsPassTask = class(TCustomCvsTask)
  private
    FPassword      :string;
    FEmptyPassword :boolean;

    procedure ChangeCvsPassInHome;
    function  ScrambleCvsPassword(const APassword: string): string;
    procedure WritePasswordTo(aFileName: string);
    {$IFDEF MSWINDOWS}
    procedure ChangeCvsPassInRegistry;
    {$ENDIF MSWINDOWS}
  public
    procedure Init;    override;
    procedure Execute; override;
  published
    property cvsroot;
    property password      :string  read FPassword      write FPassword;
    property emptypassword :boolean read FEmptyPassword write FEmptyPassword;
  end;

  TCvsChangeLogTask = class(TCustomCvsTask)
  private
    FUserList   :TList;
    FStart      :string;
    FUsersFile  :string;
    FDaysInPast :string;
    FDir        :string;
    FDestFile   :string;
    FEnd        :string;
    FDateFormat :string;
    FTimeFormat :string;
  public
    constructor Create(AOwner: TScriptElement); override;
    destructor  Destroy;                        override;

    procedure Init;    override;
    procedure Execute; override;
    function  CreateUser(AUserID, ADisplayName: string): TCvsChangeLogUserElement;
      overload;
  protected
    function  BuildArgumentsCommand  :string; override;
    function  BuildArgumentsSpecific :string; override;
  published
    function CreateUser: TCvsChangeLogUserElement; overload;

    property dest       :string read FDest       write FDest;
    property dir        :string read FDir        write FDir;
    property destfile   :string read FDestFile   write FDestFile;
    property usersfile  :string read FUsersFile  write FUsersFile;
    property daysinpast :string read FDaysInPast write FDaysInPast;
    property start      :string read FStart      write FStart;
    property _end       :string read FEnd        write FEnd;
    // following properties are not included in Ant
    property dateformat :string read FDateFormat write FDateFormat;
    property timeformat :string read FTimeFormat write FTimeFormat;
  end;


implementation

const
  FILE_IS_NEW = ' is new; current revision ';
  FILE_HAS_CHANGED = ' changed from revision ';
  FILE_WAS_REMOVED = ' is removed';
  GET_FILE = 1;
  GET_DATE = 2;
  GET_COMMENT = 3;
  GET_REVISION = 4;
  GET_PREVIOUS_REV = 5;

var
  // for scramble cvs password
  PSW_SHIFTS: array [0..255] of byte =
   ( 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,
    16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
   114, 120,  53,  79,  96, 109,  72, 108,  70,  64,  76,  67, 116,  74,  68,  87,
   111,  52,  75, 119,  49,  34,  82,  81,  95,  65, 112,  86, 118, 110, 122, 105,
    41,  57,  83,  43,  46, 102,  40,  89,  38, 103,  45,  50,  42, 123,  91,  35,
   125,  55,  54,  66, 124, 126,  59,  47,  92,  71, 115,  78,  88, 107, 106,  56,
    36, 121, 117, 104, 101, 100,  69,  73,  99,  63,  94,  93,  39,  37,  61,  48,
    58, 113,  32,  90,  44,  98,  60,  51,  33,  97,  62,  77,  84,  80,  85, 223,
   225, 216, 187, 166, 229, 189, 222, 188, 141, 249, 148, 200, 184, 136, 248, 190,
   199, 170, 181, 204, 138, 232, 218, 183, 255, 234, 220, 247, 213, 203, 226, 193,
   174, 172, 228, 252, 217, 201, 131, 230, 197, 211, 145, 238, 161, 179, 160, 212,
   207, 221, 254, 173, 202, 146, 224, 151, 140, 196, 205, 130, 135, 133, 143, 246,
   192, 159, 244, 239, 185, 168, 215, 144, 139, 165, 180, 157, 147, 186, 214, 176,
   227, 231, 219, 169, 175, 156, 206, 198, 129, 164, 150, 210, 154, 177, 134, 127,
   182, 128, 158, 208, 162, 132, 167, 209, 149, 241, 153, 251, 237, 236, 171, 195,
   243, 233, 253, 240, 194, 250, 191, 155, 142, 137, 245, 235, 163, 242, 178, 152
   );

{$IFDEF VER130}
//
// utility functions that exist in Delphi 6 and later but not in Delphi 5
//

function FileIsReadOnly(const AFileName: string): boolean;
begin
	Result := FileGetAttr(AFileName) and faReadOnly > 0;
end;

function AnsiToUtf8(const AString: string): string;
begin
	Result := AString;
end;
{$ENDIF VER130}

  { Local function }
function ParseCVSDate(ADate: string): TDateTime;
var
  bConverted: boolean;
  bMonth: word;
begin
  Result := Now;
  // try to create TDateTime from string date in different format
  // Supported formats:
  // format used in system by current locale
  // [d]d.mm.yyyy            (24.9.2001)
  // yyyy-[m]m-[d]d [h]h:[m]m:[s]s   (2001-09-24 10:15:45)
  // yyyy/[m]m/[d]d [h]h:[m]m:[s]s   (2001/09/24 10:15:45)
  // yyyy-[m]m-[d]d            (2001-09-24)
  // yyyy/[m]m/[d]d            (2001/09/24)
  // [d]d MMM yyyy           (24 Sep 2001)

  bConverted := False;
  try { current Locale settings }
    Result     := StrToDate(ADate);
    bConverted := True;
  except
  end;

  if not bConverted then
  begin { dd.mm.yyyy }
    try
      if regex.Match('(\d?\d)\.(\d?\d)\.(\d\d\d\d)', ADate) then
      begin
        Result := EncodeDate(
                      StrToInt(regex.SubExp[3].Text),   {year}
                      StrToInt(regex.SubExp[2].Text),   {month}
                      StrToInt(regex.SubExp[1].Text));  {day}
        bConverted := True;
      end;
    except
    end;
  end;

  if not bConverted then
  begin { yyyy/mm/dd hh:mm:ss } { yyyy-mm-dd hh:mm:ss }
    try
      if regex.Match('(\d\d\d\d)[/-](\d?\d)[/-](\d?\d) (\d?\d):(\d?\d):(\d?\d)', ADate) then
      begin
        Result := EncodeDate(
                    StrToInt(regex.SubExp[1].Text),   {year}
                    StrToInt(regex.SubExp[2].Text),   {month}
                    StrToInt(regex.SubExp[3].Text));  {day}
        Result := Result + EncodeTime(
                    StrToInt(regex.SubExp[4].Text),   {hour}
                    StrToInt(regex.SubExp[5].Text),   {min}
                    StrToInt(regex.SubExp[6].Text),   {sec}
                    0);                               {msec}
        bConverted := True;
      end;
    except
    end;
  end;

  if not bConverted then
  begin { yyyy-mm-dd } { yyyy/mm/dd }
    try
      if regex.Match('(\d\d\d\d)[/-](\d?\d)[/-](\d?\d)', ADate) then
      begin
        Result := EncodeDate(
                    StrToInt(regex.SubExp[1].Text),  {year}
                    StrToInt(regex.SubExp[2].Text),  {month}
                    StrToInt(regex.SubExp[3].Text)); {day}
        bConverted := True;
      end;
    except
    end;
  end;

  if not bConverted then
  begin { dd MMM yyyy}
    try
      if regex.Match('(\d?\d) ([A-Z][a-z][a-z]) (\d\d\d\d)', ADate) then
      begin
        bMonth := 0;
        if regex.SubExp[2].Text = 'Jan' then bMonth := 1;
        if regex.SubExp[2].Text = 'Feb' then bMonth := 2;
        if regex.SubExp[2].Text = 'Mar' then bMonth := 3;
        if regex.SubExp[2].Text = 'Apr' then bMonth := 4;
        if regex.SubExp[2].Text = 'May' then bMonth := 5;
        if regex.SubExp[2].Text = 'Jun' then bMonth := 6;
        if regex.SubExp[2].Text = 'Jul' then bMonth := 7;
        if regex.SubExp[2].Text = 'Aug' then bMonth := 8;
        if regex.SubExp[2].Text = 'Sep' then bMonth := 9;
        if regex.SubExp[2].Text = 'Oct' then bMonth := 10;
        if regex.SubExp[2].Text = 'Nov' then bMonth := 11;
        if regex.SubExp[2].Text = 'Dec' then bMonth := 12;
        if bMonth <> 0 then
        begin
          Result := EncodeDate(
                       StrToInt(regex.SubExp[3].Text),   {year}
                       bMonth,                           {month}
                       StrToInt(regex.SubExp[1].Text));  {day}
          bConverted := True;
        end;
      end;
    except
    end;
  end;
  if not bConverted then Result := Now;
end;

{ TCustomCvsTask }

function TCustomCvsTask.AddOption(AOption, AValue: string; AForceQuote: boolean): string;
begin
  Result := ' ' + AOption;
  if AValue <> '' then
  begin
    if (Pos(' ', AValue) > 0) or AForceQuote
       then Result := Result + '"' + AValue + '"'
       else Result := Result + AValue;
  end;
end;

function TCustomCvsTask.BuildArguments: string;
begin
  BuildArgumentsGlobal;
  BuildArgumentsCommand;
  BuildArgumentsSpecific;

  Result := inherited BuildArguments;
end;

function TCustomCVSTask.BuildArgumentsGlobal: string;
begin
  // first add global CVS options
  if FQuiet then
  begin
    Log(vlVerbose, 'quiet=true');
    ArgumentList.Add('-q');
  end;
  if FNoexec then
  begin
    Log(vlVerbose, 'noexec=true');
    ArgumentList.Add('-n');
  end;
  if FCompression then
  begin
    if HasAttribute('compressionlevel') then
    begin
      if FCompressionLevel in [1..9] then
      begin
        Log(vlVerbose, 'compression=true');
        Log(vlVerbose, 'compressionlevel=' + IntToStr(FCompressionLevel));
        ArgumentList.Add(AddOption('-z', IntToStr(FCompressionLevel)));
      end
      else
      begin
        Log(vlWarnings, 'Invalid compressionlevel value (not in range 1-9): '
                        + IntToStr(FCompressionLevel));
      end;
    end
    else
    begin
      Log(vlVerbose, 'compression=true');
      ArgumentList.Add(AddOption('-z', '3'));
    end;
  end;
  if FCvsRoot <> '' then
  begin
    Log(vlVerbose, 'CVSROOT=' + FCvsRoot);
    ArgumentList.Add(AddOption('-d', FCvsRoot));
  end;
  if FCvsRsh <> '' then
  begin
    Log(vlVerbose, 'CVS_RSH=' + FCvsRsh);
    JclSysInfo.SetEnvironmentVar('CVS_RSH', FCvsRsh);
  end;
end;

function TCustomCVSTask.BuildArgumentsSpecific: string;
begin
  if FTag <> '' then
  begin
    Log(vlVerbose, 'tag=' + FTag);
    ArgumentList.Add(AddOption('-r', FTag, True));
  end;
  if fdate <> '' then
  begin
    Log(vlVerbose, 'date=' + FDate);
    ArgumentList.Add(AddOption('-D', FDate, True));
  end;
  if FPackage <> '' then
  begin
    Log(vlVerbose, 'package=' + FPackage);
    ArgumentList.Add(AddOption(FPackage));
  end;
end;

procedure TCustomCvsTask.Init;
begin
  {$IFDEF LINUX}
  Executable := 'cvs';
  {$ELSE}
  Executable := 'cvs.exe';
  {$ENDIF}
  inherited;
end;

{ TCvsTask }

function TCvsTask.BuildArgumentsCommand: string;
begin
  if FCommand <> '' then
  begin
    Log(vlVerbose, 'command=' + FCommand);
    ArgumentList.Add(AddOption(FCommand));
  end
  else
  begin
    Log(vlVerbose, 'command=checkout');
    ArgumentList.Add(AddOption('checkout'));
  end;
  if FAlias <> '' then
  begin
    Log(vlVerbose, 'alias=' + FAlias);
    ArgumentList.Add(AddOption('-d',FAlias));
  end;
end;

procedure TCvsTask.Execute;
var
  bOldDir: TPath;
begin
  bOldDir := CurrentDir;
  if FDest <> '' then
  begin
    ChangeDir(FDest, True);
  end;
  inherited;
  ChangeDir(bOldDir);
end;

{ TCvsTagDiffTask }

function TCvsTagDiffTask.BuildArgumentsCommand: string;
begin
  Log(vlVerbose, 'command=rdiff');
  ArgumentList.Add(AddOption('rdiff'));
end;

function TCvsTagDiffTask.BuildArgumentsSpecific: string;
begin
  ArgumentList.Add(AddOption('-s')); // short listing
  if FStartTag <> '' then
  begin
    if FStartTag = 'MOST RECENT' then
    begin
      Log(vlVerbose, 'trying to find most recent tag');
      FStartTag := FindMostRecentTag;
    end;
    Log(vlVerbose, 'starttag=' + FStartTag);
    ArgumentList.Add(AddOption('-r', FStartTag, True));
  end
  else if FStartDate <> '' then
  begin
    Log(vlVerbose, 'startdate=' + FStartDate);
    ArgumentList.Add(AddOption('-D', FStartDate, True));
  end;
  if FEndTag <> '' then
  begin
    if FEndTag = 'MOST RECENT' then
    begin
      Log(vlVerbose, 'trying to find most recent tag');
      FEndTag := FindMostRecentTag;
    end;
    Log(vlVerbose, 'endtag=' + FEndTag);
    ArgumentList.Add(AddOption('-r', FEndTag, True));
  end
  else if fendDate <> '' then
  begin
    Log(vlVerbose, 'enddate=' + FEndDate);
    ArgumentList.Add(AddOption('-D', FEndDate, True));
  end;
  inherited BuildArgumentsSpecific;
end;

function TCvsTagDiffTask.CopyToEnd(AString: string;
  AFrom: integer): string;
begin
  Result := Copy(AString, AFrom, Length(AString));
end;

procedure TCvsTagDiffTask.Execute;
var
  bRDiffOutput: TObjectList;
begin
  output := FileGetTempName('cvs');
  inherited;
  bRDiffOutput := TObjectList.Create;
  try
    if ParseRDiffOutput(output, bRDiffOutput) then WriteTagDiff(bRDiffOutput);
  finally
    SysUtils.DeleteFile(output);
    bRDiffOutput.Free;
  end;
end;

function TCvsTagDiffTask.FindMostRecentTag: string;
var
  bMRT: TCvsMostRecentTag;
begin
  bMRT := TCvsMostRecentTag.Create(self);
  try
    bMRT.Init;
    // copy attribute values from CvsTagDiffTask
    bMRT.compression      := FCompression;
    bMRT.compressionlevel := FCompressionlevel;
    bMRT.cvsroot          := FCvsRoot;
    bMRT.cvsrsh           := FCvsRsh;
    bMRT.package          := FPackage;
    bMRT.quiet            := FQuiet;
    bMRT.failonerror      := FFailOnError;
    bMRT.modulename       := FMostRecentModuleName;

    bMRT.Execute;
    Result := bMRT.mostrecenttag;
  finally
    bMRT.Free;
  end;
end;

procedure TCvsTagDiffTask.Init;
begin
  inherited;
  RequireAttribute('destfile');
  RequireAttribute('package');
  RequireAttribute('starttag|startdate');
  RequireAttribute('endtag|enddate');
  if (FStartTag = 'MOST RECENT') or (FEndtag = 'MOST RECENT') then
     RequireAttribute('mostrecentmodulename');
end;

function TCvsTagDiffTask.ParseRDiffOutput(AOutput: string;
  var AParsedOutput: TObjectList): boolean;
var
  i             : integer;
  bIndex        : integer;
  bNewIndex     : integer;
  bHeaderLength : integer;
  bRevSeparator : integer;
  bSL           : TStringList;
  bRevision     : string;
  bPrevRevision : string;
  bLine         : string;
  bFileName     : string;
begin
  Result := False;
  if Assigned(AParsedOutput) then
  begin
    bHeaderLength := 5 + Length(FPackage) + 2;
    bSL := TStringList.Create;
    try
      bSL.LoadFromFile(AOutput);
      for i := 0 to bSL.Count - 1 do
      begin
        bLine := CopyToEnd(bSL[i], bHeaderLength);
        bIndex := Pos(FILE_IS_NEW, bLine);
        if bIndex <> 0 then
        begin
          // it is a new file
          bFileName := Copy(bLine, 1, bIndex - 1);
          bRevision := CopyToEnd(bLine, bIndex + Length(FILE_IS_NEW));
          bNewIndex := AParsedOutput.Add(TCvsTagEntry.Create(bFileName, bRevision));
          Log(vlVerbose, TCvsTagEntry(AParsedOutput[bNewIndex]).ToString);
        end
        else
        begin
          bIndex := Pos(FILE_HAS_CHANGED, bLine);
          if bIndex <> 0 then
          begin
            // it is modified file
            bFileName := Copy(bLine, 1, bIndex - 1);
            bRevSeparator := Pos(' to ', bLine);
            bPrevRevision := Copy(bLine, bIndex + Length(FILE_HAS_CHANGED),
                                  bRevSeparator - (bIndex +Length(FILE_HAS_CHANGED)));
            // 4 is " to " length
            bRevision := CopyToEnd(bLine, bRevSeparator + 4);
            bNewIndex := AParsedOutput.Add(TCvsTagEntry.Create(bFileName,
                                                               bRevision,
                                                               bPrevRevision));
            Log(vlVerbose, TCvsTagEntry(AParsedOutput[bNewIndex]).ToString);
          end
          else
          begin
            bIndex := Pos(FILE_WAS_REMOVED, bLine);
            if bIndex <> 0 then
            begin
              // it is a removed file
              bFileName := Copy(bLine, 1, bIndex - 1);
              bNewIndex  := AParsedOutput.Add(TCvsTagEntry.Create(bFileName));
              Log(vlVerbose, TCvsTagEntry(AParsedOutput[bNewIndex]).ToString);
            end;
          end;
        end;
      end;
      Result := True;
    finally
      bSL.Free;
    end;
  end;
end;

procedure TCvsTagDiffTask.WriteTagDiff(const AParsedOutput: TObjectList);
var
  bFS :TFileStream;
  i   :integer;

  procedure StreamWriteString(const AString: string);
  var
    s: string;
  begin
    s := AnsiToUtf8(AString);
    bFS.WriteBuffer(s[1], Length(s));
  end;
begin
  bFS := TFileStream.Create(FDestfile, fmCreate);
  try
    StreamWriteString('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
    StreamWriteString('<tagdiff ');
    if FStartTag <> '' then StreamWriteString('starttag="' + FStartTag + '" ')
    else
      StreamWriteString('startdate="' + FStartDate + '" ');
    if FEndTag <> '' then StreamWriteString('endtag="' + FEndTag + '" ')
    else
      StreamWriteString('enddate="' + FEndDate + '" ');
    StreamWriteString('>'#13#10);
    for i := 0 to AParsedOutput.Count - 1 do
    begin
      StreamWriteString(WriteTagEntry(TCvsTagEntry(AParsedOutput[i])));
    end;
    StreamWriteString('</tagdiff>'#13#10);
  finally
    bFS.Free;
  end;
end;

function TCvsTagDiffTask.WriteTagEntry(const AEntry: TCvsTagEntry): string;
begin
  Result := #9'<entry>'#13#10;
  Result := Result + #9#9'<file>'#13#10;
  Result := Result + #9#9#9'<name>' + AEntry.FileName + '</name>'#13#10;
  if AEntry.Revision <> '' then
    Result := Result + #9#9#9'<revision>' + AEntry.Revision + '</revision>'#13#10;
  if AEntry.PrevRevision <> '' then
    Result := Result + #9#9#9'<prevrevision>' + AEntry.PrevRevision
                     + '</prevrevision>'#13#10;
  Result := Result + #9#9'</file>'#13#10;
  Result := Result + #9'</entry>'#13#10;
end;

{ TCvsTagEntry }

constructor TCvsTagEntry.Create(AFileName: string);
begin
  FFileName     := AFileName;
  FPrevRevision := '';
  FRevision     := '';
end;

constructor TCvsTagEntry.Create(AFileName, ARevision: string);
begin
  FFileName     := AFileName;
  FRevision     := ARevision;
  FPrevRevision := '';
end;

constructor TCvsTagEntry.Create(AFileName, ARevision, APrevRevision: string);
begin
  FFileName     := AFileName;
  FPrevRevision := APrevRevision;
  FRevision     := ARevision;
end;

function TCvsTagEntry.ToString: string;
begin
  Result := '';
  Result := Result + FFileName;
  if (FRevision = '') and (FPrevRevision = '') 
         then Result := Result + ' was removed'
  else if (FRevision <> '') and (FPrevRevision = '')
         then Result := Result + ' is new; current revision is ' + FRevision
  else if (FRevision <> '') and (FPrevRevision <> '')
         then Result := Result + ' has changed from ' + FPrevRevision + ' to ' + FRevision;
end;

{ TCvsPassTask }

// used in both, MsWindows and Linux
procedure TCvsPassTask.ChangeCvsPassInHome;
var
  bHomeDir: string;
begin
  if JclSysInfo.GetEnvironmentVar('HOME', bHomeDir, True) then
    WritePasswordTo(PathAddSeparator(bHomeDir) + '.cvspass')
  else
    Log(vlErrors, 'Cannot determine $HOME directory');
end;

{$IFDEF MSWINDOWS}
procedure TCvsPassTask.ChangeCvsPassInRegistry;
begin
  if RegKeyExists(HKEY_CURRENT_USER, 'Software\Cvsnt\cvspass') then 
  begin
    RegWriteString(HKEY_CURRENT_USER,
                   'Software\Cvsnt\cvspass',
                   FCvsRoot,
                   ScrambleCvsPassword(fPassword));
    Log(vlVerbose, 'Password for repository '
                   + FCvsRoot
                   + ' stored to registry key HKEY_CURRENT_USER/Software/Cvsnt/cvspass');
  end 
  else 
  begin
    Log(vlWarnings, 'Could not find registry key HKEY_CURRENT_USER/Software/Cvsnt/cvspass. Trying $HOME');
    ChangeCvsPassInhome;
  end;
end;
{$ENDIF}

procedure TCvsPassTask.Execute;
begin
  // no call to inherited, because of this task does not call cvs executable

  // if CVSNT is used, default password file is not regular file, but passwords
  // are stored in registry key HKEY_CURRENT_USER/Software/Cvsnt/cvspass
  // in format:  KeyName = repository; KeyValue = scrambled password

  // in linux is used ~/.cvspass
  {$IFDEF LINUX}
  ChangeCvsPassInHome;
  {$ELSE}
  ChangeCvsPassInRegistry;
  {$ENDIF}
end;

procedure TCvsPassTask.Init;
begin
  inherited;
  RequireAttribute('cvsroot');
  RequireAttribute('password|emptypassword');
  if GetAttribute('emptypassword') <> '' then 
  begin
    if FEmptyPassword then FPassword := ''; 
  end;
end;

function TCvsPassTask.ScrambleCvsPassword(const APassword: string): string;
var
  i: integer;
begin
  Result := 'A';
  for i := 1 to Length(APassword) do
  begin
    Result := Result + Chr(PSW_SHIFTS[Ord(APassword[i])]);
  end;
end;

procedure TCvsPassTask.WritePasswordTo(AFileName: string);
var
  bSL     : TStringList;
  bExists : boolean;
  bFound  : boolean;
  i       : integer;
begin
  bExists := False;
  if FileExists(AFileName) then
  begin
    bExists := True;
    if FileIsReadOnly(AFileName) then
    begin
      Log(vlErrors, 'Cannot write to ' + AFileName);
      exit;
    end;
  end;
  bSL := TStringList.Create;
  try
    if bExists then bSL.LoadFromFile(AFileName);
    bFound := False;
    for i := 0 to bSL.Count - 1 do
    begin
      if Copy(bSL[i], 1, Length(FCvsRoot)) = FCvsRoot then
      begin
        bSL[i] := FCvsRoot + ' ' + ScrambleCvsPassword(FPassword);
        bFound := True;
        break;
      end;
    end;
    if not bFound then bSL.Add(FCvsRoot + ' ' + ScrambleCvsPassword(FPassword));
    bSL.SaveToFile(AFileName);
  finally
    bSL.Free;
  end;
end;

{ TCvsChangeLogUserElement }

procedure TCvsChangeLogUserElement.Init;
begin
  inherited;
  RequireAttribute('userid');
  RequireAttribute('displayname');
end;

class function TCvsChangeLogUserElement.TagName: string;
begin
  Result := 'user';
end;

{ TCvsChangeLogTask }

function TCvsChangeLogTask.BuildArgumentsCommand: string;
begin
  Log(vlVerbose, 'command=log');
  ArgumentList.Add(AddOption('log'));
end;

function TCvsChangeLogTask.BuildArgumentsSpecific: string;
var
  s: string;
begin
  if FDaysInPast <> '' then
  begin
    DateTimeToString(FStart, 'yyyy-mm-dd', Now - StrToInt(FDaysInPast));
    Log(vlVerbose, 'daysinpast (' + FDaysInPast + ') converted to ' + FStart);
  end;
  if FStart <> '' then
  begin
    DateTimeToString(s, 'yyyy-mm-dd', ParseCVSDate(FStart));
    s := '>=' + s;
    ArgumentList.Add(AddOption('-d', s, True));
    Log(vlVerbose, 'date' + s);
  end;
  if FDir = '' then FDir := FBasedir;
  inherited BuildArgumentsSpecific;
end;

constructor TCvsChangeLogTask.Create(AOwner: TScriptElement);
begin
  FDateFormat := 'yyyy-mm-dd';
  FTimeFormat := 'hh:mm';
  inherited Create(aOwner);
  FUserList := TList.Create;
end;

function TCvsChangeLogTask.CreateUser: TCvsChangeLogUserElement;
begin
  Result := TCvsChangeLogUserElement.Create(self);
  FUserList.Add(Result);
end;

function TCvsChangeLogTask.CreateUser(AUserID,
  ADisplayName: string): TCvsChangeLogUserElement;
begin
  Result := TCvsChangeLogUserElement.Create(self);
  Result.userid := AUserID;
  Result.displayname := ADisplayName;
  FUserList.Add(Result);
end;

destructor TCvsChangeLogTask.Destroy;
begin
  FUserList.Free;
  inherited;
end;

procedure TCvsChangeLogTask.Execute;
var
  i         : integer;
  j         : integer;
  bSL       : TStringList;
  bOldDir   : TPath;
  bEntry    : TCvsEntry;
  bEntries  : THashedStringList;
begin
  bOldDir := CurrentDir;
  if FDir <> '' then
  begin
    ChangeDir(FDir, True);
    Log(vlDebug, 'directory changed to ' + FDir);
  end;
  output := FileGetTempName('cvs');
  inherited;
  ChangeDir(bOldDir);
  Log(vlDebug, 'directory changed back to ' + bOldDir);

  // append to user list from file
  if FUsersFile <> '' then
  begin
    if FileExists(FUsersFile) then
    begin
      bSL := TStringList.Create;
      try
        bSL.LoadFromFile(FUsersFile);
        for i := 0 to bSL.Count - 1 do
        begin
          if bSL.Values[bSL.Names[i]] <> '' then
          begin
            CreateUser(bSL.Names[i], bSL.Values[bSL.Names[i]]);
          end;
        end;
      finally
        bSL.Free;
      end;
    end
    else
      Log(vlWarnings, 'Userfile ' + fUsersFile + ' does not exists');
  end;
  bEntries := TCvsChangeLogParser.Parse(output);
  DeleteFile(output);

  // filter start/end dates and replace username
  for i := bEntries.Count - 1 downto 0 do
  begin
    bEntry := TCvsEntry(bEntries.Objects[i]);
    if FStart <> '' then
    begin
      if bEntry.FDate < ParseCVSDate(FStart) then
      begin
        bEntries.Delete(i);
        continue;
      end;
    end;
    if FEnd <> '' then
    begin
      if bEntry.FDate > ParseCVSDate(FEnd) then
      begin
        bEntries.Delete(i);
        continue;
      end;
    end;
    for j := 0 to fUserList.Count - 1 do
    begin
      if TCvsChangeLogUserElement(FUserList[j]).userid = bEntry.FAuthor then
      begin
        bEntry.FAuthor := TCvsChangeLogUserElement(fUserList[j]).displayname;
        break;
      end;
    end;
  end;
  TCvsChangeLogParser.OutputEntriesToXML(FDestFile, bEntries, FDateFormat, FTimeFormat);
end;

procedure TCvsChangeLogTask.Init;
begin
  inherited;
  RequireAttribute('destfile');
end;

{ TRCSFile }

constructor TRCSFile.Create(AName, ARevision: string);
begin
  FFile         := AName;
  FRevision     := ARevision;
  FPrevRevision := '';
end;

constructor TRCSFile.Create(aName, aRevision, aPrevRevision: string);
begin
  FFile         := AName;
  FRevision     := ARevision;
  FPrevRevision := '';
  if ARevision <> APrevRevision then FPrevRevision := APrevRevision;
end;

{ TCvsEntry }

procedure TCvsEntry.AddFile(AFile, ARevision, APreviousRevision: string);
begin
  FFiles.Add(TRCSFile.Create(AFile, ARevision, APreviousRevision));
end;

procedure TCvsEntry.AddFile(AFile, ARevision: string);
begin
  FFiles.Add(TRCSFile.Create(AFile, ARevision));
end;

constructor TCvsEntry.Create(ADate: TDateTime; AAuthor, AComment: string);
begin
  FDate    := ADate;
  FAuthor  := AAuthor;
  FComment := AComment;
  FFiles   := TObjectList.Create;
end;

destructor TCvsEntry.Destroy;
begin
  FFiles.Free;
  inherited;
end;

function TCvsEntry.OutAsXML(ADateFormat, ATimeFormat: string): string;
var
  i       : integer;
  s       : string;
  bOutput : string;
  bRF     : TRCSFile;

  function AddLine(AText: string): string;
  begin
    bOutput := bOutput + AText + #13#10;
  end;
begin
  bOutput := '';
  AddLine(#9'<entry>');
  DateTimeToString(s, ADateFormat, FDate);
  AddLine(#9#9'<date>' + s + '</date>');
  DateTimeToString(s, ATimeFormat, FDate);
  AddLine(#9#9'<time>' + s + '</time>');
  AddLine(#9#9'<author><![CDATA[' + FAuthor + ']]></author>');
  for i := 0 to FFiles.Count - 1 do
  begin
    bRF := TRCSFile(FFiles[i]);
    AddLine(#9#9'<file>');
    AddLine(#9#9#9'<name>' + bRF.FileName + '</name>');
    AddLine(#9#9#9'<revision>' + bRF.Revision + '</revision>');
    if bRF.PrevRevision <> '' then
    begin
      AddLine(#9#9#9'<prevrevision>' + bRF.PrevRevision + '</prevrevision>');
    end;
    AddLine(#9#9'</file>');
  end;
  AddLine(#9#9'<msg><![CDATA[' + FComment + ']]></msg>');
  AddLine(#9'</entry>');
  Result := bOutput;
end;

{ TCvsChangeLogParser }

constructor TCvsChangeLogParser.Create;
begin
  FStatus  := GET_FILE;
  FEntries := THashedStringList.Create;
end;

destructor TCvsChangeLogParser.Destroy;
begin
  FEntries.Free;
  inherited;
end;

class procedure TCvsChangeLogParser.OutputEntriesToXML(AOutputFile: string;
  AEntries: THashedStringList; ADateFormat, ATimeformat: string);
var
  i   : integer;
  bFS : TFileStream;

  procedure StreamWriteString(const AString: string);
  var
    s: string;
  begin
    s := AnsiToUtf8(AString);
    bFS.WriteBuffer(s[1], Length(s));
  end;
begin
  bFS := TFileStream.Create(AOutputFile, fmCreate);
  try
    StreamWriteString('<?xml version="1.0" encoding="UTF-8"?>'#13#10);
    StreamWriteString('<changelog>'#13#10);
    for i := 0 to AEntries.Count - 1 do
    begin
      StreamWriteString(TCvsEntry(AEntries.Objects[i]).OutAsXML(ADateFormat, ATimeFormat));
    end;
    StreamWriteString('</changelog>'#13#10);
  finally
    bFS.Free;
  end;
end;

class function TCvsChangeLogParser.Parse(AInputFile: string): THashedStringList;
begin
  with TCvsChangeLogParser.Create do
  begin
    ProcessInputFile(AInputfile);
    Result := FEntries;
  end;
end;

procedure TCvsChangeLogParser.ProcessComment(ALine: string);
var
  bLineSeparator : string;
  bEnd           : integer;
begin
  {$IFDEF LINUX}
  bLineSeparator := #$0A;
  {$ELSE}
  bLineSeparator := #$0D#$0A;
  {$ENDIF}
  if Pos('======', ALine) = 1 then
  begin
    //We have ended changelog for that particular file
    //so we can save it
    bEnd := Length(FComment) - Length(bLineSeparator);
    fComment := Copy(FComment, 1, bEnd);
    SaveEntry;
    FStatus := GET_FILE;
  end
  else if Pos('----------------------------', ALine) = 1 then
  begin
    bEnd     := Length(FComment) - Length(bLineSeparator);
    FComment := Copy(FComment, 1, bEnd);
    FStatus  := GET_PREVIOUS_REV;
  end 
  else if Pos('branches:', ALine) = 1 then
  begin
    // "branches" was not in original Ant implementation
    // ignore "branches" line; continue in Comment parsing
  end
  else
  begin
    FComment := FComment + ALine + bLineSeparator;
  end;
end;

procedure TCvsChangeLogParser.ProcessDate(ALine: string);
var
  bLineData: string;
begin
  if Pos('date:', ALine) = 1 then
  begin
    FDate     := Copy(ALine, 7, 19);
    bLineData := Copy(ALine, Pos(';', ALine) + 1, Length(ALine));
    FAuthor   := Copy(bLineData, 11, Pos(';', bLineData) - 11);
    FStatus   := GET_COMMENT;
    //Reset comment to empty here as we can accumulate multiple lines
    //in the processComment method
    FComment := '';
  end;
end;

procedure TCvsChangeLogParser.ProcessFile(ALine: string);
begin
  if Pos('Working file:', ALine) = 1 then
  begin
    FFile   := Copy(ALine, 15, Length(ALine));
    FStatus := GET_REVISION;
  end;
end;

procedure TCvsChangeLogParser.ProcessGetPreviousRevision(ALine: string);
begin
  if Pos('revision', ALine) = 0 then
  begin
    raise Exception.Create('Unexpected line from CVS: ' + ALine);
  end;
  FPreviousRevision := Copy(ALine, 10, Length(ALine));
  SaveEntry;
  FRevision := FPreviousRevision;
  FStatus   := GET_DATE;
end;

procedure TCvsChangeLogParser.ProcessInputFile(AFile: string);
var
  i   : integer;
  bSL : TStringList;
begin
  bSL := TStringList.Create;
  try
    bSL.LoadFromFile(AFile);
    for i := 0 to bSL.Count - 1 do
    begin
      case FStatus of
        GET_FILE:
          begin
            // make sure attributes are reset when
            // working on a 'new' file.
            Reset;
            ProcessFile(bSL[i]);
          end;
        GET_REVISION: 
          begin
            ProcessRevision(bSL[i]);
          end;
        GET_DATE:
          begin
            ProcessDate(bSL[i]);
          end;
        GET_COMMENT:
          begin
            ProcessComment(bSL[i]);
          end;
        GET_PREVIOUS_REV:
          begin
            ProcessGetPreviousRevision(bSL[i]);
          end;
      end;
    end;
  finally
    bSL.Free;
  end;
end;

procedure TCvsChangeLogParser.ProcessRevision(aLine: string);
begin
  if Pos('revision', ALine) = 1 then
  begin
    FRevision := Copy(ALine, 10, Length(ALine));
    FStatus   := GET_DATE;
  end
  else if Pos('======', ALine) = 1 then
  begin
    //There was no revisions in this changelog
    //entry so lets move into next file
    FStatus := GET_FILE;
  end;
end;

procedure TCvsChangeLogParser.Reset;
begin
  FFile             := '';
  FDate             := '';
  FAuthor           := '';
  FComment          := '';
  FRevision         := '';
  FPreviousRevision := '';
end;

procedure TCvsChangeLogParser.SaveEntry;
var
  i         : integer;
  bEntryKey : string;
  bEntry    : TCvsEntry;
begin
  bEntryKey := FDate + FAuthor + FComment;
  i := FEntries.IndexOf(bEntryKey);
  if i = -1 then
  begin
    bEntry := TCvsEntry.Create(ParseCVSDate(FDate), FAuthor, FComment);
    FEntries.AddObject(bEntryKey, bEntry);
  end
  else
  begin
    bEntry := TCVSEntry(FEntries.Objects[i]);
  end;
  bEntry.AddFile(FFile, FRevision, FPreviousRevision);
end;

{ TCvsMostRecentTag }

function TCvsMostRecentTag.BuildArgumentsCommand: string;
begin
  Log(vlVerbose, 'command=log');
  ArgumentList.Add(AddOption('log'));
end;

function TCvsMostRecentTag.BuildArgumentsSpecific: string;
begin
  ArgumentList.Add(AddOption('-h')); // headers only
  ArgumentList.Add(AddOption(FModuleName));
end;

procedure TCvsMostRecentTag.Execute;
var
  i   : integer;
  BSL : TStringList;
begin
  FMostRecentTag := '';
  output := FileGetTempName('cvs');
  inherited;
  bSL := TStringList.Create;
  try
    bSL.LoadFromFile(output);
    i := bSL.IndexOf('symbolic names:');
    if i <> -1 then
    begin
      if bSL[i+1][1] = #9 then
      begin
        FMostRecentTag := Trim(Copy(bSL[i+1], 1, Pos(':', bSL[i+1]) - 1));
      end;
    end;
  finally
    bSL.Free;
    DeleteFile(output);
  end;
end;

initialization
  RegisterTasks([ TCvsTask,
                  TCvsTagDiffTask,
                  TCvsPassTask,
                  TCvsChangeLogTask ]);
  RegisterElement(TCvsChangeLogTask, TCvsChangeLogUserElement);
end.
