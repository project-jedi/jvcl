{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSysComp.PAS, released Dec 26, 1999.

The Initial Developer of the Original Code is Petr Vones (petr.v@mujmail.cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s): Marcel van Brakel <brakelm@bart.nl>.

Last Modified: Jun 20, 2000
Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}

unit JvSysComp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF DELPHI5_UP}Contnrs, {$ENDIF}ShellAPI, JclBase, JclStrings, JclSysInfo,
  JclFileUtils, JvComponent;

type
  EJvProcessError = EJclError;

  TJvProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime);

  TJvProcessEntry = class(TObject)
  private
    FFileName: TFileName;
    FProcessID: DWORD;
    FProcessName: string;
    function GetSystemIconIndex(IconType: Integer): Integer;
    function GetPriority: TJvProcessPriority;
    procedure SetPriority(const Value: TJvProcessPriority);
  public
    constructor Create(AProcessID: DWORD; const AFileName: TFileName;
      const AProcessName: string);
    function Close(UseQuit: Boolean = False): Boolean;
    class function PriorityText(Priority: TJvProcessPriority): string;
    function Terminate: Boolean;
    property FileName: TFileName read FFileName;
    property LargeIconIndex: Integer index SHGFI_LARGEICON read GetSystemIconIndex;
    property Priority: TJvProcessPriority read GetPriority write SetPriority;
    property ProcessID: DWORD read FProcessID;
    property ProcessName: string read FProcessName;
    property SmallIconIndex: Integer index SHGFI_SMALLICON read GetSystemIconIndex;
  end;


  TJvCPSState = (psReady, psRunning, psWaiting);
  TJvCPSFlag = (cfDefaultErrorMode, cfNewConsole, cfNewProcGroup, cfSeparateWdm,
    cfSharedWdm, cfSuspended, cfUnicode, cfDetached);
  TJvCPSFlags = set of TJvCPSFlag;
  TJvCPSShowWindow = (swHide, swMinimize, swMaximize, swNormal);

  TJvCPSTerminateEvent = procedure(Sender: TObject; ExitCode: DWORD) of object;

  TJvCPSStartupInfo = class(TPersistent)
  private
    FDesktop: string;
    FTitle: string;
    FDefaultPosition: Boolean;
    FDefaultWindowState: Boolean;
    FDefaultSize: Boolean;
    FHeight: Integer;
    FLeft: Integer;
    FWidth: Integer;
    FShowWindow: TJvCPSShowWindow;
    FTop: Integer;
    FForceOnFeedback: Boolean;
    FForceOffFeedback: Boolean;
    function GetStartupInfo: TStartupInfo;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property StartupInfo: TStartupInfo read GetStartupInfo;
  published
    property Desktop: string read FDesktop write FDesktop;
    property Title: string read FTitle write FTitle;
    property Left: Integer read FLeft write FLeft default 0;
    property Top: Integer read FTop write FTop default 0;
    property DefaultPosition: Boolean read FDefaultPosition write FDefaultPosition default True;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property DefaultSize: Boolean read FDefaultSize write FDefaultSize default True;
    property ShowWindow: TJvCPSShowWindow read FShowWindow write FShowWindow default swNormal;
    property DefaultWindowState: Boolean read FDefaultWindowState write FDefaultWindowState default True;
    property ForceOnFeedback: Boolean read FForceOnFeedback write FForceOnFeedback default False;
    property ForceOffFeedback: Boolean read FForceOffFeedback write FForceOffFeedback default False;
  end;

  TJvCreateProcess = class(TJvComponent)
  private
    FApplicationName: string;
    FCommandLine: string;
    FCreationFlags: TJvCPSFlags;
    FCurrentDirectory: string;
    FEnvironment: TStrings;
    FState: TJvCPSState;
    FStartupInfo: TJvCPSStartupInfo;
    FPriority: TJvProcessPriority;
    FProcessInfo: TProcessInformation;
    FWaitForTerminate: Boolean;
    FOnTerminate: TJvCPSTerminateEvent;
    WaitThread: TThread;
    procedure SetWaitForTerminate(const Value: Boolean);
    procedure WaitThreadOnTerminate(Sender: TObject);
    function GetProcessInfo: TProcessInformation;
    procedure SetEnvironment(const Value: TStrings);
  protected
    procedure CheckRunning;
    procedure CheckNotWaiting;
    procedure CloseProcessHandles;
    procedure TerminateWaitThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CloseApplication(SendQuit: Boolean = False): Boolean;
    procedure Run;
    procedure StopWaiting;
    procedure Terminate;
    property ProcessInfo: TProcessInformation read GetProcessInfo;
    property State: TJvCPSState read FState;
  published
    property ApplicationName: string read FApplicationName write FApplicationName;
    property CommandLine: string read FCommandLine write FCommandLine;
    property CreationFlags: TJvCPSFlags read FCreationFlags write FCreationFlags default [];
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
    property Environment: TStrings read FEnvironment write SetEnvironment;
    property Priority: TJvProcessPriority read FPriority write FPriority default ppNormal;
    property StartupInfo: TJvCPSStartupInfo read FStartupInfo write FStartupInfo;
    property WaitForTerminate: Boolean read FWaitForTerminate write SetWaitForTerminate default True;
    property OnTerminate: TJvCPSTerminateEvent read FOnTerminate write FOnTerminate;
  end;

  TJvFTSAttributes = set of (atReadOnly, atArchive, atHidden, atSysFile);

  TJvFileTreeScan = class;

  TJvFTSFileEvent = procedure(Sender: TJvFileTreeScan; const Name: string) of object;

  TJclFileMaskComparator = class(TObject)
  private
    FFileMask: string;
    FExts: array of string;
    FNames: array of string;
    FWildChars: array of Byte;
    FSeparator: Char;
    procedure CreateMultiMasks;
    function GetCount: Integer;
    function GetExts(Index: Integer): string;
    function GetMasks(Index: Integer): string;
    function GetNames(Index: Integer): string;
    procedure SetFileMask(const Value: string);
    procedure SetSeparator(const Value: Char);
  public
    constructor Create;
    function Compare(const NameExt: string): Boolean;
    property Count: Integer read GetCount;
    property Exts[Index: Integer]: string read GetExts;
    property FileMask: string read FFileMask write SetFileMask;
    property Masks[Index: Integer]: string read GetMasks;
    property Names[Index: Integer]: string read GetNames;
    property Separator: Char read FSeparator write SetSeparator;
  end;

  TJvFileTreeScan = class(TJvComponent)
  private
    FAborting: Boolean;
    FAbortOnError: Boolean;
    FDirAttributes: TJvFTSAttributes;
    FFileMask: string;
    FFileAttributes: TJvFTSAttributes;
    FMaskComparator: TJclFileMaskComparator;
    FRunning: Boolean;
    FSorted: Boolean;
    FScanFiles: Boolean;
    FRootDirectory: string;
    FTotalDirectories: Integer;
    FTotalFiles: Integer;
    FOnDirectory: TJvFTSFileEvent;
    FOnFile: TJvFTSFileEvent;
    procedure SetFileMask(const Value: string);
  protected
    function AttrFromAttributes(Attributes: TJvFTSAttributes): Integer;
    procedure InternalScan(const DirectoryName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Abort;
    procedure Scan;
    property MaskComparator: TJclFileMaskComparator read FMaskComparator;
    property Running: Boolean read FRunning;
    property TotalDirectories: Integer read FTotalDirectories;
    property TotalFiles: Integer read FTotalFiles;
  published
    property AbortOnError: Boolean read FAbortOnError write FAbortOnError default False;
    property DirAttributes: TJvFTSAttributes read FDirAttributes write FDirAttributes default [atReadOnly, atArchive];
    property FileAttributes: TJvFTSAttributes read FFileAttributes write FFileAttributes default [atReadOnly,
      atArchive, atHidden, atSysFile];
    property FileMask: string read FFileMask write SetFileMask;
    property RootDirectory: string read FRootDirectory write FRootDirectory;
    property ScanFiles: Boolean read FScanFiles write FScanFiles default True;
    property Sorted: Boolean read FSorted write FSorted default True;
    property OnDirectory: TJvFTSFileEvent read FOnDirectory write FOnDirectory;
    property OnFile: TJvFTSFileEvent read FOnFile write FOnFile;
  end;

implementation

uses
  TLHelp32, Psapi,
  JclSysUtils, JvComponentFunctions;

resourcestring
  RsListIndex = 'Process list index error';
  RsPIDNotFound = 'ProcessID %.8x not found';
  RsProcessIsRunning = 'Can''t perform this operation when process is running';
  RsProcessNotRunning = 'Process is not running';
  RsIdle = 'Idle';
  RsNormal = 'Normal';
  RsHigh = 'High';
  RsRealTime = 'RealTime';

const
  MaxProcessCount = 1024;
  ProcessPriorities: array[TJvProcessPriority] of DWORD =
  (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
    REALTIME_PRIORITY_CLASS);

function StrContainChars(const S: AnsiString; Chars: TSysCharSet; CheckAllChars: Boolean): Boolean;
var
  I: Integer;
  C: Char;
begin
  Result := False;
  if CheckAllChars then
  begin
    for I := 1 to Length(S) do
    begin
      C := S[I];
      if C in Chars then
        Chars := Chars - [C];
    end;
    Result := (Chars = []);
  end
  else
    for I := 1 to Length(S) do
      if S[I] in Chars then
      begin
        Result := True;
        Break;
      end;
end;

//==============================================================================
// TJclFileMaskComparator
//==============================================================================

function TJclFileMaskComparator.Compare(const NameExt: string): Boolean;
var
  I: Integer;
  NamePart, ExtPart: string;
  NameWild, ExtWild: Boolean;
begin
  Result := False;
  I := StrLastPos('.', NameExt);
  if I = 0 then
  begin
    NamePart := NameExt;
    ExtPart := '';
  end
  else
  begin
    NamePart := Copy(NameExt, 1, I - 1);
    ExtPart := Copy(NameExt, I + 1, Length(NameExt));
  end;
  for I := 0 to Length(FNames) - 1 do
  begin
    NameWild := FWildChars[I] and 1 = 1;
    ExtWild := FWildChars[I] and 2 = 2;
    if ((not NameWild and StrSame(FNames[I], NamePart)) or
      (NameWild and (StrMatch(FNames[I], NamePart) = 1))) and
      ((not ExtWild and StrSame(FExts[I], ExtPart)) or
      (ExtWild and (StrMatch(FExts[I], ExtPart) = 1))) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

constructor TJclFileMaskComparator.Create;
begin
  inherited;
  FSeparator := ';';
end;

//------------------------------------------------------------------------------

procedure TJclFileMaskComparator.CreateMultiMasks;
const
  WildChars = ['*', '?'];
var
  List: TStrings;
  I, N: Integer;
  NS, ES: string;
begin
  FExts := nil;
  FNames := nil;
  FWildChars := nil;
  List := TStringList.Create;
  try
    StrToStrings(FFileMask, FSeparator, List);
    SetLength(FExts, List.Count);
    SetLength(FNames, List.Count);
    SetLength(FWildChars, List.Count);
    for I := 0 to List.Count - 1 do
    begin
      N := StrLastPos('.', List[I]);
      if N = 0 then
      begin
        NS := List[I];
        ES := '';
      end
      else
      begin
        NS := Copy(List[I], 1, N - 1);
        ES := Copy(List[I], N + 1, 255);
      end;
      FNames[I] := NS;
      FExts[I] := ES;
      N := 0;
      if StrContainChars(NS, WildChars, False) then
        N := N or 1;
      if StrContainChars(ES, WildChars, False) then
        N := N or 2;
      FWildChars[I] := N;
    end;
  finally
    List.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetCount: Integer;
begin
  Result := Length(FWildChars);
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetExts(Index: Integer): string;
begin
  Result := FExts[Index];
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetMasks(Index: Integer): string;
begin
  Result := FNames[Index] + '.' + FExts[Index];
end;

//------------------------------------------------------------------------------

function TJclFileMaskComparator.GetNames(Index: Integer): string;
begin
  Result := FNames[Index];
end;

//------------------------------------------------------------------------------

procedure TJclFileMaskComparator.SetFileMask(const Value: string);
begin
  FFileMask := Value;
  CreateMultiMasks;
end;

//------------------------------------------------------------------------------

procedure TJclFileMaskComparator.SetSeparator(const Value: Char);
begin
  if FSeparator <> Value then
  begin
    FSeparator := Value;
    CreateMultiMasks;
  end;
end;

function InternalCloseApp(ProcessID: DWORD; UseQuit: Boolean): Boolean;
type
  PEnumWinRec = ^TEnumWinRec;
  TEnumWinRec = record
    ProcessID: DWORD;
    PostQuit: Boolean;
    FoundWin: Boolean;
  end;
var
  EnumWinRec: TEnumWinRec;

  function EnumWinProc(Wnd: HWND; Param: PEnumWinRec): BOOL; stdcall;
  var
    PID, TID: DWORD;
  begin
    TID := GetWindowThreadProcessId(Wnd, @PID);
    if PID = Param^.ProcessID then
    begin
      if Param^.PostQuit then
        PostThreadMessage(TID, WM_QUIT, 0, 0)
      else if IsWindowVisible(Wnd) then
        PostMessage(Wnd, WM_CLOSE, 0, 0);
      Param^.FoundWin := True;
    end;
    Result := True;
  end;

begin
  EnumWinRec.ProcessID := ProcessID;
  EnumWinRec.PostQuit := UseQuit;
  EnumWinRec.FoundWin := False;
  EnumWindows(@EnumWinProc, Integer(@EnumWinRec));
  Result := EnumWinRec.FoundWin;
end;

function InternalTerminateProcess(ProcessID: DWORD): Boolean;
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_TERMINATE, False, ProcessID);
  OSCheck(ProcessHandle <> 0);
  Result := TerminateProcess(ProcessHandle, 0);
  CloseHandle(ProcessHandle);
end;

function SafeCloseHandle(var H: THandle): Boolean;
begin
  if H <> 0 then
  begin
    Result := CloseHandle(H);
    if Result then
      H := 0;
  end
  else
    Result := True;
end;

{ TJvProcessEntry }

function TJvProcessEntry.Close(UseQuit: Boolean): Boolean;
begin
  Result := InternalCloseApp(ProcessID, UseQuit);
end;

constructor TJvProcessEntry.Create(AProcessID: DWORD;
  const AFileName: TFileName; const AProcessName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FProcessID := AProcessID;
  FProcessName := AProcessName;
end;

function TJvProcessEntry.GetPriority: TJvProcessPriority;
var
  ProcessHandle: THandle;
  PriorityClass: DWORD;
begin
  if ProcessID = 0 then
    Result := ppNormal
  else
  begin
    ProcessHandle := OpenProcess(PROCESS_ALL_ACCESS, False, ProcessID);
    OSCheck(ProcessHandle <> 0);
    try
      PriorityClass := GetPriorityClass(ProcessHandle);
      OSCheck(PriorityClass <> 0);
      case PriorityClass of
        NORMAL_PRIORITY_CLASS: Result := ppNormal;
        IDLE_PRIORITY_CLASS: Result := ppIdle;
        HIGH_PRIORITY_CLASS: Result := ppHigh;
        REALTIME_PRIORITY_CLASS: Result := ppRealTime;
      else
        Result := ppNormal;
      end;
    finally
      CloseHandle(ProcessHandle);
    end;
  end;
end;

function TJvProcessEntry.GetSystemIconIndex(IconType: Integer): Integer;
var
  FileInfo: TSHFileInfo;
begin
  ZeroMemory(@FileInfo, Sizeof(FileInfo));
  SHGetFileInfo(PChar(FileName), 0, FileInfo, Sizeof(FileInfo), SHGFI_SYSICONINDEX or IconType);
  Result := FileInfo.iIcon;
end;

class function TJvProcessEntry.PriorityText(Priority: TJvProcessPriority): string;
const
  PriorityTexts: array[TJvProcessPriority] of PResStringRec =
  (@RsIdle, @RsNormal, @RsHigh, @RsRealTime);
begin
  Result := LoadResString(PriorityTexts[Priority]);
end;

procedure TJvProcessEntry.SetPriority(const Value: TJvProcessPriority);
var
  ProcessHandle: THandle;
begin
  ProcessHandle := OpenProcess(PROCESS_SET_INFORMATION, False, ProcessID);
  OSCheck(ProcessHandle <> 0);
  try
    OSCheck(SetPriorityClass(ProcessHandle, ProcessPriorities[Value]));
  finally
    CloseHandle(ProcessHandle);
  end;
end;

function TJvProcessEntry.Terminate: Boolean;
begin
  Result := InternalTerminateProcess(FProcessID);
end;

{ TJvCPSStartupInfo }

procedure TJvCPSStartupInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TJvCPSStartupInfo then
    with TJvCPSStartupInfo(Dest) do
    begin
      FDesktop := Self.FDesktop;
      FTitle := Self.FTitle;
      FLeft := Self.FLeft;
      FTop := Self.FTop;
      FDefaultPosition := Self.FDefaultPosition;
      FWidth := Self.FWidth;
      FHeight := Self.FHeight;
      FDefaultSize := Self.FDefaultSize;
      FShowWindow := Self.FShowWindow;
      FDefaultWindowState := Self.FDefaultWindowState;
      FForceOnFeedback := Self.FForceOnFeedback;
      FForceOffFeedback := Self.FForceOffFeedback;
    end
  else
    inherited AssignTo(Dest);
end;

constructor TJvCPSStartupInfo.Create;
begin
  inherited Create;
  FDefaultSize := True;
  FDefaultPosition := True;
  FDefaultWindowState := True;
  FShowWindow := swNormal;
end;

function TJvCPSStartupInfo.GetStartupInfo: TStartupInfo;
const
  ShowWindowValues: array[TJvCPSShowWindow] of DWORD =
  (SW_HIDE, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED, SW_SHOWNORMAL);
begin
  ZeroMemory(@Result, Sizeof(TStartupInfo));
  with Result do
  begin
    cb := Sizeof(TStartupInfo);
    if Length(FDesktop) > 0 then
      lpDesktop := PChar(FDesktop);
    if Length(FTitle) > 0 then
      lpTitle := PChar(Title);
    if not FDefaultPosition then
    begin
      dwX := FLeft;
      dwY := FTop;
      Inc(dwFlags, STARTF_USEPOSITION);
    end;
    if not FDefaultSize then
    begin
      dwXSize := FWidth;
      dwYSize := FHeight;
      Inc(dwFlags, STARTF_USESIZE);
    end;
    if not FDefaultWindowState then
    begin
      wShowWindow := ShowWindowValues[FShowWindow];
      Inc(dwFlags, STARTF_USESHOWWINDOW);
    end;
    if FForceOnFeedback then
      Inc(dwFlags, STARTF_FORCEONFEEDBACK);
    if FForceOffFeedback then
      Inc(dwFlags, STARTF_FORCEOFFFEEDBACK);
  end;
end;

{ TJvCreateProcess }

type
  TJvWaitForProcessThread = class(TThread)
  private
    FExitCode: DWORD;
    FCloseEvent: THandle;
    FProcessHandle: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(ProcessHandle: DWORD);
    destructor Destroy; override;
    procedure TerminateThread;
  end;

procedure TJvCreateProcess.CheckNotWaiting;
begin
  if FState = psWaiting then
    raise EJvProcessError.CreateResRec(@RsProcessIsRunning);
end;

procedure TJvCreateProcess.CheckRunning;
begin
  if FState = psReady then
    raise EJvProcessError.CreateResRec(@RsProcessNotRunning);
end;

function TJvCreateProcess.CloseApplication(SendQuit: Boolean): Boolean;
begin
  CheckRunning;
  Result := InternalCloseApp(ProcessInfo.dwProcessId, SendQuit);
end;

procedure TJvCreateProcess.CloseProcessHandles;
begin
  OSCheck(SafeCloseHandle(FProcessInfo.hProcess));
  OSCheck(SafeCloseHandle(FProcessInfo.hThread));
end;

constructor TJvCreateProcess.Create(AOwner: TComponent);
begin
  inherited;
  FCreationFlags := [];
  FEnvironment := TStringList.Create;
  FPriority := ppNormal;
  FState := psReady;
  FWaitForTerminate := True;
  FStartupInfo := TJvCPSStartupInfo.Create;
end;

destructor TJvCreateProcess.Destroy;
begin
  TerminateWaitThread;
  //  CloseProcessHandles;
  FreeAndNil(FEnvironment);
  FreeAndNil(FStartupInfo);
  inherited;
end;

function TJvCreateProcess.GetProcessInfo: TProcessInformation;
begin
  Result := FProcessInfo;
end;

procedure TJvCreateProcess.Run;
const
  CreationFlagsValues: array[TJvCPSFlag] of DWORD =
  (CREATE_DEFAULT_ERROR_MODE, CREATE_NEW_CONSOLE, CREATE_NEW_PROCESS_GROUP,
    CREATE_SEPARATE_WOW_VDM, CREATE_SHARED_WOW_VDM, CREATE_SUSPENDED,
    CREATE_UNICODE_ENVIRONMENT, DETACHED_PROCESS);
var
  Flags: DWORD;
  F: TJvCPSFlag;
  AppName, CurrDir: PChar;
  EnvironmentData: PChar;
begin
  CheckNotWaiting;
  FState := psReady;

  ZeroMemory(@FProcessInfo, Sizeof(FProcessInfo));
  Flags := ProcessPriorities[FPriority];
  for F := Low(TJvCPSFlag) to High(TJvCPSFlag) do
    if F in FCreationFlags then
      Inc(Flags, CreationFlagsValues[F]);
  AppName := PCharOrNil(Trim(FApplicationName));
  CurrDir := PCharOrNil(Trim(FCurrentDirectory));
  if FEnvironment.Count = 0 then
    EnvironmentData := nil
  else
    StringsToMultiSz(EnvironmentData, FEnvironment);

  if not CreateProcess(AppName, PChar(FCommandLine), nil, nil, False, Flags,
    EnvironmentData, CurrDir, FStartupInfo.StartupInfo, FProcessInfo) then
  begin
    FreeMultiSz(EnvironmentData);
    RaiseLastOSError;
  end
  else
    FreeMultiSz(EnvironmentData);

  if FWaitForTerminate then
  begin
    WaitThread := TJvWaitForProcessThread.Create(FProcessInfo.hProcess);
    WaitThread.OnTerminate := WaitThreadOnTerminate;
    WaitThread.Resume;
    FState := psWaiting;
  end
  else
  begin
    CloseProcessHandles;
    FState := psRunning;
  end;
end;

procedure TJvCreateProcess.SetEnvironment(const Value: TStrings);
begin
  FEnvironment.Assign(Value);
end;

procedure TJvCreateProcess.SetWaitForTerminate(const Value: Boolean);
begin
  CheckNotWaiting;
  FWaitForTerminate := Value;
  FState := psReady;
end;

procedure TJvCreateProcess.StopWaiting;
begin
  TerminateWaitThread;
end;

procedure TJvCreateProcess.Terminate;
begin
  CheckRunning;
  InternalTerminateProcess(FProcessInfo.dwProcessId);
end;

procedure TJvCreateProcess.TerminateWaitThread;
begin
  if (FState = psWaiting) and Assigned(WaitThread) then
  begin
    WaitThread.OnTerminate := nil;
    TJvWaitForProcessThread(WaitThread).TerminateThread;
    FState := psReady;
    CloseProcessHandles;
  end;
end;

procedure TJvCreateProcess.WaitThreadOnTerminate(Sender: TObject);
begin
  FState := psReady;
  WaitThread := nil;
  CloseProcessHandles;
  if Assigned(FOnTerminate) then
    FOnTerminate(Self, TJvWaitForProcessThread(Sender).FExitCode);
end;

{ TJvWaitForProcessThread }

constructor TJvWaitForProcessThread.Create(ProcessHandle: DWORD);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpLower;
  FCloseEvent := CreateEvent(nil, True, False, nil);
  FProcessHandle := ProcessHandle;
end;

destructor TJvWaitForProcessThread.Destroy;
begin
  CloseHandle(FCloseEvent);
  inherited;
end;

procedure TJvWaitForProcessThread.Execute;
var
  WaitHandles: array[0..1] of THandle;
begin
  WaitHandles[0] := FCloseEvent;
  WaitHandles[1] := FProcessHandle;
  WaitForInputIdle(FProcessHandle, INFINITE);
  case WaitForMultipleObjects(2, @WaitHandles, False, INFINITE) of
    WAIT_OBJECT_0:
      FExitCode := MAXDWORD;
    WAIT_OBJECT_0 + 1:
      GetExitCodeProcess(FProcessHandle, FExitCode);
  else
    RaiseLastOSError;
  end;
end;

procedure TJvWaitForProcessThread.TerminateThread;
begin
  Terminate;
  SetEvent(FCloseEvent);
end;

{ TJvFileTreeScan }

procedure TJvFileTreeScan.Abort;
begin
  if FRunning then
    FAborting := True;
end;

function TJvFileTreeScan.AttrFromAttributes(Attributes: TJvFTSAttributes): Integer;
begin
  Result := 0;
  if atReadOnly in Attributes then
    Inc(Result, faReadOnly);
  if atArchive in Attributes then
    Inc(Result, faArchive);
  if atHidden in Attributes then
    Inc(Result, faHidden);
  if atSysFile in Attributes then
    Inc(Result, faSysFile);
end;

constructor TJvFileTreeScan.Create(AOwner: TComponent);
begin
  inherited;
  FDirAttributes := [atReadOnly, atArchive];
  FFileAttributes := [atReadOnly, atArchive, atHidden, atSysFile];
  FScanFiles := True;
  FSorted := True;
  FMaskComparator := TJclFileMaskComparator.Create;
end;

destructor TJvFileTreeScan.Destroy;
begin
  FreeAndNil(FMaskComparator);
  inherited;
end;

procedure TJvFileTreeScan.InternalScan(const DirectoryName: string);
var
  List: TStringList;
  Mask, DirSep: string;
  Se: TSearchRec;
  I, RetCode: Integer;
  Attr: Integer;
  SimpleCompare: Boolean;

  function CheckSuccess: Boolean;
  begin
    Result := (RetCode = ERROR_SUCCESS) or (RetCode = ERROR_NO_MORE_FILES) or
      (RetCode = ERROR_FILE_NOT_FOUND);
    if not Result then
    begin
      List.Clear;
      if FAbortOnError then
        FAborting := True
      else
        RaiseLastOSError;
    end;
  end;

begin
  if FAborting then
    Exit;
  List := TStringList.Create;
  try
    List.Sorted := FSorted;
    Inc(FTotalDirectories);
    DirSep := PathAddSeparator(DirectoryName);
    if Assigned(FOnDirectory) then
      FOnDirectory(Self, DirectoryName);

    if not FAborting and FScanFiles then
    begin
      Attr := AttrFromAttributes(FDirAttributes);
      SimpleCompare := FMaskComparator.Count <= 1;
      if FMaskComparator.Count <> 1 then
        Mask := '*.*'
      else
        Mask := FFileMask;
      RetCode := FindFirst(DirSep + Mask, Attr, Se);
      if CheckSuccess then
      begin
        try
          while RetCode = 0 do
          begin
            if SimpleCompare or FMaskComparator.Compare(Se.Name) then
              List.Add(Se.Name);
            RetCode := FindNext(Se);
          end;
          CheckSuccess;
        finally
          FindClose(Se);
        end;
        for I := 0 to List.Count - 1 do
        begin
          Inc(FTotalFiles);
          if Assigned(FOnFile) then
            FOnFile(Self, DirSep + List[I]);
          if FAborting then
            Break;
        end;
      end;
    end;

    List.Clear;
    Attr := (AttrFromAttributes(FDirAttributes) or faDirectory) xor $FF;
    RetCode := FindFirst(DirSep + '*.*', faAnyFile, Se);
    if CheckSuccess then
    begin
      try
        while RetCode = 0 do
        begin
          if (Se.Attr and faDirectory <> 0) and (Se.Attr and Attr = 0) and
            (Se.Name <> '.') and (Se.Name <> '..') then
            List.Add(Se.Name);
          RetCode := FindNext(Se);
        end;
        CheckSuccess;
      finally
        FindClose(Se);
      end;
      for I := 0 to List.Count - 1 do
      begin
        InternalScan(DirSep + List[I]);
        if FAborting then
          Break;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TJvFileTreeScan.Scan;
begin
  if FRunning then
    Exit;
  FAborting := False;
  FTotalDirectories := 0;
  FTotalFiles := 0;
  if not IsDirectory(FRootDirectory) then
    RaiseLastOSError;
  FRunning := True;
  try
    InternalScan(FRootDirectory);
  finally
    FRunning := False;
  end;
end;

procedure TJvFileTreeScan.SetFileMask(const Value: string);
begin
  if FFileMask <> Value then
  begin
    FFileMask := Value;
    FMaskComparator.FileMask := FFileMask;
  end;
end;

end.
