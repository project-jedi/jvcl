{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSysComp.PAS, released Dec 26, 1999.

The Initial Developer of the Original Code is Petr Vones (petr dott v att mujmail dott cz)
Portions created by Petr Vones are Copyright (C) 1999 Petr Vones.
Portions created by Microsoft are Copyright (C) 1998, 1999 Microsoft Corp.
All Rights Reserved.

Contributor(s):
  Marcel van Brakel <brakelm att bart dott nl>.
  Remko Bonte <remkobonte att myrealbox dott com> (redirect console output)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvCreateProcess;

interface

uses
  Windows, Messages, SysUtils, Classes,
  {$IFNDEF COMPILER6_UP}
  Forms,
  {$ENDIF !COMPILER6_UP}
  ShellAPI, SyncObjs, Contnrs,
  JclStrings,
  JvComponent, JvTypes;

const
  CCPS_BufferSize = 1024;
  CCPS_MaxBufferSize = 65536;

type
  EJvProcessError = EJVCLException;

  TJvProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime);

  TJvConsoleOption = (coOwnerData, coRedirect);
  TJvConsoleOptions = set of TJvConsoleOption;

  TJvCPSRawReadEvent = procedure(Sender: TObject; const S: string) of object;
  TJvCPSReadEvent = procedure(Sender: TObject; const S: string; const StartsOnNewLine: Boolean) of object;
  TJvCPSTerminateEvent = procedure(Sender: TObject; ExitCode: DWORD) of object;

  TJvRWHandles = record
    Read: THandle;
    Write: THandle;
  end;

  TJvRWEHandles = record
    Read: THandle;
    Write: THandle;
    Error: THandle;
  end;

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
    property LargeIconIndex: Integer index SHGFI_LARGEICON read
      GetSystemIconIndex;
    property Priority: TJvProcessPriority read GetPriority write SetPriority;
    property ProcessID: DWORD read FProcessID;
    property ProcessName: string read FProcessName;
    property SmallIconIndex: Integer index SHGFI_SMALLICON read
      GetSystemIconIndex;
  end;

  TJvCPSBuffer = array [0..CCPS_BufferSize - 1] of Char;
  TJvCPSState = (psReady, psRunning, psWaiting);
  TJvCPSFlag = (cfDefaultErrorMode, cfNewConsole, cfNewProcGroup, cfSeparateWdm,
    cfSharedWdm, cfSuspended, cfUnicode, cfDetached);
  TJvCPSFlags = set of TJvCPSFlag;
  TJvCPSShowWindow = (swHide, swMinimize, swMaximize, swNormal);

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
    property DefaultPosition: Boolean read FDefaultPosition write
      FDefaultPosition default True;
    property Width: Integer read FWidth write FWidth default 0;
    property Height: Integer read FHeight write FHeight default 0;
    property DefaultSize: Boolean read FDefaultSize write FDefaultSize default True;
    property ShowWindow: TJvCPSShowWindow read FShowWindow write FShowWindow
      default swNormal;
    property DefaultWindowState: Boolean read FDefaultWindowState write
      FDefaultWindowState default True;
    property ForceOnFeedback: Boolean read FForceOnFeedback write
      FForceOnFeedback default False;
    property ForceOffFeedback: Boolean read FForceOffFeedback write
      FForceOffFeedback default False;
  end;

  TJvCreateProcess = class(TJvComponent)
  private
    FApplicationName: string;
    FCommandLine: string;
    FCreationFlags: TJvCPSFlags;
    FCurrentDirectory: string;
    FEnvironment: TStringList;
    FState: TJvCPSState;
    FStartupInfo: TJvCPSStartupInfo;
    FPriority: TJvProcessPriority;
    FProcessInfo: TProcessInformation;
    FWaitForTerminate: Boolean;
    FConsoleOptions: TJvConsoleOptions;
    FOnTerminate: TJvCPSTerminateEvent;
    FOnRead: TJvCPSReadEvent;
    FOnRawRead: TJvCPSRawReadEvent;
    FWaitThread: TThread;
    FReadThread: TThread;
    FHandle: THandle;
    FCurrentLine: string; // Last output of the console with no #10 char.
    FCursorPosition: Integer; // Position of the cursor on FCurrentLine
    FConsoleOutput: TStringList;
    FParseBuffer: TJvCPSBuffer;
    FExitCode: Cardinal;
    FEndLock: TCriticalSection; // lock to synchronize ending of the threads
    FStartsOnNewLine: Boolean;
    function GetConsoleOutput: TStrings;
    function GetEnvironment: TStrings;
    procedure SetWaitForTerminate(const Value: Boolean);
    procedure WaitThreadTerminated(Sender: TObject);
    procedure ConsoleWaitThreadTerminated(Sender: TObject);
    procedure ReadThreadTerminated(Sender: TObject);
    procedure SetEnvironment(const Value: TStrings);
    function GetHandle: THandle;

    procedure StartConsoleEnding;
    procedure EndConsoleEnding;
  protected
    procedure CheckRunning;
    procedure CheckNotWaiting;
    procedure CloseProcessHandles;
    procedure TerminateWaitThread;
    procedure HandleReadEvent;
    procedure ParseConsoleOutput(Data: PChar; ASize: Cardinal);
    procedure DoReadEvent(const EndsWithNewLine: Boolean);
    procedure DoRawReadEvent(Data: PChar; const ASize: Cardinal);
    procedure DoTerminateEvent;
    procedure WndProc(var Msg: TMessage);
    property Handle: THandle read GetHandle;
    procedure CloseRead;
    procedure CloseWrite;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CloseApplication(SendQuit: Boolean = False): Boolean;
    procedure Run;
    procedure StopWaiting;
    procedure Terminate;
    function Write(const S: string): Boolean;
    function WriteLn(const S: string): Boolean;
    property ProcessInfo: TProcessInformation read FProcessInfo;
    property State: TJvCPSState read FState;
    property ConsoleOutput: TStrings read GetConsoleOutput;
  published
    property ApplicationName: string read FApplicationName write FApplicationName;
    property CommandLine: string read FCommandLine write FCommandLine;
    property CreationFlags: TJvCPSFlags read FCreationFlags write FCreationFlags
      default [];
    property CurrentDirectory: string read FCurrentDirectory write
      FCurrentDirectory;
    property Environment: TStrings read GetEnvironment write SetEnvironment;
    property Priority: TJvProcessPriority read FPriority write FPriority default
      ppNormal;
    property StartupInfo: TJvCPSStartupInfo read FStartupInfo write
      FStartupInfo;
    property WaitForTerminate: Boolean read FWaitForTerminate write
      SetWaitForTerminate default True;
    property ConsoleOptions: TJvConsoleOptions read FConsoleOptions write
      FConsoleOptions default [coOwnerData];
    property OnTerminate: TJvCPSTerminateEvent read FOnTerminate write
      FOnTerminate;
    property OnRead: TJvCPSReadEvent read FOnRead write FOnRead;
    property OnRawRead: TJvCPSRawReadEvent read FOnRawRead write FOnRawRead;
  end;

implementation

uses
  Math,
  JclSysUtils,
  JvJCLUtils, JvJVCLUtils, JvConsts, JvResources;

const
  CM_READ = WM_USER + 1;

  //MaxProcessCount = 4096;
  ProcessPriorities: array [TJvProcessPriority] of DWORD =
    (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
     REALTIME_PRIORITY_CLASS);

type
  { Threads which monitor the created process }

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

  TJvConsoleThread = class(TJvWaitForProcessThread)
  private
    // Write end of the pipe
    FWriteHandle: THandle;
    FWriteEvent: THandle;
    // Critical sections to synchronize access to the buffers
    FWriteLock: TCriticalSection;
    // Fixed size buffer; maybe change to sizeable
    FOutputBuffer: TJvCPSBuffer;
    FOutputBufferEnd: Cardinal;
  protected
    procedure Execute; override;
    function TryWrite: Boolean;
  public
    constructor Create(ProcessHandle: DWORD; AWriteHandle: THandle);
    destructor Destroy; override;
    function write(const S: string): Boolean;
    procedure CloseWrite;
  end;

  TJvReadThread = class(TThread)
  private
    // Read end of the pipe
    FReadHandle: THandle;
    // Critical sections to synchronize access to the buffers
    FReadLock: TCriticalSection;
    // Handle to the TJvCreateProcess
    FDestHandle: THandle;
    FPreBuffer: PChar;
    FInputBuffer: PChar;
    FInputBufferSize: Cardinal;
    FInputBufferEnd: Cardinal;
  protected
    FLoopEntered: Boolean;
    FTerminateAfterLoopEntered: Boolean;
    procedure CopyToBuffer(Buffer: PChar; ASize: Cardinal);
    procedure Execute; override;
  public
    constructor Create(AReadHandle, ADestHandle: THandle);
    destructor Destroy; override;
    procedure CloseRead;
    function ReadBuffer(var ABuffer: TJvCPSBuffer; out ABufferSize: Cardinal): Boolean;
    procedure TerminateThread;
  end;

//=== Local procedures =======================================================

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
    if PID = Param.ProcessID then
    begin
      if Param.PostQuit then
        PostThreadMessage(TID, WM_QUIT, 0, 0)
      else
      if IsWindowVisible(Wnd) then
        PostMessage(Wnd, WM_CLOSE, 0, 0);
      Param.FoundWin := True;
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

function ConstructPipe(var ConsoleHandles: TJvRWEHandles; var LocalHandles: TJvRWHandles): Boolean;
var
  LHandles: TJvRWHandles;
  LSecurityAttr: TSecurityAttributes;
  LSecurityDesc: TSecurityDescriptor;

  procedure CloseAllHandles;
  begin
    // Some error occurred; close all possibly created handles
    SafeCloseHandle(ConsoleHandles.Read);
    SafeCloseHandle(ConsoleHandles.Write);
    SafeCloseHandle(ConsoleHandles.Error);
    SafeCloseHandle(LocalHandles.Read);
    SafeCloseHandle(LocalHandles.Write);
    SafeCloseHandle(LHandles.Read);
    SafeCloseHandle(LHandles.Write);
  end;

begin
  { http://support.microsoft.com/default.aspx?scid=KB;EN-US;q190351& }
  { http://community.borland.com/article/0,1410,10387,00.html }
  FillChar(LSecurityAttr, SizeOf(TSecurityAttributes), 0);
  FillChar(ConsoleHandles, SizeOf(TJvRWEHandles), 0);
  FillChar(LocalHandles, SizeOf(TJvRWHandles), 0);
  FillChar(LHandles, SizeOf(TJvRWHandles), 0);

  // Set up the security attributes struct.
  LSecurityAttr.nLength := SizeOf(TSecurityAttributes);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    // Initialize security descriptor (Windows NT)
    InitializeSecurityDescriptor(@LSecurityDesc, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@LSecurityDesc, True, nil, False);
    LSecurityAttr.lpSecurityDescriptor := @LSecurityDesc;
  end
  else
    LSecurityAttr.lpSecurityDescriptor := nil;
  LSecurityAttr.bInheritHandle := True;

  // Create the child output pipe.
  Result := CreatePipe(LHandles.Read, ConsoleHandles.Write, @LSecurityAttr, 0);
  if not Result then
  begin
    CloseAllHandles;
    Exit;
  end;

  { Create a duplicate of the output write handle for the std error
    write handle. This is necessary in case the child application
    closes one of its std output handles.
  }
  Result := DuplicateHandle(GetCurrentProcess, ConsoleHandles.Write,
    GetCurrentProcess,
    @ConsoleHandles.Error, // Address of new handle.
    0, True, // Make it inheritable.
    DUPLICATE_SAME_ACCESS);
  if not Result then
  begin
    CloseAllHandles;
    Exit;
  end;

  // Create the child input pipe.
  Result := CreatePipe(ConsoleHandles.Read, LHandles.Write, @LSecurityAttr, 0);
  if not Result then
  begin
    CloseAllHandles;
    Exit;
  end;

  { Create new output read handle and the input write handles. Set
    the Properties to FALSE. Otherwise, the child inherits the
    properties and, as a result, non-closeable handles to the pipes
    are created.
  }
  Result := DuplicateHandle(GetCurrentProcess, LHandles.Read,
    GetCurrentProcess,
    @LocalHandles.Read, // Address of new handle.
    0, False, // Make it uninheritable.
    DUPLICATE_SAME_ACCESS);
  if not Result then
  begin
    CloseAllHandles;
    Exit;
  end;

  Result := DuplicateHandle(GetCurrentProcess, LHandles.Write,
    GetCurrentProcess,
    @LocalHandles.Write, // Address of new handle.
    0, False, // Make it uninheritable.
    DUPLICATE_SAME_ACCESS);
  if not Result then
  begin
    CloseAllHandles;
    Exit;
  end;

  { Okay, everything went as expected; now close inheritable copies of the
    handles you do not want to be inherited.
  }
  SafeCloseHandle(LHandles.Read);
  SafeCloseHandle(LHandles.Write);
end;

//=== { TJvProcessEntry } ====================================================

constructor TJvProcessEntry.Create(AProcessID: DWORD;
  const AFileName: TFileName; const AProcessName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FProcessID := AProcessID;
  FProcessName := AProcessName;
end;

function TJvProcessEntry.Close(UseQuit: Boolean): Boolean;
begin
  Result := InternalCloseApp(ProcessID, UseQuit);
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
        NORMAL_PRIORITY_CLASS:
          Result := ppNormal;
        IDLE_PRIORITY_CLASS:
          Result := ppIdle;
        HIGH_PRIORITY_CLASS:
          Result := ppHigh;
        REALTIME_PRIORITY_CLASS:
          Result := ppRealTime;
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
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  SHGetFileInfo(PChar(FileName), 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or IconType);
  Result := FileInfo.iIcon;
end;

class function TJvProcessEntry.PriorityText(Priority: TJvProcessPriority): string;
begin
  case Priority of
    ppIdle:
      Result := RsIdle;
    ppNormal:
      Result := RsNormal;
    ppHigh:
      Result := RsHigh;
    ppRealTime:
      Result := RsRealTime;
  end;
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

//=== { TJvCPSStartupInfo } ==================================================

constructor TJvCPSStartupInfo.Create;
begin
  inherited Create;
  FDefaultSize := True;
  FDefaultPosition := True;
  FDefaultWindowState := True;
  FShowWindow := swNormal;
end;

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

function TJvCPSStartupInfo.GetStartupInfo: TStartupInfo;
const
  ShowWindowValues: array [TJvCPSShowWindow] of DWORD =
    (SW_HIDE, SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED, SW_SHOWNORMAL);
begin
  FillChar(Result, SizeOf(TStartupInfo), #0);
  with Result do
  begin
    cb := SizeOf(TStartupInfo);
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

//=== { TJvWaitForProcessThread } ============================================

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
  SafeCloseHandle(FCloseEvent);
  inherited Destroy;
end;

procedure TJvWaitForProcessThread.Execute;
var
  WaitHandles: array [0..1] of THandle;
begin
  WaitHandles[0] := FCloseEvent;
  WaitHandles[1] := FProcessHandle;
  WaitForInputIdle(FProcessHandle, INFINITE);
  case WaitForMultipleObjects(2, PWOHandleArray(@WaitHandles[0]), False, INFINITE) of
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

//=== { TJvReadThread } ======================================================

constructor TJvReadThread.Create(AReadHandle, ADestHandle: THandle);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpLower;

  FReadLock := TCriticalSection.Create;

  // Note: TJvReadThread is responsible for closing the FReadHandle
  FReadHandle := AReadHandle;
  FDestHandle := ADestHandle;

  FInputBuffer := nil;
  FInputBufferSize := CCPS_BufferSize;
  FInputBufferEnd := 0;
  ReallocMem(FInputBuffer, FInputBufferSize);
  GetMem(FPreBuffer, CCPS_BufferSize);
end;

destructor TJvReadThread.Destroy;
begin
  SafeCloseHandle(FReadHandle);
  inherited Destroy;
  { It is (theoretically) possible that the inherited Destroy triggers an
    OnTerminate event and the following fields can be accessed in the handler,
    thus free them after the destroy.
  }
  ReallocMem(FInputBuffer, 0);
  FReadLock.Free;
  FreeMem(FPreBuffer);
end;

procedure TJvReadThread.CloseRead;
begin
  FReadLock.Acquire;
  try
    SafeCloseHandle(FReadHandle);
  finally
    FReadLock.Release;
  end;
end;

procedure TJvReadThread.CopyToBuffer(Buffer: PChar; ASize: Cardinal);
// Copy data in Buffer (with size ASize) to FInputBuffer.
begin
  FReadLock.Acquire;
  try
    if FInputBufferEnd + ASize > FInputBufferSize then
    begin
      // Safety check..
      if FInputBufferSize > CCPS_MaxBufferSize then
        // ..main thread seems to be blocked; flush the input buffer
        FInputBufferEnd := 0
      else
      begin
        // Need to upscale FInputBuffer
        FInputBufferSize := FInputBufferSize * 2;
        ReallocMem(FInputBuffer, FInputBufferSize);
      end;
    end;

    // Do the copy
    Move(Buffer[0], FInputBuffer[FInputBufferEnd], ASize);
    Inc(FInputBufferEnd, ASize);
  finally
    FReadLock.Release;
  end;

  // Notify TJvCreateProcess that data has been read from the pipe
  PostMessage(FDestHandle, CM_READ, 0, 0);
end;

procedure TJvReadThread.Execute;
// Read data from the pipe (FReadHandle) to FPreBuffer
var
  BytesRead: Cardinal;
begin
  { FTerminateAfterLoopEntered and FLoopEntered ensure that the loop is
    entered minimal once, ie ReadFile is called minimal once. This ensures that
    the output of very fast finishing executables is read. (We can't change
    the loop to repeat until and call Terminate because Terminated is checked
    in Classes.ThreadProc just before Execute is entered.)
  }

  while not Terminated or (FTerminateAfterLoopEntered and FLoopEntered) do
  begin
    FLoopEntered := True;

    { ReadFile will block until *some* data is available on the pipe }
    if not ReadFile(FReadHandle, FPreBuffer[0], CCPS_BufferSize, BytesRead, nil) then
    begin
      // Only exit if last error is ERROR_BROKEN_PIPE, thus ignore other errors
      if GetLastError = ERROR_BROKEN_PIPE then
        // pipe done - normal exit path.
        Exit;
    end
    else
      CopyToBuffer(FPreBuffer, BytesRead);
  end;
end;

function TJvReadThread.ReadBuffer(var ABuffer: TJvCPSBuffer;
  out ABufferSize: Cardinal): Boolean;
// Copy FInputBuffer to ABuffer.
// This function is executed in the context of the main thread;
// FReadLock is for synchronization with the read thread.
begin
  FReadLock.Acquire;
  try
    Result := FInputBufferEnd > 0;
    if not Result then
      Exit;

    ABufferSize := Min(FInputBufferEnd, CCPS_BufferSize);

    // Copy the data from FInputBuffer to ABuffer.
    Move(FInputBuffer[0], ABuffer[0], ABufferSize);

    // If not all data in FInputBuffer is copied to ABuffer, then place
    // the data not copied at the begin of FInputBuffer.
    if FInputBufferEnd > ABufferSize then
      Move(FInputBuffer[ABufferSize], FInputBuffer[0],
        FInputBufferEnd - ABufferSize);

    Dec(FInputBufferEnd, ABufferSize);
  finally
    FReadLock.Release;
  end;
end;

procedure TJvReadThread.TerminateThread;
begin
  if FLoopEntered then
  begin
    Terminate;
    CloseRead;
  end
  else
    FTerminateAfterLoopEntered := True;
end;

//=== { TJvConsoleThread } ===================================================

constructor TJvConsoleThread.Create(ProcessHandle: DWORD;
  AWriteHandle: THandle);
begin
  inherited Create(ProcessHandle);

  FWriteLock := TCriticalSection.Create;

  // Note: TJvConsoleThread is responsible for closing the FWriteHandle
  FWriteHandle := AWriteHandle;

  FWriteEvent := CreateEvent(
    nil, // No security attributes
    True, // Manual reset
    False, // Initial state
    nil // No name
    );
end;

destructor TJvConsoleThread.Destroy;
begin
  SafeCloseHandle(FWriteHandle);
  SafeCloseHandle(FWriteEvent);
  inherited Destroy;
  { It is (theoretically) possible that the inherited Destroy triggers an
    OnTerminate event and the following fields can be accessed in the handler,
    thus free them after the destroy.
  }
  FWriteLock.Free;
end;

procedure TJvConsoleThread.CloseWrite;
begin
  FWriteLock.Acquire;
  try
    SafeCloseHandle(FWriteHandle);
  finally
    FWriteLock.Release;
  end;
end;

procedure TJvConsoleThread.Execute;
var
  WaitHandles: array [0..2] of THandle;
  HandleCount: Cardinal;
begin
  WaitHandles[0] := FCloseEvent;
  WaitHandles[1] := FProcessHandle;
  WaitHandles[2] := FWriteEvent;
  HandleCount := 3;

  WaitForInputIdle(FProcessHandle, INFINITE);

  while not Terminated do
    case WaitForMultipleObjects(HandleCount, PWOHandleArray(@WaitHandles[0]), False, INFINITE) of
      WAIT_OBJECT_0:
        begin
          // Close event fired; exit
          FExitCode := MAXDWORD;
          Exit;
        end;
      WAIT_OBJECT_0 + 1:
        begin
          // process ended; exit
          GetExitCodeProcess(FProcessHandle, FExitCode);
          Exit;
        end;
      WAIT_OBJECT_0 + 2:
        // Write event fired; try to write
        if not TryWrite then
          // No longer respond when write event fires
          HandleCount := 2;
    else
      Exit;
    end;
end;

function TJvConsoleThread.TryWrite: Boolean;
// Write data in FOutputBuffer to the pipe (FWriteHandle)
// Result = False; if console or user has closed the pipe.
var
  BytesWritten: Cardinal;
  BytesToWrite: Cardinal;
begin
  Result := True;

  FWriteLock.Acquire;
  try
    try
      { Check handle inside lock, because it can be closed by another thread, by
        calling CloseWrite }
      if FWriteHandle = 0 then
        Exit;

      if FOutputBufferEnd <= 0 then
        Exit;

      BytesToWrite := FOutputBufferEnd;

      if not WriteFile(FWriteHandle, FOutputBuffer, BytesToWrite, BytesWritten, nil) then
      begin
        { WriteFile documentation on MSDN states that WriteFile returns
          ERROR_BROKEN_PIPE if the console closes it's read handle, but that
          seems incorrect; check it anyway }
        if (GetLastError = ERROR_NO_DATA) or (GetLastError = ERROR_BROKEN_PIPE) then
          // Pipe was closed (normal exit path).
          SafeCloseHandle(FWriteHandle);
        Exit;
      end;

      if BytesWritten <= 0 then
        Exit;

      if BytesWritten < BytesToWrite then
        // Move unwritten tail to the begin of the buffer
        Move(FOutputBuffer[BytesWritten], FOutputBuffer[0], BytesToWrite - BytesWritten);

      Dec(FOutputBufferEnd, BytesWritten);
    finally
      Result := FWriteHandle <> 0;
      if FOutputBufferEnd = 0 then
        ResetEvent(FWriteEvent);
    end;
  finally
    FWriteLock.Release;
  end;
end;

function TJvConsoleThread.Write(const S: string): Boolean;
// Add S to FOutputBuffer; actual writing is done in TryWrite.
// This function is executed in the context of the main thread;
// FWriteLock is for synchronization with the write thread.
begin
  if Length(S) <= 0 then
  begin
    Result := True;
    Exit;
  end;

  FWriteLock.Acquire;
  try
    Result := FWriteHandle <> 0;
    if not Result then
      Exit;

    Result := Cardinal(Length(S)) + FOutputBufferEnd <= CCPS_BufferSize;
    if not Result then
      Exit;

    Move(PChar(S)^, FOutputBuffer[FOutputBufferEnd], Length(S));
    Inc(FOutputBufferEnd, Length(S));

    if FOutputBufferEnd > 0 then
      // Notify the TJvConsoleThread that there is some data to write
      SetEvent(FWriteEvent);
  finally
    FWriteLock.Release;
  end;
end;

//=== { TJvCreateProcess } ===================================================

constructor TJvCreateProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreationFlags := [];
  FEnvironment := TStringList.Create;
  FPriority := ppNormal;
  FState := psReady;
  FWaitForTerminate := True;
  FStartupInfo := TJvCPSStartupInfo.Create;
  FConsoleOutput := TStringList.Create;
  FConsoleOptions := [coOwnerData];
end;

destructor TJvCreateProcess.Destroy;
begin
  TerminateWaitThread;
  //  CloseProcessHandles;
  FreeAndNil(FEndLock);
  FreeAndNil(FEnvironment);
  FreeAndNil(FStartupInfo);
  if FHandle <> 0 then
    DeallocateHWndEx(FHandle);
  inherited Destroy;
  FConsoleOutput.Free;
end;

procedure TJvCreateProcess.CheckNotWaiting;
begin
  if FState = psWaiting then
    raise EJvProcessError.CreateRes(@RsEProcessIsRunning);
end;

procedure TJvCreateProcess.CheckRunning;
begin
  if FState = psReady then
    raise EJvProcessError.CreateRes(@RsEProcessNotRunning);
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

procedure TJvCreateProcess.CloseRead;
begin
  if FReadThread is TJvReadThread then
    TJvReadThread(FReadThread).CloseRead;
end;

procedure TJvCreateProcess.CloseWrite;
begin
  if FWaitThread is TJvConsoleThread then
    TJvConsoleThread(FWaitThread).CloseWrite;
end;

procedure TJvCreateProcess.ConsoleWaitThreadTerminated(Sender: TObject);
var
  AllThreadsDone: Boolean;
begin
  StartConsoleEnding;
  try
    if Assigned(FReadThread) then
      TJvReadThread(FReadThread).TerminateThread;

    FExitCode := TJvWaitForProcessThread(Sender).FExitCode;

    { We only fire a TerminateEvent if both the read thread and the wait thread
      have terminated; usually the read thread will terminate before the wait
      thread; must be determined inside the lock (FEndLock) }
    AllThreadsDone := FReadThread = nil;

    // Indicates that the wait thread is done; must bo set inside lock.
    FWaitThread := nil;
  finally
    EndConsoleEnding;
  end;

  if AllThreadsDone then
    DoTerminateEvent;
end;

procedure TJvCreateProcess.DoRawReadEvent(Data: PChar; const ASize: Cardinal);
var
  S: string;
begin
  if Assigned(FOnRawRead) then
  begin
    // Do copy because of possible #0's etc.
    SetLength(S, ASize);
    Move(Data^, PChar(S)^, ASize);
    FOnRawRead(Self, S);
  end;
end;

procedure TJvCreateProcess.DoReadEvent(const EndsWithNewLine: Boolean);
begin
  // Notify user and update current line & cursor
  if not (coOwnerData in ConsoleOptions) then
  begin
    if FStartsOnNewLine or (ConsoleOutput.Count = 0) then
      ConsoleOutput.Add(FCurrentLine)
    else
      ConsoleOutput[ConsoleOutput.Count - 1] := FCurrentLine;
  end;
  if Assigned(FOnRead) then
    FOnRead(Self, FCurrentLine, FStartsOnNewLine);
  if EndsWithNewLine then
  begin
    FCurrentLine := '';
    FCursorPosition := 0;
  end;
  FStartsOnNewLine := EndsWithNewLine;
end;

procedure TJvCreateProcess.DoTerminateEvent;
begin
  FState := psReady;
  FreeAndNil(FEndLock);
  CloseProcessHandles;
  if Assigned(FOnTerminate) then
    FOnTerminate(Self, FExitCode);
end;

procedure TJvCreateProcess.EndConsoleEnding;
begin
  FEndLock.Leave;
end;

function TJvCreateProcess.GetConsoleOutput: TStrings;
begin
  Result := FConsoleOutput;
end;

function TJvCreateProcess.GetEnvironment: TStrings;
begin
  Result := FEnvironment;
end;

function TJvCreateProcess.GetHandle: THandle;
begin
  if FHandle = 0 then
    FHandle := AllocateHWndEx(WndProc);
  Result := FHandle;
end;

procedure TJvCreateProcess.HandleReadEvent;
var
  ASize: Cardinal;
begin
  { Copy the data from the read thread to the this (main) thread and
    parse the console output }
  if FReadThread is TJvReadThread then
    while Assigned(FReadThread) and
      TJvReadThread(FReadThread).ReadBuffer(FParseBuffer, ASize) do
      ParseConsoleOutput(FParseBuffer, ASize);
end;

procedure TJvCreateProcess.ParseConsoleOutput(Data: PChar; ASize: Cardinal);
var
  P, Q: PChar;

  procedure DoOutput;
  { Copy chunk [Q..P) to the current line & Update cursor position }
  var
    ChunkSize: Integer;
  begin
    ChunkSize := P - Q;
    if ChunkSize <= 0 then
      Exit;

    // Does the chunck fit on the current line..
    if Length(FCurrentLine) < FCursorPosition + ChunkSize then
      // .. if not resize current line
      SetLength(FCurrentLine, FCursorPosition + ChunkSize);

    // Move the chunk to the current line
    Move(Q^, (PChar(FCurrentLine) + FCursorPosition)^, ChunkSize);

    // Update the cursor
    Inc(FCursorPosition, ChunkSize);
  end;

  procedure DoTab;
  begin
    // Does the chunck (8 spaces) fit on the current line..
    if Length(FCurrentLine) < FCursorPosition + 8 then
      // .. if not resize current line }
      SetLength(FCurrentLine, FCursorPosition + 8);

    // Fill 8 spaces on the currentline at the cursor position
    FillChar((PChar(FCurrentLine) + FCursorPosition)^, 8, #32);

    // Update the cursor
    Inc(FCursorPosition, 8);
  end;

begin
  DoRawReadEvent(Data, ASize);

  P := Data;
  Q := Data;

  while Cardinal(P - Data) < ASize do
    case P^ of
      #0, #7: // NULL and BELL
        begin
          // Replace with space 
          P^ := #32;
          Inc(P);
        end;
      Backspace:
        begin
          DoOutput;
          Dec(FCursorPosition);
          if FCursorPosition < 0 then
            FCursorPosition := 0;
          Inc(P);
          Q := P;
        end;
      Tab:
        begin
          // Replace with 8 spaces
          DoOutput;
          DoTab;
          Inc(P);
          Q := P;
        end;
      Lf:
        begin
          DoOutput;
          DoReadEvent(True);
          Inc(P);
          Q := P;
        end;
      Cr:
        begin
          DoOutput;
          FCursorPosition := 0;
          Inc(P);
          Q := P;
        end;
    else
      Inc(P);
    end;
  DoOutput;
  DoReadEvent(False);
end;

procedure TJvCreateProcess.ReadThreadTerminated(Sender: TObject);
var
  AllThreadsDone: Boolean;
begin
  StartConsoleEnding;
  try
    // Read for the last time data from the read thread
    HandleReadEvent;
    if FCurrentLine <> '' then
      DoReadEvent(False);

    { We only fire a TerminateEvent if both the read thread and the wait thread
      have terminated; usually the read thread will terminate before the wait
      thread; must be determined inside the lock (FEndLock) }
    AllThreadsDone := FWaitThread = nil;

    // Indicates that the wait thread is done; must bo set inside lock.
    FReadThread := nil;
  finally
    EndConsoleEnding;
  end;

  if AllThreadsDone then
    DoTerminateEvent;
end;

procedure TJvCreateProcess.Run;
const
  CreationFlagsValues: array [TJvCPSFlag] of DWORD =
    (CREATE_DEFAULT_ERROR_MODE, CREATE_NEW_CONSOLE, CREATE_NEW_PROCESS_GROUP,
     CREATE_SEPARATE_WOW_VDM, CREATE_SHARED_WOW_VDM, CREATE_SUSPENDED,
     CREATE_UNICODE_ENVIRONMENT, DETACHED_PROCESS);
var
  LConsoleHandles: TJvRWEHandles; // Handles which the console will use
  LLocalHandles: TJvRWHandles; // Handles which we will use
  LStartupInfo: TStartupInfo;
  Flags: DWORD;
  F: TJvCPSFlag;
  AppName, CurrDir: PChar;
  EnvironmentData: PChar;
  DoRedirect: Boolean;
begin
  CheckNotWaiting;
  FState := psReady;
  FStartsOnNewLine := True;
  FCurrentLine := '';
  FCursorPosition := 0;
  DoRedirect := coRedirect in ConsoleOptions;

  FillChar(FProcessInfo, SizeOf(FProcessInfo), #0);
  FillChar(LLocalHandles, SizeOf(LLocalHandles), #0);
  FillChar(LConsoleHandles, SizeOf(LConsoleHandles), #0);

  Flags := ProcessPriorities[FPriority];
  for F := Low(TJvCPSFlag) to High(TJvCPSFlag) do
    if F in FCreationFlags then
      Inc(Flags, CreationFlagsValues[F]);
  AppName := PCharOrNil(Trim(FApplicationName));
  CurrDir := PCharOrNil(Trim(FCurrentDirectory));
  if Environment.Count = 0 then
    EnvironmentData := nil
  else
    StringsToMultiSz(EnvironmentData, Environment);

  try
    LStartupInfo := FStartupInfo.GetStartupInfo;

    if DoRedirect then
    begin
      if not ConstructPipe(LConsoleHandles, LLocalHandles) then
        RaiseLastOSError;

      with LStartupInfo do
      begin
        dwFlags := dwFlags or STARTF_USESTDHANDLES;
        hStdOutput := LConsoleHandles.Write;
        hStdInput := LConsoleHandles.Read;
        hStdError := LConsoleHandles.Error;
      end;
    end;

    if not CreateProcess(AppName, PChar(FCommandLine), nil, nil, DoRedirect,
      Flags, EnvironmentData, CurrDir, LStartupInfo, FProcessInfo) then
    begin
      CloseProcessHandles;
      SafeCloseHandle(LLocalHandles.Write);
      SafeCloseHandle(LLocalHandles.Read);
      RaiseLastOSError;
    end;

    if DoRedirect then
    begin
      { (rb) We assume that a thread is done if its pointer (FReadThread/FWaitThread)
             is nil (See code of ReadThreadTerminated and WaitThreadTerminated).
             Thus we have to create both threads before we start any of them
             (otherwise it will go wrong with very fast finishing executables)
             (See Mantis #1393)
      }
      FState := psWaiting;

      FReadThread := TJvReadThread.Create(LLocalHandles.Read, Handle);
      FReadThread.OnTerminate := ReadThreadTerminated;

      FWaitThread := TJvConsoleThread.Create(FProcessInfo.hProcess, LLocalHandles.Write);
      FWaitThread.OnTerminate := ConsoleWaitThreadTerminated;

      FReadThread.Resume;
      FWaitThread.Resume;
    end
    else
    if FWaitForTerminate then
    begin
      FState := psWaiting;

      FWaitThread := TJvWaitForProcessThread.Create(FProcessInfo.hProcess);
      FWaitThread.OnTerminate := WaitThreadTerminated;
      FWaitThread.Resume;
    end
    else
    begin
      { http://support.microsoft.com/default.aspx?scid=kb;en-us;124121 }
      WaitForInputIdle(FProcessInfo.hProcess, INFINITE);
      CloseProcessHandles;
      FState := psRunning;
    end;
  finally
    { Close pipe handles (do not continue to modify the parent).
      You need to make sure that no handles to the write end of the
      output pipe are maintained in this process or else the pipe will
      not close when the child process exits and the ReadFile will hang.
    }
    SafeCloseHandle(LConsoleHandles.Write);
    SafeCloseHandle(LConsoleHandles.Read);
    SafeCloseHandle(LConsoleHandles.Error);
    FreeMultiSz(EnvironmentData);
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

procedure TJvCreateProcess.StartConsoleEnding;
begin
  if not Assigned(FEndLock) then
    FEndLock := TCriticalSection.Create;
  FEndLock.Enter;
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
  { This is a dangerous function; because the read thread uses a blocking
    function there's no way we can stop it (normally); just signal the
    thread that is has to end;

    Note that thus it's the user responsibility to ensure that the console
    will end. If the console ends, the read thread will end also.

    An console can (always?) be ended by calling 'TJvCreateProcess.Terminate'
  }
  if FState = psWaiting then
  begin
    if Assigned(FWaitThread) then
    begin
      FWaitThread.OnTerminate := nil;
      TJvWaitForProcessThread(FWaitThread).TerminateThread;
      FWaitThread := nil;
    end;
    if Assigned(FReadThread) then
    begin
      FReadThread.OnTerminate := nil;
      TJvReadThread(FReadThread).TerminateThread;
      FReadThread := nil;
    end;
    FState := psReady;
    CloseProcessHandles;
  end;
end;

procedure TJvCreateProcess.WaitThreadTerminated(Sender: TObject);
begin
  FWaitThread := nil;

  { We only fire a TerminateEvent if both the read thread and the wait thread
    have terminated; usually the read thread will terminate before the wait
    thread: }

  FExitCode := TJvWaitForProcessThread(Sender).FExitCode;
  DoTerminateEvent;
end;

procedure TJvCreateProcess.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = CM_READ then
    try
      HandleReadEvent;
    except
      {$IFDEF COMPILER6_UP}
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(Self);
      {$ELSE}
      Application.HandleException(Self);
      {$ENDIF COMPILER6_UP}
    end
    else
      Result := DefWindowProc(Handle, Msg, WParam, LParam);
end;

function TJvCreateProcess.Write(const S: string): Boolean;
begin
  Result := (FWaitThread is TJvConsoleThread) and
    TJvConsoleThread(FWaitThread).Write(S);
end;

function TJvCreateProcess.WriteLn(const S: string): Boolean;
begin
  Result := Write(S + sLineBreak);
end;

end.
