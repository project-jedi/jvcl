{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTData.pas, released on 2000-09-22.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.
            
You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQMTData;

interface

uses
  SysUtils, Classes, Contnrs, SyncObjs,
  {$IFDEF MSWINDOWS}
  {$IFDEF DEBUGINFO_ON}
  Windows,   // for OutputDebugString
  {$ENDIF DEBUGINFO_ON}
  {$ENDIF MSWINDOWS}
  JvQMTSync, JvQMTConsts, JvQMTThreading, JvQFinalize;

type
  TMTBoundedQueue = class(TObjectQueue)
  private
    FEmpty: TMTSemaphore;
    FFull: TMTSemaphore;
    FMutex: TMTMutex;
    FName: string;
  public
    constructor Create(Size: Integer; Name: string = '');
    destructor Destroy; override;
    function Peek: TObject;
    function Pop: TObject;
    procedure Push(AObject: TObject);
  end;
  
  TMTAsyncBuffer = class(TObject)
  private
    FBuffer: TMTBoundedQueue;
    FData: TObject;
    FDataReady: TMTMutex;
    FName: string;
    FVCLReady: TMTMutex;
    FWorkerThread: TMTThread;
    procedure WorkerExecute(Thread: TMTThread);
  protected
    procedure DoDataEvent; virtual; abstract;
    procedure InitMutex; virtual; abstract;
    procedure PerformDataXChg; virtual; abstract;
  public
    constructor Create(Size: Integer; Name: string = '');
    destructor Destroy; override;
    function Read: TObject; virtual; abstract;
    procedure Write(AObject: TObject; FreeOnFail: Boolean = True); virtual; abstract;
  end;
  
  TMTBufferToVCL = class(TMTAsyncBuffer)
  private
    FOnCanRead: TNotifyEvent;
  protected
    procedure DoDataEvent; override;
    procedure InitMutex; override;
    procedure PerformDataXChg; override;
  public
    destructor Destroy; override;
    function Read: TObject; override;
    procedure Write(AObject: TObject; FreeOnFail: Boolean = True); override;
    property OnCanRead: TNotifyEvent read FOnCanRead write FOnCanRead;
  end;
  
  TMTVCLToBuffer = class(TMTAsyncBuffer)
  private
    FOnCanWrite: TNotifyEvent;
  protected
    procedure DoDataEvent; override;
    procedure InitMutex; override;
    procedure PerformDataXChg; override;
  public
    destructor Destroy; override;
    function Read: TObject; override;
    procedure Write(AObject: TObject; FreeOnFail: Boolean = True); override;
  published
    property OnCanWrite: TNotifyEvent read FOnCanWrite write FOnCanWrite;
  end;

implementation


uses
  JvQResources;


const
  sUnitName = 'JvMTData';



var
  GlobalDataThreadsMan: TMTManager = nil;

function DataThreadsMan: TMTManager;
begin
  if not Assigned(GlobalDataThreadsMan) then
  begin
    GlobalDataThreadsMan := TMTManager.Create;
    AddFinalizeObjectNil(sUnitName, TObject(GlobalDataThreadsMan));
  end;
  Result := GlobalDataThreadsMan;
end;

const
  cRead = 'Read';
  cWrite = 'Write';

//=== TMTBoundedQueue ========================================================

constructor TMTBoundedQueue.Create(Size: Integer; Name: string = '');
begin
  inherited Create;
  if Name = '' then
    FName := ClassName
  else
    FName := Name;
  
  FMutex := TMTMutex.Create;
  FEmpty := TMTSemaphore.Create(Size, Size + 1, FName + '.Space'); // do not localize
  FFull := TMTSemaphore.Create(0, Size + 1, FName + '.Data'); // do not localize
end;

destructor TMTBoundedQueue.Destroy;
begin
  while Count > 0 do
    Pop.Free;
  
  FMutex.Free;
  FEmpty.Free;
  FFull.Free;
  inherited Destroy;
end;

function TMTBoundedQueue.Peek: TObject;
begin
  FFull.Wait;
  FMutex.Enter;
  try
    Result := inherited Peek;
  finally
    FMutex.Leave;
    FFull.Signal;
  end;
end;

function TMTBoundedQueue.Pop: TObject;
begin
  FFull.Wait;
  FMutex.Enter;
  try
    Result := inherited Pop;
  finally
    FMutex.Leave;
    FEmpty.Signal;
  end;
end;

procedure TMTBoundedQueue.Push(AObject: TObject);
begin
  FEmpty.Wait;
  FMutex.Enter;
  try
    inherited Push(AObject);
  finally
    FMutex.Leave;
    FFull.Signal;
  end;
end;

//=== TMTAsyncBuffer =========================================================

constructor TMTAsyncBuffer.Create(Size: Integer; Name: string = '');
begin
  inherited Create;
  if Name = '' then
    FName := ClassName
  else
    FName := Name;
  
  FBuffer := TMTBoundedQueue.Create(Size, 'Queue'); // do not localize
  FDataReady := TMTMutex.Create('DataReady'); // do not localize
  FVCLReady  := TMTMutex.Create('VCLReady'); // do not localize

  InitMutex;

  FWorkerThread := DataThreadsMan.AcquireNewThread;
  FWorkerThread.OnExecute := WorkerExecute;
  FWorkerThread.Name := Name + '.WorkerThread'; // do not localize
  FWorkerThread.Run;
end;

destructor TMTAsyncBuffer.Destroy;
begin
  FWorkerThread.Terminate;
  FWorkerThread.Wait;
  FWorkerThread.Release;

  FBuffer.Free;
  FData.Free;
  FDataReady.Free;
  FVCLReady.Free;
  inherited Destroy;
end;

procedure TMTAsyncBuffer.WorkerExecute(Thread: TMTThread);
begin
  while True do
  begin
    // wait until the data has been read (can be outside OnCanRead event)
    FVCLReady.Wait;
    // perform blocking read or write from the buffer
    PerformDataXChg;
    // set data is ready flag
    FDataReady.Signal;
    // Perform OnCanRead event in VCL thread context
    Thread.Synchronize(DoDataEvent);
  end;
end;


//=== TMTBufferToVCL =========================================================

destructor TMTBufferToVCL.Destroy;
begin
  FOnCanRead := nil;
  inherited Destroy;
end;

procedure TMTBufferToVCL.DoDataEvent;
begin
  if Assigned(FOnCanRead) then
    FOnCanRead(Self);
end;

procedure TMTBufferToVCL.InitMutex;
begin
  FDataReady.Wait;
end;

procedure TMTBufferToVCL.PerformDataXChg;
begin
  // perform blocking read from the buffer
  FData := FBuffer.Pop;
end;

function TMTBufferToVCL.Read: TObject;
begin
  if CurrentMTThread <> nil then
    raise EThread.CreateResFmt(@RsEMethodOnlyForMainThread, [cRead]);

  // Check if data ready
  FDataReady.Wait;
  // get data
  Result := FData;
  // make sure it we dont own it anymore
  FData := nil;
  // signal worker to continue
  FVCLReady.Signal;
end;

procedure TMTBufferToVCL.Write(AObject: TObject; FreeOnFail: Boolean = True);
begin
  try
    if CurrentMTThread = nil then
      raise EThread.CreateResFmt(@RsEMethodOnlyForMainThread, [cWrite]);

    // Perform blocking write to buffer
    FBuffer.Push(AObject);
  except
    if FreeOnFail then
      AObject.Free;
    raise;
  end;
end;

//=== TMTVCLToBuffer =========================================================

destructor TMTVCLToBuffer.Destroy;
begin
  FOnCanWrite := nil;
  inherited Destroy;
end;

procedure TMTVCLToBuffer.DoDataEvent;
begin
  if Assigned(FOnCanWrite) then
    FOnCanWrite(Self);
end;

procedure TMTVCLToBuffer.InitMutex;
begin
  FVCLReady.Wait;
  //FDataReady.Wait;
end;

procedure TMTVCLToBuffer.PerformDataXChg;
begin
  FBuffer.Push(FData);
  FData := nil;
end;

function TMTVCLToBuffer.Read: TObject;
begin
  if CurrentMTThread = nil then
    raise EThread.CreateResFmt(@RsEMethodOnlyForMainThread, [cRead]);

  Result := FBuffer.Pop;
end;

procedure TMTVCLToBuffer.Write(AObject: TObject; FreeOnFail: Boolean = True);
begin
  try
    if CurrentMTThread <> nil then
      raise EThread.CreateResFmt(@RsEMethodOnlyForMainThread, [cWrite]);

    // Check if data ready
    FDataReady.Wait;
  except
    if FreeOnFail then
      AObject.Free;
    raise;
  end;
  
  // save data object
  FData := AObject;
  
  // signal worker to continue
  FVCLReady.Signal;
end;

initialization

finalization
  {$IFDEF MSWINDOWS}
  // (rom) no OutputDebugString in production code
  {$IFDEF DEBUGINFO_ON}
  if DataThreadsMan.ActiveThreads then
    OutputDebugString(
      'Memory leak detected: free MTData objects before application shutdown'); // do not localize
  {$ENDIF DEBUGINFO_ON}
  {$ENDIF MSWINDOWS}

  FinalizeUnit(sUnitName);

end.
