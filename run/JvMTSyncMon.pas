{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: MTSyncMon.pas, released on 2000-09-22.

The Initial Developer of the Original Code is Erwin Molendijk.
Portions created by Erwin Molendijk are Copyright (C) 2002 Erwin Molendijk.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvMTSyncMon;

{$I jvcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes, Contnrs, SyncObjs,
  JvMTThreading, JvMTSync, JvMTConsts;

type
  TMTCondition = class;

  TMTMonitor = class(TObject)
  private
    FActiveThread: TMTThread;
    FConditions: TObjectList;
    FCriticalTransition: TCriticalSection;
    FMutex: TMTSemaphore;
    FNext: TMTSemaphore;
    FNextCount: Integer;
    function GetCondition(ID: Integer): TMTCondition;
  protected
    procedure CriticalEnter;
    procedure CriticalLeave;
    procedure DecNextCount;
    function GetNextCount: Integer;
    procedure IncNextCount;
    procedure InvalidateActiveThread;
    function IsValidActiveThread: Boolean;
    procedure SignalMutex;
    procedure SignalNext;
    procedure WaitMutex;
    procedure WaitNext;
    property ActiveThread: TMTThread read FActiveThread write FActiveThread;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    property Condition[ID: Integer]: TMTCondition read GetCondition; default;
  end;

  TMTCondition = class(TObject)
  private
    FID: Integer;
    FMonitor: TMTMonitor;
    FXCount: Integer;
    FXSem: TMTSemaphore;
  public
    constructor Create(AMonitor: TMTMonitor; AID: Integer);
    destructor Destroy; override;
    procedure Signal;
    procedure Wait;
    property ID: Integer read FID;
    property Monitor: TMTMonitor read FMonitor;
  end;

implementation

// Invalid is -1 pointer. The nil pointer is used for the main VCL thread
const
  InvalidThreadPtr = TMTThread(-1);

{$IFDEF LINUX}

function InterlockedIncrement(var I: Integer): Integer;
asm
        MOV       EDX, 1
        XCHG      EAX, EDX
        LOCK XADD [EDX], EAX
        INC       EAX
end;

function InterlockedDecrement(var I: Integer): Integer;
asm
        MOV       EDX, -1
        XCHG      EAX, EDX
        LOCK XADD [EDX], EAX
        DEC       EAX
end;

{$ENDIF LINUX}

//=== { TMTMonitor } =========================================================

constructor TMTMonitor.Create;
begin
  inherited Create;
  FConditions := TObjectList.Create;
  FMutex := TMTSemaphore.Create(1, 1);
  FNext := TMTSemaphore.Create(0, 1);
  FCriticalTransition := TCriticalSection.Create;
  InvalidateActiveThread;
end;

destructor TMTMonitor.Destroy;
begin
  FCriticalTransition.Free;
  FConditions.Free;
  FMutex.Free;
  FNext.Free;
  inherited Destroy;
end;

procedure TMTMonitor.CriticalEnter;
begin
  FCriticalTransition.Enter;
end;

procedure TMTMonitor.CriticalLeave;
begin
  FCriticalTransition.Leave;
end;

procedure TMTMonitor.DecNextCount;
begin
  InterlockedDecrement(FNextCount);
end;

procedure TMTMonitor.Enter;
begin
  WaitMutex;
  Assert(not IsValidActiveThread);
  FActiveThread := CurrentMTThread;
end;

function TMTMonitor.GetCondition(ID: Integer): TMTCondition;
var
  I: Integer;
begin
  // search for condition. start at top
  I := FConditions.Count-1;
  while (I <> -1) and (TMTCondition(FConditions[I]).ID <> ID) do
    Dec(I);
  
  // if not found, add the condition
  if I = -1 then
    I := FConditions.Add(TMTCondition.Create(Self, ID));
  
  // return the condition
  Result := TMTCondition(FConditions[I])
end;

function TMTMonitor.GetNextCount: Integer;
begin
  Result := FNextCount;
end;

procedure TMTMonitor.IncNextCount;
begin
  InterlockedIncrement(FNextCount);
end;

procedure TMTMonitor.InvalidateActiveThread;
begin
  FActiveThread := InvalidThreadPtr;
end;

function TMTMonitor.IsValidActiveThread: Boolean;
begin
  Result := FActiveThread <> InvalidThreadPtr;
end;

procedure TMTMonitor.Leave;
begin
  CriticalEnter;
  try
    if (CurrentMTThread = FActiveThread) or (not IsValidActiveThread) then
    begin
      InvalidateActiveThread;
      if GetNextCount > 0 then
        SignalNext
      else
        SignalMutex;
    end;
  finally
    CriticalLeave;
  end;
end;

procedure TMTMonitor.SignalMutex;
begin
  FMutex.Signal;
end;

procedure TMTMonitor.SignalNext;
begin
  FNext.Signal;
end;

procedure TMTMonitor.WaitMutex;
begin
  FMutex.Wait;
end;

procedure TMTMonitor.WaitNext;
begin
  FNext.Wait;
end;

//=== { TMTCondition } =======================================================

constructor TMTCondition.Create(AMonitor: TMTMonitor; AID: Integer);
begin
  inherited Create;
  FID := AID;
  FMonitor := AMonitor;
  FXSem := TMTSemaphore.Create(0, 1);
end;

destructor TMTCondition.Destroy;
begin
  FXSem.Free;
  inherited Destroy;
end;

procedure TMTCondition.Signal;
var
  OtherWaiting: Boolean;
begin
  FMonitor.CriticalEnter;
  try
    //FMonitor.FActiveThread := nil;
    FMonitor.InvalidateActiveThread;
    FMonitor.IncNextCount;
    OtherWaiting := FXCount > 0;
    if OtherWaiting then
      FXSem.Signal;
  finally
    FMonitor.CriticalLeave;
  end;

  if OtherWaiting then
    try
      FMonitor.WaitNext;   // Can raise EMTTerminateError
    except
      on EMTTerminateError do
      begin
        FMonitor.CriticalEnter;
        try
          FMonitor.DecNextCount;
        finally
          FMonitor.CriticalLeave;
        end;
        raise;
      end;
    end;
  
  FMonitor.CriticalEnter;
  try
    FMonitor.DecNextCount;
    FMonitor.ActiveThread := CurrentMTThread;
  finally
    FMonitor.CriticalLeave;
  end;
end;

procedure TMTCondition.Wait;
begin
  FMonitor.CriticalEnter;
  try
    //FMonitor.FActiveThread := nil;
    FMonitor.InvalidateActiveThread;
    InterlockedIncrement(FXCount);

    if FMonitor.GetNextCount > 0 then
      FMonitor.SignalNext
    else
      FMonitor.SignalMutex;
  finally
    FMonitor.CriticalLeave;
  end;

  try
    FXSem.Wait;  // Can raise EMTTerminateError
  except
    on EMTTerminateError do
    begin
      FMonitor.CriticalEnter;
      try
        InterlockedDecrement(FXCount);
      finally
        FMonitor.CriticalLeave;
      end;
      raise;
    end;
  end;

  FMonitor.CriticalEnter;
  try
    InterlockedDecrement(FXCount);
    FMonitor.ActiveThread := CurrentMTThread;
  finally
    FMonitor.CriticalLeave;
  end;
end;

end.
