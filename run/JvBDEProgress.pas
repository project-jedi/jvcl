{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDbPrgrss.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDEProgress;

interface

uses
  Windows, Classes, Controls, DB, DBTables, Bde,
  JvTimer, JvComponent;

type
  TOnMessageChange = procedure(Sender: TObject; const Msg: string) of object;
  TOnPercentChange = procedure(Sender: TObject; PercentDone: Integer) of object;
  TOnProgressEvent = procedure(Sender: TObject; var AbortQuery: Boolean) of object;
  TOnTraceEvent = procedure(Sender: TObject; Flag: TTraceFlag; const Msg: string) of object;

  TJvDBProgress = class(TJvComponent)
  private
    FActive: Boolean;
    FStartTime: Longint;
    FTimer: TJvTimer;
    FWaitCursor: TCursor;
    FGauge: TControl;
    FMessageControl: TControl;
    FStreamedValue: Boolean;
    FGenProgressCallback: TObject;
    FQryProgressCallback: TObject;
    FOnMessageChange: TOnMessageChange;
    FOnPercentChange: TOnPercentChange;
    FOnProgress: TOnProgressEvent;
    FTraceFlags: TTraceFlags;
    FTraceCallback: TObject;
    FTrace: Boolean;
    FOnTrace: TOnTraceEvent;
    FSessionName: string;
    FSessionLink: TObject;
    procedure SetTrace(Value: Boolean);
    procedure SetTraceFlags(Value: TTraceFlags);
    function TraceCallBack(CBInfo: Pointer): CBRType;
    function GetDBSession: TSession;
    procedure SetSessionName(const Value: string);
    procedure Activate;
    procedure Deactivate;
    procedure FreeTimer;
    procedure StartTimer;
    procedure TimerExpired(Sender: TObject);
    function GenProgressCallback(CBInfo: Pointer): CBRType;
    function QryProgressCallback(CBInfo: Pointer): CBRType;
    procedure SetActive(Value: Boolean);
    procedure SetPercent(Value: Integer);
    procedure SetMessage(const Value: string);
    procedure SetMessageControl(Value: TControl);
    procedure SetGauge(Value: TControl);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ProgressMsgValue(const Msg: string): Longint;
  published
    property Active: Boolean read FActive write SetActive default True;
    property WaitCursor: TCursor read FWaitCursor write FWaitCursor default crHourGlass;
    property MessageControl: TControl read FMessageControl write SetMessageControl;
    property Gauge: TControl read FGauge write SetGauge;
    property SessionName: string read FSessionName write SetSessionName;
    property Trace: Boolean read FTrace write SetTrace default False;
    property TraceFlags: TTraceFlags read FTraceFlags write SetTraceFlags default [];
    property OnTrace: TOnTraceEvent read FOnTrace write FOnTrace;
    property OnMessageChange: TOnMessageChange read FOnMessageChange write FOnMessageChange;
    property OnPercentChange: TOnPercentChange read FOnPercentChange write FOnPercentChange;
    property OnProgress: TOnProgressEvent read FOnProgress write FOnProgress;
  end;

  TJvDBCallbackEvent = function(CBInfo: Pointer): CBRType of object;
  TJvDBCallbackChain = (dcOnlyOnce, dcChain, dcReplace);

  TJvDBCallback = class(TObject)
  private
    FOwner: TObject;
    FCBType: CBType;
    FCBBuf: Pointer;
    FCBBufLen: Cardinal;
    FOldCBData: Longint;
    FOldCBBuf: Pointer;
    FOldCBBufLen: Word;
    FOldCBFunc: Pointer;
    FInstalled: Boolean;
    FChain: TJvDBCallbackChain;
    FCallbackEvent: TJvDBCallbackEvent;
  protected
    function Invoke(CallType: CBType; var CBInfo: Pointer): CBRType;
  public
    constructor Create(AOwner: TObject; CBType: CBType;
      CBBufSize: Cardinal; CallbackEvent: TJvDBCallbackEvent;
      Chain: TJvDBCallbackChain);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Math, Forms, StdCtrls, 
  JvProgressUtils;

const
  cbQRYPROGRESS = cbRESERVED4;

function BdeCallBack(CallType: CBType; Data: Longint; CBInfo: Pointer): CBRType; stdcall;
begin
  if Data <> 0 then
    Result := TJvDBCallback(Data).Invoke(CallType, CBInfo)
  else
    Result := cbrUSEDEF;
end;

//=== { TJvDBCallback } ======================================================

constructor TJvDBCallback.Create(AOwner: TObject; CBType: CBType;
  CBBufSize: Cardinal; CallbackEvent: TJvDBCallbackEvent;
  Chain: TJvDBCallbackChain);
begin
  inherited Create;
  FOwner := AOwner;
  FCBType := CBType;
  FCallbackEvent := CallbackEvent;
  DbiGetCallBack(nil, FCBType, @FOldCBData, @FOldCBBufLen, @FOldCBBuf,
    pfDBICallBack(FOldCBFunc));
  FChain := Chain;
  if not Assigned(FOldCBFunc) then
    FOldCBBufLen := 0;
  if not Assigned(FOldCBFunc) or (FChain in [dcChain, dcReplace]) then
  begin
    FCBBufLen := Max(CBBufSize, FOldCBBufLen);
    FCBBuf := AllocMem(FCBBufLen);
    Check(DbiRegisterCallback(nil, FCBType, Longint(Self), FCBBufLen,
      FCBBuf, BdeCallBack));
    FInstalled := True;
  end;
end;

destructor TJvDBCallback.Destroy;
begin
  if FInstalled then
    if Assigned(FOldCBFunc) and (FChain = dcChain) then
      try
        DbiRegisterCallback(nil, FCBType, FOldCBData, FOldCBBufLen,
          FOldCBBuf, pfDBICallback(FOldCBFunc));
      except
      end
    else
      DbiRegisterCallback(nil, FCBType, 0, 0, nil, nil);
  if FCBBuf <> nil then
    FreeMem(FCBBuf, FCBBufLen);
  inherited Destroy;
end;

function TJvDBCallback.Invoke(CallType: CBType; var CBInfo: Pointer): CBRType;
begin
  Result := cbrUSEDEF;
  if CallType = FCBType then
    try
      Result := FCallbackEvent(CBInfo);
    except
      Application.HandleException(Self);
    end;
  if Assigned(FOldCBFunc) and (FChain = dcChain) then
    Result := pfDBICallBack(FOldCBFunc)(CallType, FOldCBData, CBInfo);
end;

// (rom) changed to var
var
  ProgressList: TList = nil;

procedure SetWaitCursor;
begin
  if GetCurrentThreadID = MainThreadID then
    Screen.Cursor :=
      TJvDBProgress(ProgressList.Items[ProgressList.Count - 1]).WaitCursor;
end;

procedure AddProgress(Progress: TJvDBProgress);
begin
  if ProgressList = nil then
    ProgressList := TList.Create;
  if ProgressList.IndexOf(Progress) = -1 then
    ProgressList.Add(Progress);
end;

procedure RemoveProgress(Progress: TJvDBProgress);
begin
  if ProgressList <> nil then
  begin
    ProgressList.Remove(Progress);
    if ProgressList.Count = 0 then
    begin
      ProgressList.Free;
      ProgressList := nil;
      Screen.Cursor := crDefault;
    end;
  end;
end;

//=== { TJvSessionLink } =====================================================

type
  TJvSessionLink = class(TDatabase)
  private
    FProgress: TJvDBProgress;
  public
    destructor Destroy; override;
  end;

destructor TJvSessionLink.Destroy;
begin
  if FProgress <> nil then
  begin
    FProgress.FSessionLink := nil;
    FProgress.Trace := False;
    FProgress.Active := False;
  end;
  inherited Destroy;
end;

//=== { TJvDBProgress } ======================================================

constructor TJvDBProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWaitCursor := crHourGlass;
  FActive := True;
end;

destructor TJvDBProgress.Destroy;
begin
  FOnTrace := nil;
  Trace := False;
  Active := False;
  FreeTimer;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvDBProgress.Loaded;
begin
  inherited Loaded;
  FStreamedValue := True;
  try
    SetActive(FActive);
    SetTrace(FTrace);
  finally
    FStreamedValue := False;
  end;
end;

procedure TJvDBProgress.TimerExpired(Sender: TObject);
begin
  FreeTimer;
  SetPercent(0);
  SetMessage('');
end;

procedure TJvDBProgress.FreeTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FStartTime := 0;
  end;
  Screen.Cursor := crDefault;
  SetCursor(Screen.Cursors[crDefault]); { force update cursor }
end;

procedure TJvDBProgress.StartTimer;
begin
  if FTimer = nil then
  begin
    FTimer := TJvTimer.Create(Self);
    FTimer.Interval := 500;
  end;
  with FTimer do
  begin
    if not Enabled then
      FStartTime := GetTickCount;
    OnTimer := TimerExpired;
    Enabled := True;
  end;
end;

procedure TJvDBProgress.SetPercent(Value: Integer);
begin
  if Gauge <> nil then
  begin
    SetProgressMax(Gauge, 100);
    SetProgressValue(Gauge, Value);
  end;
  if Assigned(FOnPercentChange) then
    FOnPercentChange(Self, Value);
end;

procedure TJvDBProgress.SetMessage(const Value: string);
begin
  if MessageControl <> nil then
  begin
    TLabel(MessageControl).Caption := Value;
    MessageControl.Refresh;
  end;
  if Assigned(FOnMessageChange) then
    FOnMessageChange(Self, Value);
end;

procedure TJvDBProgress.SetActive(Value: Boolean);
begin
  if (FActive <> Value) or FStreamedValue then
  begin
    if not (csDesigning in ComponentState) then
    begin
      if Value then
        AddProgress(Self)
      else
        RemoveProgress(Self);
      if (FGenProgressCallback = nil) and Value then
      begin
        Activate;
        FGenProgressCallback := TJvDBCallback.Create(Self, cbGENPROGRESS,
          Max(SizeOf(CBPROGRESSDesc), SizeOf(DBIPATH) + SizeOf(Integer) * 4),
          GenProgressCallback, dcChain);
        FQryProgressCallback := TJvDBCallback.Create(Self, cbQRYPROGRESS,
          SizeOf(DBIQryProgress), QryProgressCallback, dcChain);
      end
      else
      if not Value and (FGenProgressCallback <> nil) then
      begin
        Sessions.CurrentSession := GetDBSession;
        FGenProgressCallback.Free;
        FGenProgressCallback := nil;
        FQryProgressCallback.Free;
        FQryProgressCallback := nil;
        FreeTimer;
        if not Trace then
          Deactivate;
      end;
    end;
    FActive := Value;
  end;
end;

procedure TJvDBProgress.Activate;
var
  S: TSession;
begin
  if FSessionLink = nil then
  begin
    S := Sessions.List[SessionName];
    S.Open;
    Sessions.CurrentSession := S;
    FSessionLink := TJvSessionLink.Create(S);
    try
      TJvSessionLink(FSessionLink).Temporary := True;
      TJvSessionLink(FSessionLink).KeepConnection := False;
      TJvSessionLink(FSessionLink).FProgress := Self;
    except
      FSessionLink.Free;
      FSessionLink := nil;
      raise;
    end;
  end
  else
    Sessions.CurrentSession := TDatabase(FSessionLink).Session;
end;

procedure TJvDBProgress.Deactivate;
begin
  if FSessionLink <> nil then
  begin
    TJvSessionLink(FSessionLink).FProgress := nil;
    FSessionLink.Free;
    FSessionLink := nil;
  end;
end;

function TJvDBProgress.GetDBSession: TSession;
begin
  Result := Sessions.FindSession(SessionName);
  if Result = nil then
    Result := DBTables.Session;
end;

procedure TJvDBProgress.SetSessionName(const Value: string);
var
  KeepActive, KeepTrace: Boolean;
begin
  if Value <> SessionName then
    if not (csDesigning in ComponentState) then
    begin
      KeepActive := Active;
      KeepTrace := Trace;
      Active := False;
      Trace := False;
      FSessionName := Value;
      Active := KeepActive;
      Trace := KeepTrace;
    end
    else
      FSessionName := Value;
end;

procedure TJvDBProgress.SetTrace(Value: Boolean);
begin
  if (FTrace <> Value) or (FStreamedValue and Value) then
    if not (csDesigning in ComponentState) then
    begin
      if Value then
      begin
        Activate;
        GetDBSession.TraceFlags := FTraceFlags;
        FTraceCallback := TJvDBCallback.Create(Self, cbTRACE,
          smTraceBufSize, TraceCallBack, dcReplace);
      end
      else
      if FTraceCallback <> nil then
      begin
        Sessions.CurrentSession := GetDBSession;
        FTraceCallback.Free;
        FTraceCallback := nil;
        if not Active then
          Deactivate;
      end;
      FTrace := (FTraceCallback <> nil);
    end
    else
      FTrace := Value;
end;

procedure TJvDBProgress.SetTraceFlags(Value: TTraceFlags);
begin
  FTraceFlags := Value;
  if Trace then
    GetDBSession.TraceFlags := FTraceFlags;
end;

function TJvDBProgress.TraceCallBack(CBInfo: Pointer): CBRType;
var
  CurFlag: TTraceFlag;
begin
  Result := cbrUSEDEF;
  if Trace and Assigned(FOnTrace) then
  begin
    case PTraceDesc(CBInfo)^.eTraceCat of
      traceQPREPARE:
        CurFlag := tfQPrepare;
      traceQEXECUTE:
        CurFlag := tfQExecute;
      traceERROR:
        CurFlag := tfError;
      traceSTMT:
        CurFlag := tfStmt;
      traceCONNECT:
        CurFlag := tfConnect;
      traceTRANSACT:
        CurFlag := tfTransact;
      traceBLOB:
        CurFlag := tfBlob;
      traceMISC:
        CurFlag := tfMisc;
      traceVENDOR:
        CurFlag := tfVendor;
      traceDATAIN:
        CurFlag := tfDataIn;
      traceDATAOUT:
        CurFlag := tfDataOut;
    else
      Exit;
    end;
    if CurFlag in TraceFlags then
      FOnTrace(Self, CurFlag, StrPas(PTraceDesc(CBInfo)^.pszTrace));
  end;
end;

procedure TJvDBProgress.SetMessageControl(Value: TControl);
begin
  FMessageControl := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvDBProgress.SetGauge(Value: TControl);
begin
  FGauge := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TJvDBProgress.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if AOperation = opRemove then
    if AComponent = Gauge then
      Gauge := nil
    else
    if AComponent = MessageControl then
      MessageControl := nil;
end;

function TJvDBProgress.GenProgressCallback(CBInfo: Pointer): CBRType;
var
  CallInfo: pCBPROGRESSDesc;
  AbortOp: Boolean;
begin
  CallInfo := CBInfo;
  Result := cbrUSEDEF;
  StartTimer;
  if (FTimer <> nil) and FTimer.Enabled {and (GetTickCount > FStartTime)} then
    SetWaitCursor;
  if Assigned(FOnProgress) then
  begin
    AbortOp := False;
    FOnProgress(Self, AbortOp);
    if AbortOp then
      Result := cbrABORT;
  end;
  if CallInfo^.iPercentDone >= 0 then
    SetPercent(CallInfo^.iPercentDone)
  else
    SetMessage(StrPas(CallInfo^.szMsg));
end;

function TJvDBProgress.QryProgressCallback(CBInfo: Pointer): CBRType;
var
  CallInfo: pDBIQryProgress;
  AbortOp: Boolean;
  PcntDone: Double;
begin
  CallInfo := CBInfo;
  Result := cbrUSEDEF;
  StartTimer;
  {if (FTimer <> nil) and FTimer.Enabled then SetWaitCursor;}
  if Assigned(FOnProgress) then
  begin
    AbortOp := False;
    FOnProgress(Self, AbortOp);
    if AbortOp then
      Result := cbrABORT;
  end;
  with CallInfo^ do
    PcntDone := (stepsCompleted / Max(1, stepsInQry)) *
      (elemCompleted / Max(1, totElemInStep));
  SetPercent(Round(PcntDone * 100));
end;

function TJvDBProgress.ProgressMsgValue(const Msg: string): Longint;
begin
  if Msg <> '' then
    Result := StrToIntDef(Trim(Copy(Msg, Pos(':', Msg) + 1, MaxInt)), -1)
  else
    Result := -1;
end;

end.

