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

The Original Code is: JvCommStatus.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvQCommStatus;

interface

uses
  Windows, SysUtils, Classes, 
  JvQTypes, JvQComponent;

type
  TJvCommPort = 0..8;

  TJvCommWatcher = class(TThread)
  private
    FHandle: THandle;
    FStat: Cardinal;
    FOnChange: TNotifyEvent;
    procedure Changed;
  protected
    procedure Execute; override;
  end;

  TJvCommStatus = class(TJvComponent)
  private
    FClearToSend: Boolean;
    FDataSetReady: Boolean;
    FRing: Boolean;
    FReceiveLine: Boolean;
    FHandle: THandle;
    FWatcher: TJvCommWatcher;
    FDummy: Boolean;
    FComm: TJvCommPort;
    FOnChanged: TNotifyEvent;
    procedure SetComm(const Value: TJvCommPort);
    procedure OnChange(Sender: TObject);
    procedure UpdateStates(State: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Do not store dummies }
    property ClearToSend: Boolean read FClearToSend write FDummy stored False;
    property DataSetReady: Boolean read FDataSetReady write FDummy stored False;
    property Ring: Boolean read FRing write FDummy stored False;
    property ReceiveLine: Boolean read FReceiveLine write FDummy stored False;
    property Comm: TJvCommPort read FComm write SetComm default 0;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

//=== TJvCommStatus ==========================================================

constructor TJvCommStatus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComm := 0;
  FHandle := 0;

  if not (csDesigning in ComponentState) then
  begin
    FWatcher := TJvCommWatcher.Create(True);
    FWatcher.FreeOnTerminate := True;

    FWatcher.FHandle := FHandle;
    FWatcher.FStat := 0;
    FWatcher.FOnChange := OnChange;

    FWatcher.Resume;
  end
  else
    FWatcher := nil;

  SetComm(FComm);
end;

destructor TJvCommStatus.Destroy;
begin
  if FWatcher <> nil then
  begin
    FWatcher.Terminate;
    FWatcher := nil;
  end;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  inherited Destroy;
end;

procedure TJvCommStatus.UpdateStates(State: Cardinal);
begin
  FClearToSend := (State and MS_CTS_ON) <> 0;
  FDataSetReady := (State and MS_DSR_ON) <> 0;
  FRing := (State and MS_RING_ON) <> 0;
  FReceiveLine := (State and MS_RLSD_ON) <> 0;
end;

procedure TJvCommStatus.OnChange(Sender: TObject);
begin
  if (FWatcher <> nil) and (FHandle <> 0) then
    UpdateStates(FWatcher.FStat)
  else
    UpdateStates(0);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvCommStatus.SetComm(const Value: TJvCommPort);
var
  Stat: Cardinal;
  CommName: string;
begin
  if FWatcher <> nil then
    FWatcher.FHandle := 0;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := 0;
  FComm := Value;
  // (rom) simplified through better TJvCommPort
  if FComm <> 0 then
  begin
    CommName := 'COM' + IntToStr(FComm);
    FHandle := CreateFile(PChar(CommName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  end;

  if GetCommModemStatus(FHandle, Stat) then
    UpdateStates(Stat)
  else
    UpdateStates(0);

  if FWatcher <> nil then
  begin
    FWatcher.FHandle := FHandle;
    FWatcher.FStat := 0;
    if FHandle <> 0 then
      FWatcher.Resume
    else
      FWatcher.Suspend;
  end;
  OnChange(Self);
end;

//=== TJvCommWatcher =========================================================

procedure TJvCommWatcher.Changed;
begin
  FOnChange(nil);
end;

procedure TJvCommWatcher.Execute;
var
  Mask: Cardinal;
begin
  // (rom) secure thread against exceptions
  try
    while not Terminated do
    begin
      if FHandle <> 0 then
      begin
        GetCommModemStatus(FHandle, Mask);
        if Mask <> FStat then
        begin
          FStat := Mask;
          Synchronize(Changed);
        end;
      end;
      Sleep(50);
    end;
  except
  end;
end;

end.

