{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCommStatus.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCommStatus;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvTypes, JvComponent;

type
  TJvCommPort = 0..8;

  TJvCommWatcher = class(TThread)
  private
  protected
    procedure Execute; override;
    procedure Changed;
  public
    // (rom) ugly needs TODO
    FHandle: THandle;
    FStat: Cardinal;
    FOnChange: TNotifyEvent;
  end;

  TJvCommStatus = class(TJvComponent)
  private
    FClear: Boolean;
    FDataSet: Boolean;
    FRing: Boolean;
    FReceive: Boolean;
    FHandle: THandle;
    FWatcher: TJvCommWatcher;
    FBidon: Boolean;
    FComm: TJvCommPort;
    FOnChanged: TNotifyEvent;
    procedure SetComm(const Value: TJvCommPort);
    procedure OnChange(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ClearToSend: Boolean read FClear write FBidon;
    property DataSetReady: Boolean read FDataSet write FBidon;
    property Ring: Boolean read FRing write FBidon;
    property ReceiveLine: Boolean read FReceive write FBidon;
    property Comm: TJvCommPort read FComm write SetComm default 0;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{**************************************************}

constructor TJvCommStatus.Create(AOwner: TComponent);
begin
  inherited;
  FComm := 0;
  FHandle := 0;

  if not (csDesigning in ComponentState) then
  begin
    FWatcher := TJvCommWatcher.Create(True);
    FWatcher.FreeOnTerminate := True;

    FWatcher.FHandle := FHandle;
    FWatcher.FStat := 0;
    FWatcher.FonChange := OnChange;

    FWatcher.Resume;
  end
  else
    FWatcher := nil;

  SetComm(FComm);
end;

{**************************************************}

destructor TJvCommStatus.Destroy;
begin
  if FWatcher <> nil then
  begin
    FWatcher.Terminate;
    FWatcher := nil;
  end;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  inherited;
end;

{**************************************************}

procedure TJvCommStatus.OnChange(Sender: TObject);
var
  stat: Cardinal;
begin
  if (FWatcher <> nil) and (FHandle <> 0) then
    stat := FWatcher.FStat
  else
    stat := 0;

  FClear := (stat and MS_CTS_ON) <> 0;
  FDataSet := (stat and MS_DSR_ON) <> 0;
  FRing := (stat and MS_RING_ON) <> 0;
  FReceive := (stat and MS_RLSD_ON) <> 0;

  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{**************************************************}

procedure TJvCommStatus.SetComm(const Value: TJvCommPort);
var
  stat: Cardinal;
  CommName: string;
begin
  if FWatcher <> nil then
    FWatcher.FHandle := 0;
  FComm := Value;
  if FHandle <> 0 then
    CloseHandle(FHandle);
  FHandle := 0;
  // (rom) simplified through better TJvCommPort
  if FComm <> 0 then
  begin
    CommName := 'COM' + IntToStr(FComm);
    FHandle := CreateFile(PChar(CommName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  end;

  FClear := False;
  FDataSet := False;
  FRing := False;
  FReceive := False;

  if GetCommModemStatus(FHandle, stat) then
  begin
    FClear := (stat and MS_CTS_ON) <> 0;
    FDataSet := (stat and MS_DSR_ON) <> 0;
    FRing := (stat and MS_RING_ON) <> 0;
    FReceive := (stat and MS_RLSD_ON) <> 0;
  end;

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

///////////////////////////////////////////////////////////
// TJvCommWatcher
///////////////////////////////////////////////////////////

procedure TJvCommWatcher.Changed;
begin
  FOnChange(nil);
end;

{**************************************************}

procedure TJvCommWatcher.Execute;
var
  mask: Cardinal;
begin
  while not Terminated do
  begin
    if FHandle <> 0 then
    begin
      GetCommModemStatus(FHandle, mask);
      if mask <> FStat then
      begin
        FStat := mask;
        Synchronize(Changed);
      end;
    end;
    Sleep(50);
  end;
end;

end.
