{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWaitingProgress.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvWaitingProgress;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JvSpecialProgress, JVCLVer;

type
  TJvProgressThread = class(TThread)
  protected
    procedure Draw;
    procedure Execute; override;
  public
    FDelay: Cardinal;
    FOnDraw: TNotifyEvent;
  end;

  TJvWaitingProgress = class(TWinControl)
  private
    FActive: Boolean;
    FRefresh: Cardinal;
    FLength: Cardinal;
    FOnEnded: TNotifyEvent;
    FWait: TJvProgressThread;
    FProgress: TJvSpecialProgress;
    FAboutJVCL: TJVCLAboutInfo;

    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetActive(const Value: Boolean);
    procedure SetLength(const Value: Cardinal);
    procedure SetRefresh(const Value: Cardinal);
    procedure OnScroll(Sender: TObject);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    function GetBColor: TColor;
    procedure SetBColor(const Value: TColor);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Length: Cardinal read FLength write SetLength default 30000;
    property Active: Boolean read FActive write SetActive default False;
    property RefreshInterval: Cardinal read FRefresh write SetRefresh default 500;
    property OnEnded: TNotifyEvent read FOnEnded write FOnEnded;
    property ProgressColor: TColor read GetColor write SetColor;
    property Color: TColor read GetBColor write SetBColor;
  end;

implementation

///////////////////////////////////////////////////////////
// TJvWaitingProgress
///////////////////////////////////////////////////////////

constructor TJvWaitingProgress.Create(AOwner: TComponent);
begin
  inherited;
  FActive := False;
  FRefresh := 500;
  FLength := 30000;

  FWait := TJvProgressThread.Create(True);
  FWait.FreeOnTerminate := False;
  FWait.FDelay := FRefresh;
  FWait.FOnDraw := OnScroll;

  FProgress := TJvSpecialProgress.Create(Self);
  FProgress.Parent := Self;
  FProgress.Maximum := FLength;
  FProgress.Position := 0;
  FProgress.StartColor := clBlack;
  FProgress.EndColor := clBlack;
  FProgress.Solid := True;

  Width := 100;
  Height := 10;

  FProgress.Left := 0;
  FProgress.Top := 0;
  FProgress.Width := Width;
  FProgress.Height := Height;

  inherited Color := FProgress.Color;
end;

{**************************************************}

destructor TJvWaitingProgress.Destroy;
begin
  FWait.Terminate;
  while not FWait.Terminated do
    Application.ProcessMessages;
  FWait.Free;

  FProgress.Free;
  inherited;
end;

{**************************************************}

function TJvWaitingProgress.GetBColor: TColor;
begin
  Result := FProgress.Color;
end;

{**************************************************}

function TJvWaitingProgress.GetColor: TColor;
begin
  Result := FProgress.StartColor;
end;

{**************************************************}

procedure TJvWaitingProgress.OnScroll(Sender: TObject);
begin
  //Step
  if Integer(FProgress.Position) + Integer(FRefresh) > Integer(FLength) then
  begin
    FProgress.Position := FLength;
    SetActive(False);
    if Assigned(FOnEnded) then
      FOnEnded(Self);
  end
  else
    FProgress.Position := FProgress.Position + Integer(FRefresh);
  Application.ProcessMessages;
end;

{**************************************************}

procedure TJvWaitingProgress.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      FProgress.Position := 0;
      FWait.Resume;
    end
    else
      FWait.Suspend;
  end;
end;

{**************************************************}

procedure TJvWaitingProgress.SetBColor(const Value: TColor);
begin
  if FProgress.Color <> Value then
  begin
    FProgress.Color := Value;
    inherited Color := Value;
  end;
end;

{**************************************************}

procedure TJvWaitingProgress.SetColor(const Value: TColor);
begin
  FProgress.StartColor := Value;
  FProgress.EndColor := Value;
end;

{**************************************************}

procedure TJvWaitingProgress.SetLength(const Value: Cardinal);
begin
  FLength := Value;
  FProgress.Position := 0;
  FProgress.Maximum := FLength;
end;

{**************************************************}

procedure TJvWaitingProgress.SetRefresh(const Value: Cardinal);
begin
  FRefresh := Value;
  FWait.FDelay := FRefresh;
end;

{**************************************************}

procedure TJvWaitingProgress.WMSize(var Msg: TWMSize);
begin
  FProgress.Width := Self.Width;
  FProgress.Height := Self.Height;
end;

///////////////////////////////////////////////////////////
// TJvProgressThread
///////////////////////////////////////////////////////////

procedure TJvProgressThread.Draw;
begin
  if Assigned(FOnDraw) then
    FOnDraw(nil);
end;

{**************************************************}

procedure TJvProgressThread.Execute;
begin
  while not Terminated do
  begin
    Synchronize(Draw);
    Sleep(FDelay);
  end;
end;

end.
