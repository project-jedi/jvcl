{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClock.PAS, released on 2001-02-28.

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

unit JvClock;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, JvTypes, JVCLVer;

type
  TClockStyle = (csTime, csTimeDate, csDateTime, csDate);
{$EXTERNALSYM TClockStyle}
  TOnUpdate = procedure(Sender: TObject; Value: string) of object;
{$EXTERNALSYM TOnUpdate}

  TJvClock = class(TWinControl)
  private
    FLabel: TLabel;
    FPanel: TPanel;
    FTimer: TTimer;
    FOnUpdate: TOnUpdate;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FClock: TClockStyle;
    FAboutJVCL: TJVCLAboutInfo;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure OnUpdate(Sender: TObject);
    procedure Setclock(const Value: TClockStyle);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Font: TFont read GetFont write SetFont;
    property OnTime: TOnUpdate read FOnUpdate write FOnUpdate;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property ClockStyle: TClockStyle read FClock write SetClock default csTime;
    property ShowHint;
    property Visible;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnResize;
  end;

implementation

{*******************************************************}

constructor TJvClock.Create(AOwner: TComponent);
begin
  inherited;
  FColor := clInfoBk;
  FOver := False;
  FClock := csTime;

  Width := 100;
  Height := 50;
  FPanel := TPanel.Create(Self);
  FPanel.Parent := Self;
  FPanel.Visible := True;
  FPanel.Align := AlClient;
  FPanel.BevelInner := bvLowered;
  FPanel.BevelOuter := bvRaised;
  FPanel.BevelWidth := 2;

  FLabel := TLabel.Create(FPanel);
  FLabel.Caption := TimeToStr(Time);
  FLabel.Parent := FPanel;
  FLabel.AutoSize := True;
  FLabel.Left := (Width div 2) - (FLabel.Width div 2);
  FLabel.Top := (Height div 2) - (FLabel.Height div 2);
  FLabel.Visible := True;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := OnUpdate;
  FTimer.Interval := 910;
  FTimer.Enabled := True;
end;

{*******************************************************}

destructor TJvClock.Destroy;
begin
  FTImer.Free;
  FLabel.Free;
  FPanel.Free;
  inherited;
end;

{*******************************************************}

function TJvClock.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

{*******************************************************}

procedure TJvClock.OnUpdate(Sender: TObject);
begin
  case FClock of
    csTime:
      FLabel.Caption := TimeToStr(Time);
    csTimeDate:
      FLabel.Caption := TimeToStr(Time) + ' ' + DateToStr(Date);
    csDateTime:
      FLabel.Caption := DateToStr(Date) + ' ' + TimeToStr(Time);
    csDate:
      FLabel.Caption := DateToStr(Date);
  end;
  if Assigned(FOnUpdate) then
    FOnUpdate(Self, FLabel.Caption);
end;

{*******************************************************}

procedure TJvClock.SetFont(const Value: TFont);
begin
  FLabel.Font := Value;
  FLabel.Left := (Width div 2) - (FLabel.Width div 2);
  FLabel.Top := (Height div 2) - (FLabel.Height div 2);
end;

{*******************************************************}

procedure TJvClock.WMSize(var Msg: TWMSize);
begin
  inherited;
  FLabel.Left := (Width div 2) - (FLabel.Width div 2);
  FLabel.Top := (Height div 2) - (FLabel.Height div 2);
end;
{*******************************************************}

procedure TJvClock.CMMouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  Application.HintColor := FColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvClock.CMMouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

procedure TJvClock.Setclock(const Value: TClockStyle);
begin
  FClock := Value;
  OnUpdate(Self);
  FLabel.Left := (Width div 2) - (FLabel.Width div 2);
  FLabel.Top := (Height div 2) - (FLabel.Height div 2);
end;

end.
