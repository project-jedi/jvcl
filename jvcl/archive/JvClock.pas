{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvClock.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvClock;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls,
  JvComponent;

type
  TJvClockStyle = (csTime, csTimeDate, csDateTime, csDate);
  TJvOnTimeUpdate = procedure(Sender: TObject; Value: string) of object;

  TJvClock = class(TJvWinControl)
  private
    FLabel: TLabel;
    FTimer: TTimer;
    FOnTime: TJvOnTimeUpdate;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FHintColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FClock: TJvClockStyle;
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    procedure OnTimeUpdate(Sender: TObject);
    procedure SetClockStyle(const Value: TJvClockStyle);
    procedure UpdateTextPosition;
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ClockStyle: TJvClockStyle read FClock write SetClockStyle default csTime;
    property Font: TFont read GetFont write SetFont;
    property Height default 50;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnTime: TJvOnTimeUpdate read FOnTime write FOnTime;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property ShowHint;
    property Visible;
    property Width default 100;
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

uses
  JvThemes;

constructor TJvClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IncludeThemeStyle(Self, [csParentBackground]);
  FHintColor := clInfoBk;
  FOver := False;
  FClock := csTime;

  Width := 100;
  Height := 50;

  FLabel := TLabel.Create(Self);
  FLabel.Caption := TimeToStr(Time);
  FLabel.Parent := Self;
  FLabel.AutoSize := True;
  UpdateTextPosition;
  FLabel.Visible := True;

  FTimer := TTimer.Create(Self);
  FTimer.OnTimer := OnTimeUpdate;
  FTimer.Interval := 910;
  FTimer.Enabled := True;
end;

destructor TJvClock.Destroy;
begin
  FTimer.Enabled := False;
  inherited Destroy;
end;

function TJvClock.GetFont: TFont;
begin
  Result := FLabel.Font;
end;

procedure TJvClock.OnTimeUpdate(Sender: TObject);
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
  if Assigned(FOnTime) then
    FOnTime(Self, FLabel.Caption);
end;

procedure TJvClock.SetFont(const Value: TFont);
begin
  FLabel.Font := Value;
  UpdateTextPosition;
end;

procedure TJvClock.WMSize(var Msg: TWMSize);
begin
  inherited;
  UpdateTextPosition;
end;

procedure TJvClock.CMMouseEnter(var Msg: TMessage);
begin
  FOver := True;
  FSaved := Application.HintColor;
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FHintColor;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvClock.CMMouseLeave(var Msg: TMessage);
begin
  Application.HintColor := FSaved;
  FOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvClock.SetClockStyle(const Value: TJvClockStyle);
begin
  FClock := Value;
  OnTimeUpdate(Self);
  UpdateTextPosition;
  FLabel.Left := (Width div 2) - (FLabel.Width div 2);
  FLabel.Top := (Height div 2) - (FLabel.Height div 2);
end;

procedure TJvClock.UpdateTextPosition;
begin
  FLabel.Left := (Width div 2) - (FLabel.Width div 2);
  FLabel.Top := (Height div 2) - (FLabel.Height div 2);
end;

end.
