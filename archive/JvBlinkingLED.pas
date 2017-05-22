{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransled.PAS, released on 2002-12-23.

The Initial Developer of the Original Code is Thomas Hensle (http://www.thensle.de)
Portions created by Thomas Hensle are Copyright (C) 2002 Thomas Hensle.
Portions created by XXXX Corp. are Copyright (C) 2002, 2003 XXXX Corp.
All Rights Reserved.

Contributor(s):
Thomas Huber (Thomas_D_huber@t-online.de)
peter3
  (load new image only when needed, center image in control, draw border at designtime,
  renamed "Blinking" property to "Active", reimplemented using a thread isto timer,
  Active is false by default, default interval is 1000, if interval is < 1, thread is not activated)


Peter Korf (created JvBlinkingLED from JvTransLED)

Last Modified: 2003-10-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvBlinkingLED;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Controls, Graphics, ExtCtrls,
  {$ENDIF}
  {$IFDEF VisualCLX}
  QControls, QGraphics, QExtCtrls,
  {$ENDIF}
  JvComponent, JvTransLED;

type
  TJvBlinkingLED = class(TJvTransLED)
  private
    FThread: TThread;
    FColorOn: TColor;
    FColorOff: TColor;
    FStatus: Boolean;
    FOnChange: TNotifyEvent;
    FInterval: Cardinal;
    procedure SetColorOn(Value: TColor);
    procedure SetColorOff(Value: TColor);
    procedure SetInterval(Value: Cardinal);
    procedure SetActive(Value: Boolean);
    function GetActive: Boolean;
    procedure SetStatus(Value: Boolean);
    function GetStatus: Boolean;
    procedure DoBlink(Sender: Tobject; BlinkOn: boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF VCL}
    property DragCursor;
    property DragKind;
    property OnStartDock;
    property OnEndDock;
    {$ENDIF}
    property Align;
    property Anchors;
    property AutoSize;
    property ColorOn: TColor read FColorOn write SetColorOn default clLime;
    property ColorOff: TColor read FColorOff write SetColorOff default clRed;
    property Status: Boolean read GetStatus write SetStatus default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property Active: Boolean read GetActive write SetActive default false;
    property Constraints;
    property DragMode;
    property Height default 17;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 17;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

type
  TBlinkEvent = procedure(Sender: TObject; BlinkOn: boolean) of object;
  TBlinkThread = class(TThread)
  private
    FOnBlink: TBlinkEvent;
    FBlinkOn: boolean;
    FInterval: Cardinal;
    procedure DoBlink;
  public
    constructor Create(Interval: Cardinal);
    procedure Execute; override;
    property Interval: Cardinal read FInterval;
    property OnBlink: TBlinkEvent read FOnBlink write FOnBlink;
  end;

constructor TJvBlinkingLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterval := 1000;
  ColorOn := clLime;
  ColorOff := clRed;
  Active := false;
  Status := True;
end;

destructor TJvBlinkingLED.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  FreeAndNil(FThread);
  inherited Destroy;
end;

procedure TJvBlinkingLED.SetColorOn(Value: TColor);
begin
  FColorOn := Value;
  Color := Value;
end;

procedure TJvBlinkingLED.SetColorOff(Value: TColor);
begin
  FColorOff := Value;
  Color := Value;
end;

procedure TJvBlinkingLED.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    if FThread <> nil then
    begin
      FreeAndNil(FThread);
      FThread := TBlinkThread.Create(FInterval);
      TBlinkThread(FThread).OnBlink := DoBlink;
      if FInterval > 0 then
        FThread.Resume;
    end;
  end;
end;

procedure TJvBlinkingLED.SetActive(Value: Boolean);
begin
  if Value then
  begin
    if (FThread = nil) then
      FThread := TBlinkThread.Create(Interval);
    TBlinkThread(FThread).OnBlink := DoBlink;
    if Interval > 0 then
      FThread.Resume;
  end
  else if FThread <> nil then
    FThread.Suspend;
end;

function TJvBlinkingLED.GetActive: Boolean;
begin
  Result := (FThread <> nil) and (FInterval > 0) and not FThread.Suspended;
end;

procedure TJvBlinkingLED.SetStatus(Value: Boolean);
begin
  FStatus := Value;
  if Status then
    Color := ColorOn
  else
    Color := ColorOff;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TJvBlinkingLED.GetStatus: Boolean;
begin
  Result := FStatus;
end;

{ TBlinkThread }

constructor TBlinkThread.Create(Interval: Cardinal);
begin
  inherited Create(true);
  FInterval := Interval;
end;

procedure TBlinkThread.DoBlink;
begin
  if Assigned(FOnBlink) then FOnBlink(self, FBlinkOn);
  FBlinkOn := not FBlinkOn;
end;

procedure TBlinkThread.Execute;
begin
  FBlinkOn := false;
  while not Terminated and not Suspended do
  begin
    Synchronize(DoBlink);
    sleep(FInterval);
  end;
end;

procedure TJvBlinkingLED.DoBlink(Sender: Tobject; BlinkOn: boolean);
begin
  Status := BlinkOn;
end;

end.

