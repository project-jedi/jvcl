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
peter3 (load new image only when needed, center image in control, draw border at designtime)

Peter Korf (created JvBlinkingLED from JvTransLED)


Last Modified: 2003-07-

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBlinkingLED;

interface

uses
  Windows, Classes, Graphics, ExtCtrls, JvComponent, JvTransLED;

type
  TJvBlinkingLED = class(TJvTransLED)
  private
    FTimer:    TTimer;
    FColorOn: TColor;
    FColorOff: TColor;
    FStatus: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetColorOn(Value: TColor);
    procedure SetColorOff(Value: TColor);
    procedure SetInterval(Value: Cardinal);
    function GetInterval:Cardinal;
    procedure SetBlinking(Value: Boolean);
    function GetBlinking:Boolean;
    procedure SetStatus(Value: Boolean);
    function GetStatus:Boolean;
  protected
    procedure TimerEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property ColorOn: TColor read FColorOn write SetColorOn default clLime;
    property ColorOff: TColor read FColorOff write SetColorOff default clRed;
    property Status: Boolean read GetStatus write SetStatus default True;
    property Interval: Cardinal read GetInterval write SetInterval;
    property Blinking: Boolean read GetBlinking write SetBlinking default True;
    property Constraints;
    property DragCursor;
    property DragKind;
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
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Controls;

constructor TJvBlinkingLED.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer:=TTimer.Create(self);
  FTimer.OnTimer:=TimerEvent;
  ColorOn:=clLime;
  ColorOff:=clRed;
  Blinking:=True;
  Status:=True;
end;

destructor TJvBlinkingLED.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  inherited Destroy;
end;

procedure TJvBlinkingLED.SetColorOn(Value: TColor);
begin
  FColorOn:=Value;
  Color:=Value;
end;

procedure TJvBlinkingLED.SetColorOff(Value: TColor);
begin
  FColorOff:=Value;
  Color:=Value;
end;

procedure TJvBlinkingLED.SetInterval(Value: Cardinal);
begin
  FTimer.Interval:=Value;
end;

function TJvBlinkingLED.GetInterval:Cardinal;
begin
  Result:=FTimer.Interval;
end;

procedure TJvBlinkingLED.TimerEvent(Sender: TObject);
begin
 Status:=not Status;
end;

procedure TJvBlinkingLED.SetBlinking(Value: Boolean);
begin
 FTimer.Enabled:=Value;
end;

function TJvBlinkingLED.GetBlinking:Boolean;
begin
  Result:=FTimer.Enabled;
end;

procedure TJvBlinkingLED.SetStatus(Value: Boolean);
begin
 FStatus:=Value;
 if Status then
   Color:=ColorOn
  else
   Color:=ColorOff;

 if Assigned(FOnChange) then FOnChange(Self);
end;

function TJvBlinkingLED.GetStatus:Boolean;
begin
  Result:=FStatus;
end;

end.
