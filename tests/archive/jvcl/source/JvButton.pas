{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButton.PAS, released on 2001-02-28.

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

unit JvButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Menus, Controls,
  Forms, JvComponent, JVCLVer;

const
  CM_FORCESIZE = WM_USER + 777;

type
  TCmForceSize = record
    Msg: Cardinal;
    Sender: TControl;
    NewSize: TSmallPoint;
    Result: Longint;
  end;

  TJvButtonMouseState = (bsMouseInside, bsMouseDown);
  TJvButtonMouseStates = set of TJvButtonMouseState;

  TJvCustomGraphicButton = class(TJvGraphicControl)
  private
    FCaption: TCaption;
    FStates: TJvButtonMouseStates;
    FPattern,FBuffer: TBitmap;
    FOnMouseExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FFlat: boolean;
    FDropDownMenu: TPopupMenu;
    FDown: boolean;
    FOnParentColorChanged: TNotifyEvent;
    FForceSameSize: boolean;
    procedure SetCaption(const Value: TCaption);
    procedure SetFlat(const Value: boolean);
    procedure SetDown(const Value: boolean);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SetForceSameSize(const Value: boolean);
    procedure CMForceSize(var Msg: TCMForceSize); message CM_FORCESIZE;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; virtual;
    procedure MouseExit; virtual;
    procedure Paint; override;
    procedure PaintButton(Canvas: TCanvas); virtual;
    procedure PaintFrame(Canvas: TCanvas); virtual;
    function InsideBtn(X, Y: Integer): Boolean; virtual;
    property MouseStates:TJvButtonMouseStates read FStates write FStates default [];
    property ForceSameSize:boolean read FForceSameSize write SetForceSameSize default false;
    property Pattern:TBitmap read FPattern;
    property Caption:TCaption read FCaption write SetCaption;
    property Flat:boolean read FFlat write SetFlat default True;
    property Down:boolean read FDown write SetDown default false;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvCustomButton = class(TButton)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FDropDownMenu: TPopupMenu;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    FOver: Boolean;
    FWordWrap: boolean;
    FForceSameSize: boolean;
    procedure SetHotFont(const Value: TFont);
    procedure SetWordWrap(const Value: boolean);
    procedure SetForceSameSize(const Value: boolean);
    procedure CMForceSize(var Msg: TCMForceSize); message CM_FORCESIZE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
  protected
    procedure DoMouseEnter; dynamic;
    procedure DoMouseLeave; dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetRealCaption: string;dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    procedure Click; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  protected
    property WordWrap:boolean read FWordWrap write SetWordWrap default true;
    property ForceSameSize:boolean read FForceSameSize write SetForceSameSize default false;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FonMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

//  TJvButton = class(TJvCustomButton);

implementation

const
  JvBtnLineSeparator = '|';


function CreateBrushPattern: TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  Result.Width := 8; { must have this size }
  Result.Height := 8;
  with Result.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBtnFace;
    FillRect(Rect(0, 0, Result.Width, Result.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixles }
          Pixels[X, Y] := clWhite; { on even/odd rows }
  end;
end;

constructor TJvCustomGraphicButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque,csDoubleClicks];
  FStates := [];
  SetBounds(0, 0, 40, 40);
  FPattern := CreateBrushPattern;
  FBuffer := TBitmap.Create;
  FFlat := True;
  FForceSameSize := false;
end;

destructor TJvCustomGraphicButton.Destroy;
begin
  FPattern.Free;
  FBuffer.Free;
  inherited Destroy;
end;

{ Handle speedkeys (Alt + key) }

procedure TJvCustomGraphicButton.CMDialogChar(var Msg: TCMDialogChar);
begin
  with Msg do
    if IsAccel(CharCode, FCaption) and Enabled then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TJvCustomGraphicButton.CMEnabledChanged(var Msg: TMessage);
begin
  if not Enabled then
    FStates := [];
  Repaint;
end;

procedure TJvCustomGraphicButton.CMMouseEnter(var Msg: TMessage);
begin
  MouseEnter;
end;

procedure TJvCustomGraphicButton.CMMouseLeave(var Msg: TMessage);
begin
  MouseEnter;
  inherited;
end;

procedure TJvCustomGraphicButton.MouseEnter;
begin
  // for D7...
  if csDesigning in ComponentState then
    Exit;
  if Enabled then
  begin
    Include(FStates,bsMouseInside);
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);
    if Flat then
      Refresh;
  end;
end;

procedure TJvCustomGraphicButton.MouseExit;
begin
  if csDesigning in ComponentState then
    Exit; // for D7...
  if Enabled then
  begin
    Exclude(FStates,bsMouseInside);
    if Assigned(FOnMouseExit) then
      FOnMouseExit(Self);
    if Flat then
      Refresh;
  end;
end;

procedure TJvCustomGraphicButton.Paint;
begin
//  FBuffer.Width := Width;
//  FBuffer.Height := Height;
  PaintFrame(Canvas);
  PaintButton(Canvas);
//  BitBlt(Canvas.Handle,0,0,Width,Height,FBuffer.Canvas.Handle,0,0,SRCCOPY);
end;

procedure TJvCustomGraphicButton.PaintFrame(Canvas: TCanvas);
begin
// do nothing
end;

procedure TJvCustomGraphicButton.PaintButton(Canvas: TCanvas);
begin
// do nothing
end;

function TJvCustomGraphicButton.InsideBtn(X, Y: Integer): Boolean;
begin
  Result := PtInRect(Rect(0, 0, Width, Height), Point(X, Y));
end;

procedure TJvCustomGraphicButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Tmp: TPoint;
  Msg: TMsg;
begin
  if not Enabled then
    Exit;

  inherited MouseDown(Button, Shift, X, Y);

  //   if Assigned(OnMouseDown) then OnMouseDown(Self,Button,Shift,X,Y);

  if InsideBtn(X, Y) then
  begin
    FStates := [bsMouseDown,bsMouseInside];
    Repaint;
  end;
  if Assigned(FDropDownMenu) and (Button = mbLeft) then
  begin
    { calc where to put menu }
    Tmp := ClientToScreen(Point(0, Height));
    FDropDownMenu.PopupComponent := self; 

    FDropDownMenu.Popup(Tmp.X, Tmp.Y);
    { wait 'til menu is done }
    while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      {nothing};
    { release button }
    MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TJvCustomGraphicButton.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  //   if Assigned(OnMouseMove) then OnMouseMove(Self,Shift,X,Y);
  if (bsMouseDown in FStates) then
  begin
    if not InsideBtn(X, Y) then
    begin
      if FStates = [bsMouseDown] then { mouse has slid off, so release }
      begin
        FStates := [];
        Repaint;
      end;
    end
    else
    begin
      if FStates = [] then { mouse has slid back on, so push }
      begin
        FStates := [bsMouseDown,bsMouseInside];
        Repaint;
      end;
    end;
  end;
end;

procedure TJvCustomGraphicButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled or Down then
    Exit;
  inherited MouseUp(Button, Shift, X, Y);

  Exclude(FStates,bsMouseDown);
  Repaint;
end;

procedure TJvCustomGraphicButton.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Invalidate;
  end;
end;

procedure TJvCustomGraphicButton.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

procedure TJvCustomGraphicButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
end;

procedure TJvCustomGraphicButton.SetDown(const Value: boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
    if FDown then
    begin
      Include(FStates,bsMouseDown);
      {     Click; }{ uncomment and see what happens... }
    end
    else
      Exclude(FStates,bsMouseDown);
    Repaint;
  end;
end;

constructor TJvCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
//  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FWordWrap := true;
  FForceSameSize := false;
end;

destructor TJvCustomButton.Destroy;
begin
  FHotFont.Free;
  FFontSave.Free;
  inherited Destroy;
end;

procedure TJvCustomButton.Click;
begin
  inherited Click;
  if FDropDownMenu <> nil then
  begin
    FDropDownMenu.PopupComponent := self;
    FDropDownMenu.Popup(GetClientOrigin.x, GetClientOrigin.y + Height);
    Perform(CM_MOUSELEAVE, 0, 0);
  end;
end;

procedure TJvCustomButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_MULTILINE;
end;

procedure TJvCustomButton.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FonCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomButton.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomButton.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotFont);
    end;
    FOver := True;
  end;
  DoMouseEnter;
end;

procedure TJvCustomButton.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
  end;
  DoMouseLeave;
end;

procedure TJvCustomButton.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

procedure TJvCustomGraphicButton.CMParentColorChanged(var Msg: TMessage);
begin
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
  inherited;
end;

procedure TJvCustomButton.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomButton.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;


function TJvCustomButton.GetRealCaption: string;
begin
  if WordWrap then
    Result := StringReplace(Caption, JvBtnLineSeparator, #10, [rfReplaceAll])
  else
    Result := Caption;
end;

procedure TJvCustomButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvCustomButton.SetForceSameSize(const Value: boolean);
begin
  if FForceSameSize <> Value then
  begin
    FForceSameSize := Value;
    if FForceSameSize then
      SetBounds(Left,Top,Width,Height);
  end;
end;

procedure TJvCustomButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Form: TCustomForm;
  Msg: TCMForceSize;
begin
  inherited;
  if ForceSameSize then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) then
    begin
      // (p3) what is this rect doing here?
      // r := Rect(aLeft, aTop, aWidth, aHeight);
      Msg.Msg := CM_FORCESIZE;
      Msg.Sender := Self;
      Msg.NewSize.x := AWidth;
      Msg.NewSize.y := AHeight;
      Form.Broadcast(Msg);
    end;
  end;
end;


procedure TJvCustomButton.CMForceSize(var Msg: TCMForceSize);
begin
  with Msg do
    if Sender <> Self then
      with NewSize do
        inherited SetBounds(Left, Top, X, Y);
end;

procedure TJvCustomButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    DropDownMenu := nil;
end;

procedure TJvCustomGraphicButton.SetForceSameSize(const Value: boolean);
begin
  if FForceSameSize <> Value then
  begin
    FForceSameSize := Value;
    if FForceSameSize then
      SetBounds(Left,Top,Width,Height);
  end;
end;

procedure TJvCustomGraphicButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  Form: TCustomForm;
  Msg: TCMForceSize;
begin
  inherited;
  if ForceSameSize then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) then
    begin
      // (p3) what is this rect doing here?
      Msg.Msg := CM_FORCESIZE;
      Msg.Sender := self;
      Msg.NewSize.x := AWidth;
      Msg.NewSize.y := AHeight;
      Form.Broadcast(Msg);
    end;
  end;
end;

procedure TJvCustomGraphicButton.CMForceSize(var Msg: TCMForceSize);
begin
  with Msg do
    if Sender <> Self then
      with NewSize do
        inherited SetBounds(Left, Top, X, Y);
end;

end.

