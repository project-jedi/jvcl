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

{$I jvcl.inc}

unit JvButton;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFDEF VCL}
  Graphics, Controls, Forms, StdCtrls, Menus, Buttons,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QStdCtrls, QMenus, QButtons, Types,
  QWindows,
  {$ENDIF VisualCLX}
  JvComponent, JvConsts, JvTypes, JvExStdCtrls, JvThemes;

type
  TJvButtonMouseState = (bsMouseInside, bsMouseDown);
  TJvButtonMouseStates = set of TJvButtonMouseState;

  TJvCustomGraphicButton = class(TJvGraphicControl)
  private
    FStates: TJvButtonMouseStates;
    FBuffer: TBitmap;
    FFlat: Boolean;
    FDropDownMenu: TPopupMenu;
    FDown: Boolean;
    FOnParentColorChanged: TNotifyEvent;
    FForceSameSize: Boolean;
    FAllowAllUp: Boolean;
    FGroupIndex: Integer;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FHotTrackFontOptions: TJvTrackFontOptions;
    function GetPattern: TBitmap;
    procedure SetFlat(const Value: Boolean);
    procedure SetDown(Value: Boolean);

    procedure CMButtonPressed(var Msg: TJvCMButtonPressed); message CM_JVBUTTONPRESSED;
    procedure CMForceSize(var Msg: TCMForceSize); message CM_FORCESIZE;
    {$IFDEF VCL}
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    {$ENDIF VCL}
    procedure SetForceSameSize(const Value: Boolean);
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetHotFont(const Value: TFont);
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    function DoDropDownMenu(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    procedure UpdateExclusive;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure Paint; override;
    procedure PaintButton(Canvas: TCanvas); virtual;
    procedure PaintFrame(Canvas: TCanvas); virtual;
    function InsideBtn(X, Y: Integer): Boolean; virtual;

    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; override;
    procedure EnabledChanged; override;
    procedure FontChanged; override;
    procedure ParentColorChanged; override;
    procedure RepaintBackground;virtual;


    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;

    property MouseStates: TJvButtonMouseStates read FStates write FStates default [];
    property ForceSameSize: Boolean read FForceSameSize write SetForceSameSize default False;
    property Pattern: TBitmap read GetPattern;
    property Flat: Boolean read FFlat write SetFlat default True;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property Down: Boolean read FDown write SetDown default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;

    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvCustomButton = class(TJvExButton)
  private
    FColor: TColor;
    FSaved: TColor;
    FOnParentColorChanged: TNotifyEvent;
    FDropDownMenu: TPopupMenu;
    FHotTrack: Boolean;
    FHotFont: TFont;
    FFontSave: TFont;
    FOver: Boolean;
    FWordWrap: Boolean;
    FForceSameSize: Boolean;
    FHotTrackFontOptions: TJvTrackFontOptions;
    procedure SetHotFont(const Value: TFont);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetForceSameSize(const Value: Boolean);
    procedure CMForceSize(var Msg: TCMForceSize); message CM_FORCESIZE;
    procedure SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure FontChanged; override;
    procedure ParentColorChanged; override;
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    function GetRealCaption: string; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
    property ForceSameSize: Boolean read FForceSameSize write SetForceSameSize default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property HotTrack: Boolean read FHotTrack write FHotTrack default False;
    property HotTrackFont: TFont read FHotFont write SetHotFont;
    property HotTrackFontOptions: TJvTrackFontOptions read FHotTrackFontOptions write SetHotTrackFontOptions default
      DefaultTrackFontOptions;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure Click; override;
  end;

  // TJvDropDownButton draws a DropDown button with the DropDown glyph
  // (also themed). It ignores the properties Glyph and Flat
  TJvDropDownButton = class(TSpeedButton)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  JvJVCLUtils;

const
  JvBtnLineSeparator = '|';

var
  GlobalPattern: TBitmap = nil;

function CreateBrushPattern: TBitmap;
var
  X, Y: Integer;
begin
  if GlobalPattern = nil then
  begin
    GlobalPattern := TBitmap.Create;
    try
      GlobalPattern.Width := 8; { must have this size }
      GlobalPattern.Height := 8;
      with GlobalPattern.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := clBtnFace;
        FillRect(Rect(0, 0, GlobalPattern.Width, GlobalPattern.Height));
        for Y := 0 to 7 do
          for X := 0 to 7 do
            if (Y mod 2) = (X mod 2) then { toggles between even/odd pixels }
              Pixels[X, Y] := clWhite; { on even/odd rows }
      end;
    except
      FreeAndNil(GlobalPattern);
    end;
  end;
  Result := GlobalPattern;
end;

// == TJvCustomGraphicButton ===================================================

constructor TJvCustomGraphicButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque, csDoubleClicks];
  FStates := [];
  SetBounds(0, 0, 40, 40);
  FBuffer := TBitmap.Create;
  FFlat := True;
  FForceSameSize := False;
  FHotFont := TFont.Create;
  FHotTrackFontOptions := DefaultTrackFontOptions;
end;

destructor TJvCustomGraphicButton.Destroy;
begin
  FBuffer.Free;
  FHotFont.Free;
  inherited Destroy;
end;

{ Handle speedkeys (Alt + key) }

function TJvCustomGraphicButton.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := IsAccel(Key, Caption) and Enabled and (ssAlt in Shift);
  if Result then
    Click
  else
    Result := inherited WantKey(Key, Shift, KeyText);
end;

procedure TJvCustomGraphicButton.EnabledChanged;
begin
  inherited EnabledChanged;
  if not Enabled then
    FStates := [];
  RepaintBackground;
end;

procedure TJvCustomGraphicButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if Enabled then
  begin
    Include(FStates, bsMouseInside);
    inherited MouseEnter(Control);
    if Flat then
      RepaintBackground;
  end;
end;

procedure TJvCustomGraphicButton.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if Enabled then
  begin
    Exclude(FStates, bsMouseInside);
    inherited MouseLeave(Control); // trigger event
    if Flat then
      RepaintBackground;
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
  if (bsMouseInside in FStates) and HotTrack then
    Canvas.Font := FHotFont
  else
    Canvas.Font := Font;
end;

function TJvCustomGraphicButton.InsideBtn(X, Y: Integer): Boolean;
begin
  Result := PtInRect(Rect(0, 0, Width, Height), Point(X, Y));
end;

procedure TJvCustomGraphicButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Tmp: TPoint;
begin
  if not Enabled then
    Exit;

  inherited MouseDown(Button, Shift, X, Y);

  if InsideBtn(X, Y) then
  begin
    FStates := [bsMouseDown, bsMouseInside];
    RepaintBackground;
  end;
  Tmp := ClientToScreen(Point(0, Height));
  DoDropDownMenu(Button, Shift, Tmp.X, Tmp.Y);
end;

procedure TJvCustomGraphicButton.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
  begin
    if not InsideBtn(X, Y) then
    begin
      Exclude(FStates, bsMouseInside);
      RepaintBackground;
    end
    else
    begin
      Include(FStates, bsMouseInside);
      RepaintBackground;
    end;
  end;
end;

procedure TJvCustomGraphicButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled or Down then
    Exit;
  inherited MouseUp(Button, Shift, X, Y);
  Exclude(FStates, bsMouseDown);
  RepaintBackground;
end;

function TJvCustomGraphicButton.DoDropDownMenu(Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
{$IFDEF VCL}
var
  Msg: TMsg;
{$ENDIF VCL}
begin
  Result := (Button = mbLeft) and (DropDownMenu <> nil);
  if Result then
  begin
    DropDownMenu.PopupComponent := Self;
    case DropDownMenu.Alignment of
      paRight:
         Inc(X, Width);
      paCenter:
        Inc(X, Width div 2);
    end;
    DropDownMenu.Popup(X, Y);
    {$IFDEF VCL}
    { wait 'til menu is done }
    while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do
      {nothing};
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    repeat
      Application.ProcessMessages; // (ahuser) does this really do the job?
    until not QWidget_isVisible(DropDownMenu.handle); // (asn) it did not, now it does
    {$ENDIF VisualCLX}
    { release button }
    MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TJvCustomGraphicButton.SetFlat(const Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if FFlat then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    RepaintBackground;
  end;
end;

procedure TJvCustomGraphicButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DropDownMenu) then
    DropDownMenu := nil;
end;

procedure TJvCustomGraphicButton.SetDown(Value: Boolean);
begin
  if GroupIndex <> 0 then
    Value := False;
  if FDown <> Value then
  begin
    if FDown and not AllowAllUp then
      Exit;
    FDown := Value;
    if FDown then
    begin
      Include(FStates, bsMouseDown);
      {     Click; }{ uncomment and see what happens... }
    end
    else
      Exclude(FStates, bsMouseDown);
  end;
end;

procedure TJvCustomGraphicButton.SetForceSameSize(const Value: Boolean);
begin
  if FForceSameSize <> Value then
  begin
    FForceSameSize := Value;
    if FForceSameSize then
      SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TJvCustomGraphicButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Form: TCustomForm;
  Msg: TCMForceSize;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if ForceSameSize then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) then
    begin
      // (p3) what is this rect doing here?
      Msg.Msg := CM_FORCESIZE;
      Msg.Sender := Self;
      Msg.NewSize.X := AWidth;
      Msg.NewSize.Y := AHeight;
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

function TJvCustomGraphicButton.GetPattern: TBitmap;
begin
  Result := CreateBrushPattern;
end;

procedure TJvCustomGraphicButton.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TJvCustomGraphicButton.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TJvCustomGraphicButton.UpdateExclusive;
var
  Msg: TJvCMButtonPressed;
begin
  if (GroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_JVBUTTONPRESSED;
    Msg.Index := GroupIndex;
    Msg.Control := Self;
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJvCustomGraphicButton.CMButtonPressed(var Msg: TJvCMButtonPressed);
var
  Sender: TJvCustomGraphicButton;
begin
  if Msg.Index = GroupIndex then
  begin
    Sender := TJvCustomGraphicButton(Msg.Control);
    if Sender <> Self then
    begin
      if Sender.Down and Down then
      begin
        Down := False;
        Exclude(FStates, bsMouseDown);
        RepaintBackground;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TJvCustomGraphicButton.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

procedure TJvCustomGraphicButton.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
  end;
end;

{$IFDEF VCL}
procedure TJvCustomGraphicButton.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  RepaintBackground;
end;
{$ENDIF VCL}

procedure TJvCustomGraphicButton.FontChanged;
begin
  inherited FontChanged;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCustomGraphicButton.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

// == TJvCustomButton ==========================================================

constructor TJvCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  FHotTrack := False;
  FHotFont := TFont.Create;
  FFontSave := TFont.Create;
  // ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FWordWrap := True;
  FForceSameSize := False;
  FHotTrackFontOptions := DefaultTrackFontOptions;
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
    FDropDownMenu.PopupComponent := Self;
    FDropDownMenu.Popup(GetClientOrigin.X, GetClientOrigin.Y + Height);
    {$IFDEF VCL}
    Perform(CM_MOUSELEAVE, 0, 0);
    {$ELSE}
    MouseLeave(Self);
    {$ENDIF VCL}
  end;
end;

{$IFDEF VCL}
procedure TJvCustomButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_MULTILINE;
end;
{$ENDIF VCL}

procedure TJvCustomButton.SetHotTrackFontOptions(const Value: TJvTrackFontOptions);
begin
  if FHotTrackFontOptions <> Value then
  begin
    FHotTrackFontOptions := Value;
    UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
  end;
end;

procedure TJvCustomButton.SetHotFont(const Value: TFont);
begin
  FHotFont.Assign(Value);
end;

procedure TJvCustomButton.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
    begin
      FFontSave.Assign(Font);
      Font.Assign(FHotFont);
    end;
    FOver := True;
    inherited MouseEnter(Control);
  end;
end;

procedure TJvCustomButton.MouseLeave(Control: TControl);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Font.Assign(FFontSave);
    FOver := False;
    inherited MouseLeave(Control);
  end;
end;

procedure TJvCustomButton.FontChanged;
begin
  inherited FontChanged;
  UpdateTrackFont(HotTrackFont, Font, HotTrackFontOptions);
end;

procedure TJvCustomButton.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

function TJvCustomButton.GetRealCaption: string;
begin
  if WordWrap then
    Result := StringReplace(Caption, JvBtnLineSeparator, #10, [rfReplaceAll])
  else
    Result := Caption;
end;

procedure TJvCustomButton.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TJvCustomButton.SetForceSameSize(const Value: Boolean);
begin
  if FForceSameSize <> Value then
  begin
    FForceSameSize := Value;
    if FForceSameSize then
      SetBounds(Left, Top, Width, Height);
  end;
end;

procedure TJvCustomButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  Form: TCustomForm;
  Msg: TCMForceSize;
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if ForceSameSize then
  begin
    Form := GetParentForm(Self);
    if Assigned(Form) then
    begin
      Msg.Msg := CM_FORCESIZE;
      Msg.Sender := Self;
      Msg.NewSize.X := AWidth;
      Msg.NewSize.Y := AHeight;
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
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDropDownMenu) then
    DropDownMenu := nil;
end;

procedure TJvCustomGraphicButton.RepaintBackground;
var R:TRect;
begin
  if (Parent <> nil) and Parent.HandleAllocated then
  begin
    R := BoundsRect;
    InvalidateRect(Parent.Handle,@R, True);
  end;
  Repaint;
end;

// == TJvDropDownButton ===================================================

constructor TJvDropDownButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 16;
  Height := 16;
end;

procedure TJvDropDownButton.Paint;
var
  PaintRect: TRect;
  DrawFlags: Integer;
  dc: HDC;
  Bmp: TBitmap;
begin
  // adjust FState and FDragging
  dc := Canvas.Handle;
  Bmp := TBitmap.Create;
  try
    Bmp.Width := 1;
    Bmp.Height := 1;
    Canvas.Handle := Bmp.Canvas.Handle;
    try
      inherited Paint;
    finally
      Canvas.Handle := dc;
    end;
  finally
    Bmp.Free;
  end;

  PaintRect := Rect(0, 0, Width, Height);
  DrawFlags := DFCS_SCROLLCOMBOBOX or DFCS_ADJUSTRECT;
  if FState in [bsDown, bsExclusive] then
    DrawFlags := DrawFlags or DFCS_PUSHED;

  {$IFDEF JVCLThemesEnabled}
  if ThemeServices.ThemesEnabled then
    DrawThemedFrameControl(Self, Canvas.Handle, PaintRect, DFC_SCROLL, DrawFlags)
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    {$IFDEF VisualCLX}
    Canvas.Start;
    {$ENDIF VisualCLX}
    DrawFrameControl(Canvas.Handle, PaintRect, DFC_SCROLL, DrawFlags);

    {$IFDEF VisualCLX}
    Canvas.Stop;
    {$ENDIF VisualCLX}
  end;
end;


initialization

finalization
  FreeAndNil(GlobalPattern);

end.

