{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExForms.pas, released on 2004-01-04

The Initial Developer of the Original Code is Andreas Hausladen [Andreas.Hausladen@gmx.de]
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

Last Modified: 2004-01-12

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}

unit JvExForms;
interface
uses
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms,
  {$ENDIF VisualCLX}
  Classes, SysUtils,
  JvThemes, JvExControls;

type
  
  TJvExScrollingWinControl = class(TScrollingWinControl, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
  {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
  {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
    {$IFDEF REINTRODUCE_HITTEST}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
    {$ENDIF REINTRODUCE_HITTEST}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExScrollBox = class(TScrollBox, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
  {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
  {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
    {$IFDEF REINTRODUCE_HITTEST}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
    {$ENDIF REINTRODUCE_HITTEST}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomFrame = class(TCustomFrame, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
  {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
  {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
    {$IFDEF REINTRODUCE_HITTEST}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
    {$ENDIF REINTRODUCE_HITTEST}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExFrame = class(TFrame, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
  {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
  {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
    {$IFDEF REINTRODUCE_HITTEST}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
    {$ENDIF REINTRODUCE_HITTEST}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  private
    FCanvas: TCanvas;
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
    procedure Paint; virtual;
    property Canvas: TCanvas read FCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF VisualCLX}
  end;
  
  TJvExCustomForm = class(TCustomForm, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
  {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
  {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
    {$IFDEF REINTRODUCE_HITTEST}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
    {$ENDIF REINTRODUCE_HITTEST}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end; // do not implement Painting()
  
  TJvExForm = class(TForm, IJvWinControlEvents, IJvControlEvents)
  {$IFDEF VCL}
  protected
  { IJvControlEvents }
    procedure VisibleChanged; dynamic;
    procedure EnabledChanged; dynamic;
    procedure TextChanged; dynamic;
    procedure FontChanged; dynamic;
    procedure ColorChanged; dynamic;
    procedure ParentFontChanged; dynamic;
    procedure ParentColorChanged; dynamic;
    procedure ParentShowHintChanged; dynamic;
    function WantKey(Key: Integer; Shift: TShiftState;
      const KeyText: WideString): Boolean; virtual;
    function HintShow(var HintInfo : THintInfo): Boolean; dynamic;
    function HitTest(X, Y: Integer): Boolean; dynamic;
    procedure MouseEnter(Control: TControl); dynamic;
    procedure MouseLeave(Control: TControl); dynamic;
  {$IFNDEF HASAUTOSIZE}
  {$IFNDEF COMPILER6_UP}
    procedure SetAutoSize(Value: Boolean); virtual;
  {$ENDIF !COMPILER6_UP}
  {$ENDIF !HASAUTOSIZE}
  { IJvWinControlEvents }
    procedure CursorChanged; dynamic;
    procedure ShowingChanged; dynamic;
    procedure ShowHintChanged; dynamic;
    procedure ControlsListChanging(Control: TControl; Inserting: Boolean); dynamic;
    procedure ControlsListChanged(Control: TControl; Inserting: Boolean); dynamic;
  {$IFDEF JVCLThemesEnabledD56}
  private
    function GetParentBackground: Boolean;
  protected
    procedure SetParentBackground(Value: Boolean); virtual;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
  {$ENDIF JVCLThemesEnabledD56}
  public
    procedure Dispatch(var Msg); override;
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF VCL}
  protected
    function DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean; virtual;
  {$IFDEF VisualCLX}
    {$IFDEF REINTRODUCE_HITTEST}
  protected
    function HitTest(X, Y: Integer): Boolean; overload; dynamic;
    {$ENDIF REINTRODUCE_HITTEST}
   {$IF not declared(PatchedVCLX)}
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
   {$IFEND}
  protected
    procedure Painting(Sender: QObjectH; EventRegion: QRegionH); override;
  {$ENDIF VisualCLX}
  end; // do not implement Painting()

implementation


{$IFDEF VCL}
procedure TJvExScrollingWinControl.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExScrollingWinControl.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExScrollingWinControl.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExScrollingWinControl.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExScrollingWinControl.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExScrollingWinControl.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExScrollingWinControl.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExScrollingWinControl.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExScrollingWinControl.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExScrollingWinControl.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExScrollingWinControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExScrollingWinControl.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExScrollingWinControl.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExScrollingWinControl.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExScrollingWinControl.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}
 {$IFDEF REINTRODUCE_HITTEST}
function TJvExScrollingWinControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
 {$ENDIF REINTRODUCE_HITTEST}

 {$IF not declared(PatchedVCLX)}
procedure TJvExScrollingWinControl.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExScrollingWinControl.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExScrollingWinControl.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExScrollingWinControl.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExScrollingWinControl.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExScrollingWinControl.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExScrollingWinControl.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExScrollingWinControl.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExScrollingWinControl.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExScrollingWinControl.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExScrollingWinControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExScrollingWinControl.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExScrollingWinControl.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExScrollingWinControl.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExScrollBox.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExScrollBox.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExScrollBox.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExScrollBox.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExScrollBox.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExScrollBox.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExScrollBox.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExScrollBox.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExScrollBox.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExScrollBox.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExScrollBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExScrollBox.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExScrollBox.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExScrollBox.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExScrollBox.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}
 {$IFDEF REINTRODUCE_HITTEST}
function TJvExScrollBox.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
 {$ENDIF REINTRODUCE_HITTEST}

 {$IF not declared(PatchedVCLX)}
procedure TJvExScrollBox.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExScrollBox.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExScrollBox.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExScrollBox.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExScrollBox.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExScrollBox.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExScrollBox.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExScrollBox.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExScrollBox.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExScrollBox.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExScrollBox.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExScrollBox.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExScrollBox.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExCustomFrame.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomFrame.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomFrame.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomFrame.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomFrame.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomFrame.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomFrame.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomFrame.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomFrame.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomFrame.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomFrame.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomFrame.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomFrame.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomFrame.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomFrame.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}
 {$IFDEF REINTRODUCE_HITTEST}
function TJvExCustomFrame.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
 {$ENDIF REINTRODUCE_HITTEST}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomFrame.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomFrame.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomFrame.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomFrame.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomFrame.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomFrame.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomFrame.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomFrame.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomFrame.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomFrame.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExCustomFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExCustomFrame.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExCustomFrame.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomFrame.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExFrame.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExFrame.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExFrame.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExFrame.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExFrame.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExFrame.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExFrame.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExFrame.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExFrame.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExFrame.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExFrame.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExFrame.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExFrame.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExFrame.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExFrame.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}
 {$IFDEF REINTRODUCE_HITTEST}
function TJvExFrame.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
 {$ENDIF REINTRODUCE_HITTEST}

 {$IF not declared(PatchedVCLX)}
procedure TJvExFrame.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExFrame.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExFrame.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExFrame.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExFrame.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExFrame.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExFrame.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExFrame.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExFrame.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExFrame.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
{$ENDIF VCL}
{$IFDEF VisualCLX}
constructor TJvExFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TJvExFrame.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

procedure TJvExFrame.Paint;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(QPainter_clipRegion(FCanvas.Handle), False);
  try
    QObject_event(Handle, Event);
  finally
    QPaintEvent_destroy(Event);
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExFrame.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExCustomForm.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExCustomForm.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExCustomForm.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExCustomForm.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExCustomForm.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExCustomForm.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExCustomForm.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExCustomForm.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExCustomForm.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExCustomForm.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExCustomForm.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExCustomForm.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomForm.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExCustomForm.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExCustomForm.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}
 {$IFDEF REINTRODUCE_HITTEST}
function TJvExCustomForm.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
 {$ENDIF REINTRODUCE_HITTEST}

 {$IF not declared(PatchedVCLX)}
procedure TJvExCustomForm.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExCustomForm.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExCustomForm.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExCustomForm.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExCustomForm.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExCustomForm.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExCustomForm.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExCustomForm.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExCustomForm.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExCustomForm.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExCustomForm.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvExForm.VisibleChanged;
begin
  InheritMsg(Self, CM_VISIBLECHANGED);
end;

procedure TJvExForm.EnabledChanged;
begin
  InheritMsg(Self, CM_ENABLEDCHANGED);
end;

procedure TJvExForm.TextChanged;
begin
  InheritMsg(Self, CM_TEXTCHANGED);
end;

procedure TJvExForm.FontChanged;
begin
  InheritMsg(Self, CM_FONTCHANGED);
end;

procedure TJvExForm.ColorChanged;
begin
  InheritMsg(Self, CM_COLORCHANGED);
end;

procedure TJvExForm.ParentColorChanged;
begin
  InheritMsg(Self, CM_PARENTCOLORCHANGED);
end;

procedure TJvExForm.ParentFontChanged;
begin
  InheritMsg(Self, CM_PARENTFONTCHANGED);
end;

procedure TJvExForm.ParentShowHintChanged;
begin
  InheritMsg(Self, CM_PARENTSHOWHINTCHANGED);
end;

function TJvExForm.WantKey(Key: Integer; Shift: TShiftState;
  const KeyText: WideString): Boolean;
begin
  Result := InheritMsg(Self, CM_DIALOGCHAR, Word(Key), ShiftStateToKeyData(Shift)) <> 0;
end;

function TJvExForm.HintShow(var HintInfo: THintInfo): Boolean;
begin
  Result := InheritMsg(Self, CM_HINTSHOW, 0, Integer(@HintInfo)) <> 0;
end;

function TJvExForm.HitTest(X, Y: Integer): Boolean;
begin
  Result := InheritMsg(Self, CM_HITTEST, 0, Integer(PointToSmallPoint(Point(X, Y)))) <> 0;
end;

procedure TJvExForm.MouseEnter(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSEENTER, 0, Integer(Control));
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExForm.MouseLeave(Control: TControl);
begin
  InheritMsg(Self, CM_MOUSELEAVE, 0, Integer(Control));
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF HASAUTOSIZE}
 {$IFNDEF COMPILER6_UP}
procedure TJvExForm.SetAutoSize(Value: Boolean);
begin
  TOpenControl_SetAutoSize(Self, Value); // do not call inherited here
end;
 {$ENDIF COMPILER6_UP}
{$ENDIF !HASAUTOSIZE}

{$ENDIF VCL}

function TJvExForm.DoPaintBackground(Canvas: TCanvas; Param: Integer): Boolean;
begin
  {$IFDEF VCL}
  Result := InheritMsg(Self, WM_ERASEBKGND, Canvas.Handle, Param) <> 0;
  {$ELSE}
  Result := False; // Qt allways paints the background
  {$ENDIF VCL}
end;

{$IFDEF VisualCLX}
 {$IFDEF REINTRODUCE_HITTEST}
function TJvExForm.HitTest(X, Y: Integer): Boolean;
begin
  Result := (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height);
end;
 {$ENDIF REINTRODUCE_HITTEST}

 {$IF not declared(PatchedVCLX)}
procedure TJvExForm.MouseEnter(Control: TControl);
begin
  inherited MouseEnter(Control);
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvExForm.MouseLeave(Control: TControl);
begin
  inherited MouseLeave(Control);
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;
 {$IFEND}
{$ENDIF VisualCLX}
{$IFDEF VisualCLX}
procedure TJvExForm.Painting(Sender: QObjectH; EventRegion: QRegionH);
begin
  if not (csDestroying in ComponentState) then
  begin
    ControlState := ControlState + [csWidgetPainting];
    try
      TControlCanvas(Canvas).StartPaint;
      try
        QPainter_setClipRegion(Canvas.Handle, EventRegion);
        DoPaintBackground(Canvas, 0);
        Paint;
      finally
        TControlCanvas(Canvas).StopPaint;
      end;
    finally
      ControlState := ControlState - [csWidgetPainting];
    end;
  end;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvExForm.CursorChanged;
begin
  InheritMsg(Self, CM_CURSORCHANGED);
end;

procedure TJvExForm.ShowHintChanged;
begin
  InheritMsg(Self, CM_SHOWHINTCHANGED);
end;

procedure TJvExForm.ShowingChanged;
begin
  InheritMsg(Self, CM_SHOWINGCHANGED);
end;

procedure TJvExForm.ControlsListChanging(Control: TControl; Inserting: Boolean);
begin
  if Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

procedure TJvExForm.ControlsListChanged(Control: TControl; Inserting: Boolean);
begin
  if not Inserting then
    InheritMsg(Self, CM_CONTROLLISTCHANGE, Integer(Control), Integer(Inserting))
  else
    InheritMsg(Self, CM_CONTROLCHANGE, Integer(Control), Integer(Inserting))
end;

{$IFDEF JVCLThemesEnabledD56}
function TJvExForm.GetParentBackground: Boolean;
begin
  Result := JvThemes.GetParentBackground(Self);
end;

procedure TJvExForm.SetParentBackground(Value: Boolean);
begin
  JvThemes.SetParentBackground(Self, Value);
end;
{$ENDIF JVCLThemesEnabledD56}
procedure TJvExForm.Dispatch(var Msg);
begin
  DispatchMsg(Self, Msg);
end;
{$ENDIF VCL}

end.
