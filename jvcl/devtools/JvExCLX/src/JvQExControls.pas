{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQExControls.pas, released on 2004-09-21

The Initial Developer of the Original Code is André Snepvangers [ASnepvangers att xs4all dot nl]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s): Marcel Besteboer

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http:{jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQExControls;

{$I jvcl.inc}
{MACROINCLUDE JvQExControls.macros}

WARNINGHEADER

interface

uses
  Classes, SysUtils,
  QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QMask, QClipbrd, TypInfo,
  Qt, QWindows, QMessages,
  JvQTypes, JvQThemes, JVCLXVer;

type
  HWND = QWindows.HWND;
  {$EXTERNALSYM HWND}
  HDC = QWindows.HDC;
  {$EXTERNALSYM HDC}
  TJvMessage = JvTypes.TJvMessage;
  {$EXTERNALSYM TJvMessage}

  { Add IJvDenySubClassing to the base class list if the control should not
    be themed by the ThemeManager (www.delphi-gems.de).
    This only works with JvExVCL derived classes. }
  IJvDenySubClassing = interface
    ['{76942BC0-2A6E-4DC4-BFC9-8E110DB7F601}']
  end;

  TDragKind = (dkDrag, dkDock);   { not implemented yet}

  TAlignInfo = record
    AlignList: TList;
    ControlIndex: Integer;
    Align: TAlign;
    Scratch: Integer;
  end;

const
  CM_DENYSUBCLASSING = JvQThemes.CM_DENYSUBCLASSING;

type
  JV_CONTROL(Control)
  JV_WINCONTROL(WinControl)

  JV_CONTROL_BEGIN(GraphicControl)
  private
    FText: TCaption; { TControl does not save the Caption property }
  protected
    procedure PaintRequest; override;
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
  public
    function ColorToRGB(Value: TColor): TColor;
  JV_CONTROL_END(GraphicControl)

  JV_CUSTOMCONTROL(CustomControl)
  JV_WINCONTROL(FrameControl)
  JV_CUSTOMCONTROL(HintWindow)

function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): Boolean;
function GetCanvas(Instance: TWinControl): TCanvas;
function GetFocusedControl(Instance: TControl): TWinControl;
function GetFocusedWnd(Instance: TControl): QWidgetH;
function GetHintColor(Instance: TControl): TColor;
function InputKeysToDlgCodes(InputKeys: TInputKeys): Integer;
function IsDoubleBuffered(Instance: TWinControl): Boolean;
function IsPaintingCopy(Instance: TWinControl): Boolean;
function SendAppMessage(Msg: Cardinal; WParam, LParam: Integer): Integer;
procedure WidgetControl_PaintTo(Instance: TWidgetControl; PaintDevice: QPaintDeviceH; X, Y: Integer);

var
  NewStyleControls: Boolean;

implementation


function SendAppMessage(Msg: Cardinal; WParam, LParam: Integer): Integer;
begin
  Result := SendMessage(Application.AppWidget, Msg, WParam, LParam);
end;

function GetCanvas(Instance: TWinControl): TCanvas;
var
  PI: PPropInfo;
begin
  Result := nil;
  if Assigned(Instance) then
  begin
    PI := GetPropInfo(Instance, 'Canvas');
    if PI <> nil then
      Result := TCanvas(GetOrdProp(Instance, PI));
  end;
end;

function IsDoubleBuffered(Instance: TWinControl): Boolean;
var
  PI: PPropInfo;
begin
  Result := True;
  if Assigned(Instance) then
  begin
    PI := GetPropInfo(Instance, 'DoubleBuffered');
    if PI <> nil then
      Result := GetEnumProp(Instance, PI) <> 0;
  end;
end;

function GetHintColor(Instance: TControl): TColor;
var
  PI: PPropInfo;
begin
  Result := clDefault;
  while (Result = clDefault) and Assigned(Instance) do
  begin
    PI := GetPropInfo(Instance, 'HintColor');
    if PI <> nil then
      Result := TColor(GetOrdProp(Instance, PI));
    Instance := Instance.Parent;
  end;
  case Result of
  clNone, clDefault: Result := Application.HintColor;
  end;
end;

function IsPaintingCopy(Instance: TWinControl): Boolean;
begin
  Result := false ;
  while not Result and Assigned(Instance) do
  begin
    Result := csPaintCopy in Instance.ControlState ;
    Instance := Instance.Parent;
  end;
end;

function DoClipBoardCommands(Msg: Integer; ClipBoardCommands: TJvClipBoardCommands): Boolean;
begin
  case Msg of
    WM_COPY          : Result := caCopy in ClipBoardCommands;
    WM_CUT           : Result := caCut in ClipBoardCommands;
    WM_PASTE         : Result := caPaste in ClipBoardCommands;
    WM_UNDO          : Result := caUndo in ClipBoardCommands;
  else
    Result := False;
  end;
end;

function GetFocusedControl(Instance: TControl): TWidgetControl;
var
  Form: TCustomForm;
begin
  Result := nil;
  Form := GetParentForm(Instance);
  if Assigned(Form) then
    Result := Form.FocusedControl;
end;

function GetFocusedWnd(Instance: TControl): QWidgetH;
var
  Control: TWidgetControl;
begin
  Result := nil;
  Control := GetFocusedControl(Instance);
  if Assigned(Control) then
    Result := Control.Handle ;
end;

function InputKeysToDlgCodes(InputKeys: TInputKeys): Integer;
begin
  Result := 0;
  if ikAll in InputKeys then
    inc(Result, DLGC_WANTALLKEYS);
  if ikArrows in InputKeys then
    inc(Result, DLGC_WANTARROWS);
  if ikChars in InputKeys then
    inc(Result, DLGC_WANTCHARS);
  if ikEdit in InputKeys then
    inc(Result, DLGC_HASSETSEL);
  if ikTabs in InputKeys then
    inc(Result, DLGC_WANTTAB);
  if ikButton in InputKeys then
    inc(Result, DLGC_BUTTON);
end;

procedure WidgetControl_PaintTo(Instance: TWidgetControl; PaintDevice: QPaintDeviceH; X, Y: Integer);
var
  PixMap: QPixmapH;
begin
  PixMap := QPixmap_create;
  with Instance do
    try
      ControlState := ControlState + [csPaintCopy];
      QPixmap_grabWidget(PixMap, Handle, 0, 0, Width, Height);
      Qt.BitBlt(PaintDevice, X, Y, PixMap, 0, 0, Width, Height, RasterOp_CopyROP, True);
    finally
      ControlState := ControlState - [csPaintCopy];
      QPixMap_destroy(PixMap);
    end;
end;

type
  TJvCLXFilters = class(TComponent)
  private
    FHook: QObject_hookH;
  protected
    function EventFilter(Receiver: QObjectH; Event: QEventH): Boolean; cdecl;
  public
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

constructor TJvCLXFilters.Create(AOwner: TComponent);
var
  Method: TMethod;
begin
  inherited Create(AOwner);
  FHook := QObject_hook_create(Application.Handle);
  TEventFilterMethod(Method) := EventFilter;
  Qt_hook_hook_events(FHook, Method);
end;

destructor TJvCLXFilters.Destroy;
begin
  if Assigned(FHook) then
    QObject_hook_destroy(FHook);
  inherited Destroy;
end;

{
  - Handles Qt Events
}
function TJvCLXFilters.EventFilter(Receiver: QObjectH; Event: QEventH): Boolean;
var
  PixMap: QPixmapH;
  Mesg: TMessage;
  R: TRect;
  Canvas: TCanvas;
  Instance: TWidgetControl;

begin
  Instance := FindControl(QWidgetH(Receiver));
  if not Assigned(Instance) then
    Exit;
  with Instance do
    case QEvent_type(Event) of

    QEventType_ApplicationPaletteChange : PostMessage(Instance, CM_SYSCOLORCHANGE, 0, 0);
    QEventType_ApplicationFontChange    : PostMessage(Instance, CM_SYSFONTCHANGED, 0, 0);
    QEventType_Enter                    : Perform(Instance, CM_MOUSEENTER, 0, 0);
    QEventType_Leave                    : Perform(Instance, CM_MOUSELEAVE, 0, 0);

    QEventType_Paint:
    begin
      with Event as QPaintEventH do
      begin
        if (csDestroying in ComponentState) or
          ([csCreating, csRecreating]* Instance.ControlState <> []) then
        begin
          Result := True;
          Exit;
        end;

        if not (csWidgetPainting in ControlState) then
          if not IsPaintingCopy(Instance) then
          begin
            QRegion_boundingRect(QPaintEvent_region(QPaintEventH(Event)), @R);
            Pixmap := QPixmap_create ;
            try
              ControlState := ControlState + [csPaintCopy];
              QPixmap_grabWidget(PixMap, Handle, R.Left, R.Top,
                R.Right - R.Left, R.Bottom - R.Top);
              Qt.BitBlt(QWidget_to_QPaintDevice(Handle), R.Left, R.Top, PixMap,
                   0, 0, R.Right - R.Left, R.Bottom - R.Top, RasterOp_CopyROP, False);
              Result := True;
            finally
              ControlState := ControlState - [csPaintCopy];
              QPixMap_destroy(PixMap);
            end;
          end
        else
        begin
        { csWidgetPainting / Erasebackground }
          with TJvMessage(Mesg) do
          begin
            Msg := WM_ERASEBKGND;
            Canvas := GetCanvas(Instance);
            try
              if Assigned(Canvas) then
              begin
                Canvas.Start;
                DC := Canvas.Handle;
              end
              else
                WParam := 0;
              LParam := 0;
              Handled := False;
              Dispatch(Mesg);
            finally
              if Assigned(Canvas) then
                Canvas.Stop;
            end;
          end;
          Result := Mesg.Result <> 0 ;
        end;
      end;  // with Event as QPaintEventH
    end;
  end;
end;

JV_CONTROL_IMPL(Control)
JV_WINCONTROL_IMPL(WinControl)
JV_CUSTOMCONTROL_IMPL(CustomControl)
JV_WINCONTROL_IMPL(FrameControl)
JV_CUSTOMCONTROL_IMPL(HintWindow)
JV_CONTROL_IMPL(GraphicControl)

function TJvExGraphicControl.GetText: TCaption;
begin
  Result := FText;
end;

procedure TJvExGraphicControl.SetText(const Value: TCaption);
begin
  if Value <> FText then
  begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TJvExGraphicControl.PaintRequest;
begin
  if not Assigned(Parent) then
    Exit;
  Canvas.Start;
  try
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Assign(Font);
    RequiredState(Canvas, [csHandleValid, csFontValid, csBrushValid]);
    inherited PaintRequest;
  finally
    Canvas.Stop;
  end;
end;

function TJvExGraphicControl.ColorToRGB(Value: TColor): TColor;
begin
  Result := QWindows.ColorToRGB(Value, Parent);
end;

UNITVERSION

end.

