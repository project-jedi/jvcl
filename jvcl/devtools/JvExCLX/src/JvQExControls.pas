{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQExControls.pas, released on 2004-09-21

The Initial Developer of the Original Code is André Snepvangers [ASnepvangers att users.sourceforge.net]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s):

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
  QGraphics, QControls, QForms, QStdCtrls, QExtCtrls, QMask, QClipbrd,
  Qt, QWindows, QMessages,
  JvQTypes, JvQThemes, JVCLXVer;

type
  HWND = QWindows.HWND;
  TClxWindowProc = procedure(var Msg: TMessage) of object;

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

  TDlgCode = (ikAll..ikEsc, ikDefault);
  TDlgCodes = set of TDlgCode;

  dcButton = ikTabs | ikArrows | ikReturns ;

  { historical names }
  dcWantAllKeys = ikAll;
  dcWantArrows = ikArrows;
  dcWantTab = ikTabs;
  dcWantChars = ikChars;
  dcWantReturns = ikReturns;
  dcWantEdit = ikEdit;
  dcWantNav = ikNav;
  dcWantEscape = ikEsc;
  dcNative = ikDefault;
  dcWantMessage = dcWantAllKeys;

const
  CM_DENYSUBCLASSING = JvQThemes.CM_DENYSUBCLASSING;

type
  JV_CONTROL(Control)
  JV_WINCONTROL(WinControl)

  JV_CONTROL_BEGIN(GraphicControl)
  private
    FText: TCaption; { TControl does not save the Caption property }
  protected
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
  public
    function ColorToRGB(Value: TColor): TColor;
  JV_CONTROL_END(GraphicControl)

  JV_CUSTOMCONTROL(CustomControl)
  JV_WINCONTROL(FrameControl)
  JV_CUSTOMCONTROL(HintWindow)

function GetHintColor(Instance: TWinControl): TColor;
function SendAppMessage(Msg: Cardinal; WParam, LParam: Integer): Integer;
procedure WidgetControl_PaintTo(Instance: TWidgetControl; PaintDevice: QPaintDeviceH; X, Y: Integer);

var
  NewStyleControls: Boolean;

implementation

function SendAppMessage(Msg: Cardinal; WParam, LParam: Integer): Integer;
begin
  Result := SendMessage(Application.AppWidget, Msg, WParam, LParam);
end;

function GetHintColor(Instance: TWinControl): TColor;
{
clNone         : Application.HintColor
clDefaultColor : Parent HintColor
}
var
  PI: PPropInfo;
begin
  Result := clDefaultColor;
  while (Result = clDefaultColor) and (Instance <> nil) do
  begin
    PI := GetPropInfo(Instance, 'HintColor');
    if PI <> nil then
      Result := TColor(GetOrdProp(Instance, PI));
    Instance := Instance.Parent;
  end;
  case Result of
  clNone, clDefaultColor: Result := Application.HintColor;
  end;
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
    end;
    finally
      ControlState := ControlState - [csPaintCopy];
      QPixMap_destroy(PixMap);
    end;
  end;
end;

type
  TJvCLXFilters = class(TComponent)
  private
    FHook: QObject_hookH;
  protected
    function EventFilter(Receiver: QObjectH; Event: QEventH): Boolean;
  public
    constructor Create(AOwner: ; override
    destructor Destroy; override;
  end;

constructor TJvCLXFilters.Create(AOwner: TComponent);
var
  Method: TMethod;
begin
  inherited Create(AOwner);
  FHook := QObject_hook_create(FHandle);
  TEventFilterMethod(Method) := EventFilter;
  Qt_hook_hook_events(FHook, Method);
end;

destructor TJvCLXFilters.Destroy;
begin
  QObject_hook_destroy(FHook);
  inherited Destroy;
end;

function TJvCLXFilters.EventFilter(Instance: TWidgetControl; Event: QEventH): Boolean;
var
  PixMap: QPixmapH;
  WinTimer: TWinTimer;
  Mesg: TMsg;
  R: TRect;
begin
  { implements doublebuffered paint & WM_ERASEBKGND
    Handles/implements HintColor
  }

  with Instance do case QEvent_type(Event) of

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
        if not (csPaintCopy in ControlState) then
        begin
          QRegion_boundingRect(QPaintEvent_region(Event), @R);
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
        { csWidgetPainting / Paintbackground }
        Mesg.hwnd := Handle;
        Mesg.Msg := WM_ERASEBKGND;
        Mesg.DC := 0;
        Mesg.LParam := 0;
        Mesg.Handled := 0;
        Dispatch(Mesg);
        Result := Mesg.Handled;
      end;
    end;  // with Event as QPaintEventH
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
    RequiredState(Canvas, [csHandleNeeded, csFontValid, csBrushValid]);
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

