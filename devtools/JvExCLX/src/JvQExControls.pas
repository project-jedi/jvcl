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

implementation

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

end.

