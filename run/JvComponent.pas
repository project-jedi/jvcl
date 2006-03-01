{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComponent.PAS, released on 2000-09-22.

The Initial Developer of the Original Code is Joe Doe .
Portions created by Joe Doe are Copyright (C) 1999 Joe Doe.
Portions created by XXXX Corp. are Copyright (C) 1998, 1999 XXXX Corp.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvComponent;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF USE_DXGETTEXT}
  JvGnugettext,
  {$ENDIF USE_DXGETTEXT}
  Windows, Messages, Controls, Forms,
  {$IFDEF VisualCLX}
  Qt, QGraphics, QStdCtrls, QForms, // TOwnerDrawState
  {$ENDIF VisualCLX}
  JvConsts,
  JVCLVer, JvComponentBase, JvExControls, JvExExtCtrls, JvExForms,
  JvExStdCtrls, JvExComCtrls;

{$IFDEF VisualCLX}
type
  HDC = QWindows.HDC;
  {$NODEFINE HDC}
  TMessage = QWindows.TMessage;
  {$NODEFINE TMessage}
  TMsg = QWindows.TMsg;
  {$NODEFINE TMsg}
  TOwnerDrawState = QStdCtrls.TOwnerDrawState;
  {$NODEFINE TOwnerDrawState}
  //TBevelKind = JvQExControls.TBevelKind;
  //{$NODEFINE TBevelKind}
  function ColorToRGB(Color: TColor; Instance: TWidgetControl = nil): TColor;
  function DrawEdge(Handle: QPainterH; var Rect: TRect; Edge: Cardinal;
    Flags: Cardinal): LongBool;
{$ENDIF VisualCLX}

type
  TJvGraphicControl = class(TJvExGraphicControl);
  TJvPubGraphicControl = class(TJvExPubGraphicControl);
  TJvCustomPanel = class(TJvExCustomPanel);
  TJvCustomControl = class(TJvExCustomControl);
  TJvWinControl = class(TJvExWinControl);
  TJvPubCustomPanel = class(TJvExPubCustomPanel);
  TJvCustomTreeView = class(TJvExCustomTreeView);

  TJvForm = class(TJvExForm)
  {$IFDEF VCL}
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  {$ENDIF VCL}
  {$IFDEF USE_DXGETTEXT}
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshTranslation; virtual;
  {$ENDIF USE_DXGETTEXT}
  end;

//=== { TJvPopupListBox } ====================================================

type
  TJvPopupListBox = class(TJvExCustomListBox)
  private
    FSearchText: string;
    FSearchTickCount: Longint;
  protected
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    procedure CreateWnd; override;
    {$IFDEF VisualCLX}
    function WidgetFlags: Integer; override;
    {$ENDIF VisualCLX}
    procedure KeyPress(var Key: Char); override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}  // For TJvForm, see Issue 3537.

{$IFDEF USE_DXGETTEXT}
const
  cDomainName = 'jvcl';
{$ENDIF USE_DXGETTEXT}


{$IFDEF VisualCLX}
function ColorToRGB(Color: TColor; Instance: TWidgetControl = nil): TColor;
begin
  Result :=  QWindows.ColorToRGB(Color, Instance);
end;

function DrawEdge(Handle: QPainterH; var Rect: TRect; Edge: Cardinal;
  Flags: Cardinal): LongBool;
begin
  Result := QWindows.DrawEdge(Handle, Rect, Edge, Flags);
end;
{$ENDIF VisualCLX}

//=== { TJvForm } ============================================================

{$IFDEF USE_DXGETTEXT}

constructor TJvForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TranslateComponent(Self, cDomainName);
end;

procedure TJvForm.RefreshTranslation;
begin
  ReTranslateComponent(Self, cDomainName);
end;

{$ENDIF USE_DXGETTEXT}

{$IFDEF VCL}

procedure TJvForm.CreateParams(var Params: TCreateParams); //override;
begin
  inherited CreateParams(Params);

  if FormStyle <> fsMDIChild then
  begin
    // Fixing the Window Ghosting "bug"
    Params.Style := params.Style or WS_POPUP;
    if Assigned(Screen.ActiveForm) then
      Params.WndParent := Screen.ActiveForm.Handle
    else if Assigned (Application.MainForm) then
      Params.WndParent := Application.MainForm.Handle
    else
      Params.WndParent := Application.Handle;
  end;
end;

{$ENDIF VCL}

//=== { TJvPopupListBox } ====================================================

{$IFDEF VCL}

procedure TJvPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

{$ENDIF VCL}

procedure TJvPopupListBox.CreateWnd;
begin
  inherited CreateWnd;
  {$IFDEF VCL}
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QWidget_setFocus(Handle);
  {$ENDIF VisualCLX}
end;

{$IFDEF VisualCLX}

function TJvPopupListBox.WidgetFlags: Integer;
begin
  Result :=
    Integer(WidgetFlags_WType_Popup) or         // WS_POPUPWINDOW
    Integer(WidgetFlags_WStyle_NormalBorder) or // WS_BORDER
    Integer(WidgetFlags_WStyle_Tool) or         // WS_EX_TOOLWINDOW
    Integer(WidgetFlags_WStyle_StaysOnTop);     // WS_EX_TOPMOST
end;

{$ENDIF VisualCLX}

procedure TJvPopupListBox.KeyPress(var Key: Char);
var
  TickCount: Int64;
begin
  case Key of
    BackSpace, Esc:
      FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount < FSearchTickCount then
          Inc(TickCount, $100000000); // (ahuser) reduces the overflow
        if TickCount - FSearchTickCount >= 4000 then
          FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then
          FSearchText := FSearchText + Key;
        {$IFNDEF CLR}
        {$IFDEF VCL}
        SendMessage(Handle, LB_SELECTSTRING, WPARAM(-1), LPARAM(PChar(FSearchText)));
        {$ENDIF VCL}
        {$ELSE}
        SendTextMessage(Handle, LB_SELECTSTRING, WPARAM(-1), FSearchText);
        {$ENDIF !CLR}
        Key := #0;
      end;
  end;
  inherited KeyPress(Key);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_DXGETTEXT}
  AddDomainForResourceString(cDomainName);
  {$ENDIF USE_DXGETTEXT}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
