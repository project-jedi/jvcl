{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvEnterTab.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            
You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

{ A unit that converts all Enter keypresses to Tab keypresses. }

unit JvEnterTab;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, StdCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Qt, QGraphics, QControls, QForms, QWindows,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvEnterAsTab = class(TJvGraphicControl)
  private
    FEnterAsTab: Boolean;
    FAllowDefault: Boolean;
    FBmp: TBitmap;
  protected
    {$IFDEF VCL}
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function TabKeyHook(Sender: QObjectH; Event: QEventH): Boolean; virtual;
    {$ENDIF VisualCLX}
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property EnterAsTab: Boolean read FEnterAsTab write FEnterAsTab default True;
    property AllowDefault: Boolean read FAllowDefault write FAllowDefault default True;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvEnterTab.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvEnterTab.res}
{$ENDIF LINUX}

constructor TJvEnterAsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoStdEvents, csFixedHeight, csFixedWidth];
  FEnterAsTab := True;
  FAllowDefault := True;
  if csDesigning in ComponentState then
  begin
    FBmp := TBitmap.Create;
    FBmp.LoadFromResourceName(hInstance, 'DESIGNENTERASTAB');
  end
  else
    Visible := False;
  {$IFDEF VisualCLX}
  InstallApplicationHook(TabKeyHook);
  {$ENDIF VisualCLX}
end;

destructor TJvEnterAsTab.Destroy;
begin
  {$IFDEF VisualCLX}
  UninstallApplicationHook(TabKeyHook);
  {$ENDIF VisualCLX}
  FBmp.Free;
  inherited Destroy;
end;

{$IFDEF VCL}
procedure TJvEnterAsTab.CMDialogKey(var Msg: TCMDialogKey);
begin
  if (GetParentForm(Self).ActiveControl is TButtonControl) and AllowDefault then
    inherited
  else
  if (Msg.CharCode = VK_RETURN) and EnterAsTab then
  begin
    GetParentForm(Self).Perform(CM_DIALOGKEY, VK_TAB, 0);
    Msg.Result := 1;
  end
  else
    inherited;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
function TJvEnterAsTab.TabKeyHook(Sender: QObjectH; Event: QEventH): Boolean;
var
  ws: WideString;
begin
  Result := False;
  if QEvent_type(Event) = QEventType_KeyPress then
  begin
    if QObject_inherits(Sender, 'QButton') and AllowDefault then
      Exit;

    if ((QKeyEvent_key(QKeyEventH(Event)) = Key_Enter) or
        (QKeyEvent_key(QKeyEventH(Event)) = Key_Return) ) and EnterAsTab then
    begin
      ws := #9;

      QApplication_postEvent(GetParentForm(Self).Handle,
        QKeyEvent_create(QEventType_KeyPress, Key_Tab, 9, 0, @ws, False, 1));
      QApplication_postEvent(GetParentForm(Self).Handle,
        QKeyEvent_create(QEventType_KeyRelease, Key_Tab, 9, 0, @ws, False, 1));

      Result := True;
    end;
  end;
end;
{$ENDIF VisualCLX}

procedure TJvEnterAsTab.Paint;
begin
  if not (csDesigning in ComponentState) then
    Exit;
  Canvas.Brush.Color := clBtnFace;
  {$IFDEF VCL}
  Canvas.BrushCopy(ClientRect, FBmp, ClientRect, clFuchsia);
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  FBmp.Transparent := True;
  FBmp.TransparentColor := clFuchsia;
  Canvas.StretchDraw(ClientRect, FBmp);
  {$ENDIF VisualCLX}
end;

procedure TJvEnterAsTab.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, 28, 28);
end;

end.

