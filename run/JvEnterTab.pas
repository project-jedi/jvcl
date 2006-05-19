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

Description:
  A unit that converts all Enter keypresses to Tab keypresses.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvEnterTab;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, Classes, Graphics, Controls,
  {$IFDEF VisualCLX}
  Qt, JvQConsts,
  {$ENDIF VisualCLX}
  JvComponent;

type
  TJvEnterAsTabEvent = procedure (Sender:TObject; AControl:TWinControl; var Handled:Boolean) of object;
  TJvEnterAsTab = class(TJvGraphicControl)
  private
    FEnterAsTab: Boolean;
    FAllowDefault: Boolean;
    FBmp: TBitmap;
    FOnHandleEnter: TJvEnterAsTabEvent;
  protected
    {$IFDEF VCL}
    function EnterHandled(AControl: TWinControl): Boolean;virtual;
    procedure CMDialogKey(var Msg: TCMDialogKey); message CM_DIALOGKEY;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    function EnterHandled(AControl: QObjectH): Boolean;virtual;
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
    // Assign a handler if you want to specify when the Enter key is not to be converted into a
    // Tab key. Only triggered if AllowDefault is true. If no event handler is assigned,
    // Enter keys will not be converted into Tab if the currently active control is a
    // TbuttonControl descendant
    property OnHandleEnter:TJvEnterAsTabEvent read FOnHandleEnter write FOnHandleEnter;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF CLR}
  Borland.Vcl.WinUtils,
  {$ENDIF CLR}
  Forms, StdCtrls;

{$R JvEnterTab.res}

constructor TJvEnterAsTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoStdEvents, csFixedHeight, csFixedWidth];
  FEnterAsTab := True;
  FAllowDefault := True;
  if csDesigning in ComponentState then
  begin
    FBmp := TBitmap.Create;
    FBmp.LoadFromResourceName(HInstance, 'DESIGNENTERASTAB');
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
function TJvEnterAsTab.EnterHandled(AControl:TWinControl):Boolean;
begin
  Result := AControl is TButtonControl;
  if Assigned(FOnHandleEnter) then
    FOnHandleEnter(Self, AControl, Result);
end;

procedure TJvEnterAsTab.CMDialogKey(var Msg: TCMDialogKey);
begin
  if (Msg.CharCode = VK_RETURN) and EnterAsTab then
  begin
    if AllowDefault and EnterHandled(GetParentForm(Self).ActiveControl) then
      inherited
    else
    begin
      GetParentForm(Self).Perform(CM_DIALOGKEY, VK_TAB, 0);
      Msg.Result := 1;
    end;
  end
  else
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
function TJvEnterAsTab.EnterHandled(AControl:QObjectH):Boolean;
begin
  Result := QObject_inherits(AControl, 'QButton');
  if Assigned(FOnHandleEnter) then
    FOnHandleEnter(Self, FindObject(AControl) as TWinControl, Result);
end;

function TJvEnterAsTab.TabKeyHook(Sender: QObjectH; Event: QEventH): Boolean;
var
  ws: WideString;
begin
  Result := False;
  if QEvent_type(Event) = QEventType_KeyPress then
  begin
    if ((QKeyEvent_key(QKeyEventH(Event)) = Key_Enter) or
      (QKeyEvent_key(QKeyEventH(Event)) = Key_Return) ) and EnterAsTab then
    begin
      if AllowDefault and EnterHandled(Sender) then
        Exit;
      ws := Tab;

      QApplication_postEvent(GetParentForm(Self).Handle,
        QKeyEvent_create(QEventType_KeyPress, Key_Tab, Ord(Tab), 0, @ws, False, 1));
      QApplication_postEvent(GetParentForm(Self).Handle,
        QKeyEvent_create(QEventType_KeyRelease, Key_Tab, Ord(Tab), 0, @ws, False, 1));

      Result := True;
    end;
  end;
end;
{$ENDIF VisualCLX}

procedure TJvEnterAsTab.Paint;
begin
  if not (csDesigning in ComponentState) then
    Exit;
  with Canvas do
  begin
    Brush.Color := clBtnFace;
    BrushCopy({$IFDEF VisualCLX} Canvas, {$ENDIF} ClientRect, FBmp, ClientRect, clFuchsia);
  end;
end;

procedure TJvEnterAsTab.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, 28, 28);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

