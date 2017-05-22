{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTabControl.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Below <100113 dott 1101 att compuserve dott com>

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvTabControl;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QForms, QComCtrls, QWindows,
  {$ENDIF VisualCLX}
  JvExComCtrls;

type
  TJvTabControl = class(TJvExTabControl)
  private
    {$IFDEF VCL}
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY; // not WantKeys
    {$ENDIF VCL}
  protected
    {$IFDEF VisualCLX}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override ;
    {$ENDIF VisualCLX}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Color;
  end;

implementation

constructor TJvTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF VisualCLX}
  InputKeys := [ikTabs];
  {$ENDIF VisualCLX}
end;

{$IFDEF VCL}
procedure TJvTabControl.CMDialogKey(var Msg: TWMKey);
begin
  if (Msg.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) and
    IsChild(Handle, Windows.GetFocus) then
  begin
    if GetKeyState(VK_SHIFT) < 0 then
    begin
      if TabIndex = 0 then
        TabIndex := Tabs.Count - 1
      else
        TabIndex := TabIndex - 1;
    end
    else
      TabIndex := (TabIndex + 1) mod Tabs.Count;
    Msg.Result := 1;
  end
  else
    inherited;
end;
{$ENDIF VCL}

{$IFDEF VisualCLX}
procedure TJvTabControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) and (ssCtrl in Shift) then
  begin
    if ssShift in Shift then
    begin
      if TabIndex = 0 then
        TabIndex := Tabs.Count - 1
      else
        TabIndex := TabIndex - 1;
    end
    else
      TabIndex := (TabIndex + 1) mod Tabs.Count;
    Key := 0 ;
  end
  else
    inherited KeyDown(Key, Shift);
end;
{$ENDIF VisualCLX}

end.

