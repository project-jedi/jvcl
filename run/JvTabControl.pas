{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTabControl.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Below <100113.1101@compuserve.com>

Last Modified: 2002-06-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTabControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  JVCLVer, JvExComCtrls;

type
  TJvTabControl = class(TJvExTabControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FHintColor: TColor;
    FSaved: TColor;
    FOnParentColorChanged: TNotifyEvent;
    procedure CMDialogKey(var Msg: TWMKey); message CM_DIALOGKEY; // not WantKeys
  protected
    procedure MouseEnter(Control: TControl); override;
    procedure MouseLeave(Control: TControl); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property Color;
  end;

implementation

constructor TJvTabControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
end;

procedure TJvTabControl.ParentColorChanged;
begin
  inherited ParentColorChanged;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvTabControl.MouseEnter(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  FSaved := Application.HintColor;
  Application.HintColor := FHintColor;
  inherited MouseEnter(Control);
end;

procedure TJvTabControl.MouseLeave(Control: TControl);
begin
  if csDesigning in ComponentState then
    Exit;
  Application.HintColor := FSaved;
  inherited MouseLeave(Control);
end;

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

end.

