{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

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

{$I jvcl.inc}

unit JvQTabControl;

interface

uses
  SysUtils, Classes,
  
  
  Types, QGraphics, QControls, QForms, QComCtrls, QWindows,
  
  JvQExComCtrls;

type
  TJvTabControl = class(TJvExTabControl)
  private
    
  protected
    
    procedure KeyDown(var Key: Word; Shift: TShiftState); override ;
    
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
  
  InputKeys := [ikTabs];
  
end;




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


end.

