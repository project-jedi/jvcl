{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAlignListbox.PAS, released on 2000-11-22.

The Initial Developer of the Original Code is Peter Below <100113.1101@compuserve.com>
Portions created by Peter Below are Copyright (C) 2000 Peter Below.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2000-mm-dd

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvFixedCheckListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, CheckLst, JVCLVer;

type
  TJvFixedCheckListBox = class(TCheckListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    { Private declarations }
    procedure WMHScroll(var msg: TWMHScroll); message WM_HSCROLL;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

implementation

{ TJvFixedCheckListBox }

procedure TJvFixedCheckListBox.WMHScroll(var msg: TWMHScroll);
var
  scrollpos: Integer;
  r: TRect;
begin
  inherited;
  if msg.ScrollCode <> SB_ENDSCROLL then
  begin
    scrollpos := GetScrollPos(handle, SB_HORZ);
    if scrollpos < 20 then
    begin
      r := ClientRect;
      r.Right := r.left + 20;
      InvalidateRect(handle, @r, false);
    end;
  end;
end;

end.
