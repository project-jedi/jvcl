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

unit JvAlignedEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, JVCLVer;

type
  TJvAlignedEdit = class(TEdit)
  private
    fAlignment: TAlignment;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetAlignment(const Value: TAlignment);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMChar(var Message: TMessage); message WM_CHAR;
  protected
    { Protected declarations }
    procedure CreateParams(var params: TCreateParams); override;
  public
    { Public declarations }
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Alignment: TAlignment read fAlignment write SetAlignment
      default taLeftJustify;
  end;

implementation

{ TJvAlignedEdit }

procedure TJvAlignedEdit.CreateParams(var params: TCreateParams);
const
  Styles: array[TAlignment] of DWORD =
  (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited;
  params.style := params.style or Styles[Falignment] or
    ES_MULTILINE * DWORD(Ord(FAlignment <> taLeftJustify));
end;

procedure TJvAlignedEdit.SetAlignment(const Value: TAlignment);
begin
  if fAlignment <> Value then
  begin
    fAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TJvAlignedEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TJvAlignedEdit.WMChar(var Message: TMessage);
var
  f: TForm;
begin
  if Message.wparam = 13 then
  begin
    f := TForm(GetParentForm(self));
    if Assigned(f) then
      f.Perform(CM_DIALOGKEY, VK_RETURN, message.lparam);
  end
  else
    inherited;
end;

end.
