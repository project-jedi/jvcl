{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTransForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTransForm;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, TypInfo, JvComponent;

type
  TJvTransForm = class(TJvComponent)
  private
    FActive: Boolean;
    FForm: TCustomForm;
    procedure SetActive(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
  end;

implementation

{**************************************************}

constructor TJvTransForm.Create(AOwner: TComponent);
begin
  inherited;
  FActive := False;
  FForm := GetParentForm(TControl(AOwner));
  SetActive(FActive);
end;

{**************************************************}

procedure TJvTransForm.SetActive(const Value: Boolean);
var
  i: Integer;
  PropInfo: PPropInfo;
  c: TComponent;
  t: Trect;
  r, s: Hrgn;
  cl: TRect;
begin
  FActive := Value;
  if csDesigning in ComponentState then Exit; // should really not be hidden at design-time ???
  cl := FForm.ClientRect;
  if FActive then
  begin
    r := CreateRectRgn(0, 0, FForm.Width, FForm.Height - cl.Bottom - 4);
    cl.Top := FForm.Height - cl.Bottom;
    cl.Left := FForm.Width - cl.Right;
    for i := 0 to FForm.ComponentCount - 1 do
    begin
      c := FForm.Components[i];
      PropInfo := GetPropInfo(c.ClassInfo, 'Left');
      if PropInfo <> nil then
      begin
        t.Left := GetOrdProp(c, PropInfo) + cl.Left - 4;
        PropInfo := GetPropInfo(c.ClassInfo, 'Top');
        t.Top := GetOrdProp(c, PropInfo) + cl.Top - 4;
        PropInfo := GetPropInfo(c.ClassInfo, 'Width');
        t.Right := GetOrdProp(c, PropInfo) + t.Left + 2;
        PropInfo := GetPropInfo(c.ClassInfo, 'Height');
        t.Bottom := GetOrdProp(c, PropInfo) + t.Top + 2;
        s := CreateRectRgn(t.Left, t.Top, t.Right, t.Bottom);
        CombineRgn(r, r, s, RGN_OR);
      end;
    end;
  end
  else
    r := 0;

  SetWindowRgn(FForm.Handle, r, True);
end;

end.
