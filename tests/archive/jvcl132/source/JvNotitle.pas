{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvNotitle.PAS, released on 2001-02-28.

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

unit JvNotitle;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JvComponent;

type
  TJvNoTitle = class(TJvComponent)
  private
    FHasTitle: Boolean;
    FForm: TCustomForm;
    procedure SetTitle(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HasTitle: Boolean read FHasTitle write SetTitle default True;
  end;

implementation

{**************************************************}

constructor TJvNoTitle.Create(AOwner: TComponent);
begin
  inherited;
  FHasTitle := True;
  FForm := GetParentForm(TControl(AOwner));
end;

{**************************************************}

destructor TJvNoTitle.Destroy;
begin
  if not (csDestroying in FForm.ComponentState) then
    SetTitle(True);
  inherited;
end;

{**************************************************}

procedure TJvNoTitle.SetTitle(const Value: Boolean);
begin
  FHasTitle := Value;
  if FHasTitle then
    SetWindowLong(FForm.Handle, GWL_STYLE, GetWindowLong(FForm.Handle, GWL_STYLE) or WS_CAPTION)
  else
    SetWindowLong(FForm.Handle, GWL_STYLE, GetWindowLong(FForm.Handle, GWL_STYLE) and not WS_CAPTION);
  FForm.Invalidate;
end;

end.
