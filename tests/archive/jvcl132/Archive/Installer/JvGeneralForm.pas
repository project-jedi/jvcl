{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGeneralForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvGeneralForm;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Jpeg, JvFormGeneral, JvInstallerPage;

type
  TJvGeneralForm = class(TJvInstallerPage)
  private
    FForm:TFormGen;
  protected
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    procedure Execute;override;
  published
    function GetForm:TForm;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';

{***********************************************}
constructor TJvGeneralForm.Create(AOwner: TComponent);
begin
  inherited;
  FForm:=TFormGen.Create(Application);
end;
{***********************************************}
procedure TJvGeneralForm.Execute;
begin
  inherited;
  if Installer<>nil then
  begin
    //assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FFOrm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    if (ImageNotEmpty) then
      FForm.Image1.picture.bitmap.assign(Installer.Options.picture);

    FForm.Tag:=0;
    FForm.Formstyle:=fsStayOnTop;
    FForm.Visible:=true;
  end
  else
      raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvGeneralForm.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FForm.Close;
  end;
end;
{***********************************************}
function TJvGeneralForm.GetForm: TForm;
begin
  result:=FForm;
end;
{***********************************************}
procedure TJvGeneralForm.UpdateButtons;
begin
  inherited;
  if FForm<>nil then
  begin
    Buttons.Previous.AssignTo(FForm.BuButton1);
    Buttons.Next.AssignTo(FForm.buButton2);
    Buttons.Cancel.AssignTo(FForm.buButton3);
  end;
end;
{***********************************************}
end.
