{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWelcome.PAS, released on 2001-02-28.

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

unit JvWelcome;

{$ObjExportAll On} 

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormWelcome, Jpeg, JvInstallerPage;

type
  TJvWelcomeOptions = class(TPersistent)
  private
    FText3: string;
    FText1: string;
    FText2: string;
    FOnChange: TNotifyEvent;
    procedure SetText1(const Value: string);
    procedure SetText2(const Value: string);
    procedure SetText3(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  public
  published
    property Text1:string read FText1 write SetText1;
    property Text2:string read FText2 write SetText2;
    property Text3:string read FText3 write SetText3;
  end;

  TJvWelcome = class(TJvInstallerPage)
  private
    FForm:TFormW;
    FOptions: TJvWelcomeOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    property Options:TJvWelcomeOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';

{***********************************************}
constructor TJvWelcome.Create(AOwner: TComponent);
begin
  inherited;
  self.Buttons.Previous.Enabled:=false;
  FOptions:=TJvWelcomeOptions.Create;
  FOptions.OnChange:=OptionsChanged;
end;
{***********************************************}
destructor TJvWelcome.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvWelcome.Execute;
begin
  inherited;
  if Installer<>nil then
  begin
    if FForm=nil then
      FForm:=TFormW.Create(Application);

    //Assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FForm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    //Assign picture
    if (ImageNotEmpty) then
       FForm.Image1.Picture.Bitmap.Assign(Installer.Options.Picture);

    //Assign texts
    OptionsChanged(self);

    FForm.tag:=0;
    FForm.Formstyle:=fsStayOnTop;
    FForm.Show;
  end
  else
     raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvWelcome.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvWelcome.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    if Options.Text1<>'' then FForm.StaticText1.Caption:=Options.Text1;
    if Options.Text2<>'' then FForm.StaticText1.Caption:=Options.Text2;
    if Options.Text3<>'' then FForm.StaticText1.Caption:=Options.Text3;
  end;
end;
{***********************************************}
procedure TJvWelcome.UpdateButtons;
begin
  inherited;
  if FForm<>nil then
  begin
    Buttons.Previous.AssignTo(FForm.BuButton1);
    Buttons.Next.AssignTo(FForm.BuButton2);
    Buttons.Cancel.AssignTo(FForm.BuButton3);
  end;
end;
{***********************************************}

{ TJvWelcomeOptions }

{***********************************************}
procedure TJvWelcomeOptions.SetText1(const Value: string);
begin
  FText1 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvWelcomeOptions.SetText2(const Value: string);
begin
  FText2 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvWelcomeOptions.SetText3(const Value: string);
begin
  FText3 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}

end.
