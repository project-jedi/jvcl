{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInstallConfirm.PAS, released on 2001-02-28.

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

unit JvInstallConfirm;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormInstallConfirm, Jpeg, JvInstallerPage, stdctrls;

type
  TJvInstallConfirmOptions = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FText: TStringList;
    FText1: string;
    procedure SetText(const Value: TStringList);
    procedure SetText1(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy;override; 
  published
    property ConfirmText:TStringList read FText write SetText;
    property Text:string read FText1 write SetText1;
  end;

  TJvInstallConfirm = class(TJvInstallerPage)
  private
    FForm:TfoInstallConfirm;
    FOptions: TJvInstallConfirmOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    property Options:TJvInstallConfirmOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';

{***********************************************}
constructor TJvInstallConfirm.Create(AOwner: TComponent);
begin
  inherited;
  FOptions:=TJvInstallConfirmOptions.Create;
  FOptions.OnChange:=OptionsChanged;
end;
{***********************************************}
destructor TJvInstallConfirm.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvInstallConfirm.Execute;
begin
  inherited;
  if Installer<>nil then
  begin
    if FForm=nil then
      FForm:=TfoInstallConfirm.Create(Application);

    //assign buttons events
    FForm.buButton3.OnClick:=Installer.Cancel;
    FForm.buButton2.OnClick:=NextClick;
    FForm.buButton1.OnClick:=PreviousClick;
    UpdateButtons;

    OptionsChanged(nil);

    if (ImageNotEmpty) then
       FForm.Image1.Picture.Bitmap.Assign(Installer.Options.picture);

    FForm.Tag:=0;
    FForm.Formstyle:=fsStayOnTop;
    FForm.Visible:=true;
  end
  else
     raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvInstallConfirm.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag := 1;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvInstallConfirm.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    FForm.Memo1.Lines.Text := FOptions.ConfirmText.Text;
    if FOptions.Text<>'' then
      FForm.StaticText1.Caption := FOptions.Text;
  end;
end;
{***********************************************}
procedure TJvInstallConfirm.UpdateButtons;
begin
  inherited;
  if FForm<>nil then
  begin
    Buttons.Previous.AssignTo(FForm.buButton1);
    Buttons.Next.AssignTo(FForm.buButton2);
    Buttons.Cancel.AssignTo(FForm.buButton3);
  end;
end;
{***********************************************}


{ TJvInstallConfirmOptions }

{***********************************************}
constructor TJvInstallConfirmOptions.Create;
begin
  FText := TStringList.Create;
end;
{***********************************************}
destructor TJvInstallConfirmOptions.Destroy;
begin
  FText.Free;
  inherited;
end;
{***********************************************}
procedure TJvInstallConfirmOptions.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallConfirmOptions.SetText1(const Value: string);
begin
  FText1 := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
end.
