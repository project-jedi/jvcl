{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUsernameSerial.PAS, released on 2001-02-28.

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

unit JvUsernameSerial;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Jpeg,
  JvFormSerial, JvInstallerPage;

type
  TJvUsernameSerialOptions = class(TPersistent)
  private
    FText: string;
    FOnChange: TNotifyEvent;
    FUsername: string;
    FSerial: string;
    procedure SetSerial(const Value: string);
    procedure SetText(const Value: string);
    procedure SetUsername(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
  published
    property Text: string read FText write SetText;
    property Username: string read FUsername write SetUsername;
    property Serial: string read FSerial write SetSerial;
  end;

  TJvUsernameSerial = class(TJvInstallerPage)
  private
    FForm: TFormSer;
    FOptions: TJvUsernameSerialOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure PreviousClick(Sender: TObject); override;
    procedure NextClick(Sender: TObject); override;
    procedure Finish; override;
    procedure UpdateButtons; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Options: TJvUsernameSerialOptions read FOptions write FOptions;
  end;

implementation

resourcestring
  RC_InstallerFilled = 'Property "Installer" must be filled';
  RC_Serial1 = 'Enter your username and password.';

  ///////////////////////////////////////////////////////////
  // TJvUsernameSerial
  ///////////////////////////////////////////////////////////

constructor TJvUsernameSerial.Create(AOwner: TComponent);
begin
  inherited;
  FForm := TFormSer.Create(Application);
  FOptions := TJvUsernameSerialOptions.Create;
  FOptions.OnChange := OptionsChanged;
end;

{***********************************************}

destructor TJvUsernameSerial.Destroy;
begin
  FOptions.Free;
  inherited;
end;

{***********************************************}

procedure TJvUsernameSerial.Execute;
begin
  inherited;
  if Installer <> nil then
  begin
    //assign buttons events
    FForm.BuButton3.OnClick := Installer.Cancel;
    FForm.BuButton2.OnClick := NextClick;
    FFOrm.BuButton1.OnClick := PreviousClick;
    UpdateButtons;

    OptionsChanged(nil);

    if (ImageNotEmpty) then
      FForm.Image1.Picture.Bitmap.Assign(Installer.Options.Picture);
    FForm.Tag := 0;
    FForm.FormStyle := fsStayOnTop;
    FForm.Visible := True;
  end
  else
    raise Exception.Create(RC_InstallerFilled);
end;

{***********************************************}

procedure TJvUsernameSerial.Finish;
begin
  inherited;
  if FForm <> nil then
  begin
    FForm.Tag := 1;
    FForm.Close;
  end;
end;

{***********************************************}

procedure TJvUsernameSerial.NextClick(Sender: TObject);
begin
  FOptions.FUsername := FForm.Edit1.Text;
  FOptions.FSerial := FForm.Edit2.Text;
  inherited;
end;

{***********************************************}

procedure TJvUsernameSerial.OptionsChanged(Sender: TObject);
begin
  if FForm <> nil then
  begin
    if FOptions.text <> '' then
      FForm.StaticText1.Caption := FOptions.Text
    else
      FForm.StaticText1.Caption := RC_Serial1;
    FForm.Edit1.Text := FOptions.FUsername;
    FForm.Edit2.Text := FOptions.FSerial;
  end;
end;

{***********************************************}

procedure TJvUsernameSerial.PreviousClick(Sender: TObject);
begin
  FOptions.FUsername := FForm.Edit1.Text;
  FOptions.FSerial := FForm.Edit2.Text;
  inherited;
end;

{***********************************************}

procedure TJvUsernameSerial.UpdateButtons;
begin
  inherited;
  if FForm <> nil then
  begin
    Buttons.Previous.AssignTo(FForm.BuButton1);
    Buttons.Next.AssignTo(FForm.BuButton2);
    Buttons.Cancel.AssignTo(FForm.BuButton3);
  end;
end;

///////////////////////////////////////////////////////////
// TJvUsernameSerialOptions
///////////////////////////////////////////////////////////

procedure TJvUsernameSerialOptions.SetSerial(const Value: string);
begin
  FSerial := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvUsernameSerialOptions.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvUsernameSerialOptions.SetUsername(const Value: string);
begin
  FUsername := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
