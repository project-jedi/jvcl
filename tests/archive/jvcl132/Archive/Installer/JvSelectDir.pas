{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSelectDir.PAS, released on 2001-02-28.

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

unit JvSelectDir;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Jpeg,
  JvFormDirectory, JvInstallerPage;

type
  TJvSelectDirOptions = class(TPersistent)
  private
    FText1: string;
    FText2: string;
    FText3: string;
    FDir: string;
    FOnChange: TNotifyEvent;
    procedure SetDir(const Value: string);
    procedure SetText1(const Value: string);
    procedure SetText2(const Value: string);
    procedure SetText3(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Directory: string read FDir write SetDir;
    property Text1: string read FText1 write SetText1;
    property Text2: string read FText2 write SetText2;
    property Text3: string read FText3 write SetText3;
  end;

  TJvSelectDir = class(TJvInstallerPage)
  private
    FForm: TFormDir;
    FOptions: TJvSelectDirOptions;
  protected
    procedure NextClick(Sender: TObject); override;
    procedure PreviousClick(Sender: TObject); override;
    procedure OptionsChanged(Sender: TObject);
    procedure Finish; override;
    procedure UpdateButtons; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Options: TJvSelectDirOptions read FOptions write FOptions;
  end;

implementation

resourcestring
  RC_InstallerFilled = 'Property "Installer" must be filled';
  RC_Select1 = 'You can still abort this installation by clicking the Cancel button.';
  RC_Select2 = 'If you want to install into another folder, click the button with a small arrow.';
  RC_Select3 = 'Setup will now install ';
  RC_Select4 = ' in the following folder.';

  ///////////////////////////////////////////////////////////
  // TJvSelectDir
  ///////////////////////////////////////////////////////////

constructor TJvSelectDir.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TJvSelectDirOptions.Create;
  FOptions.OnChange := OptionsChanged;
end;

{***********************************************}

destructor TJvSelectDir.Destroy;
begin
  FOptions.Free;
  inherited;
end;

{***********************************************}

procedure TJvSelectDir.Execute;
begin
  inherited;
  if Installer <> nil then
  begin
    if FForm = nil then
      FForm := TFormDir.Create(Application);

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

procedure TJvSelectDir.Finish;
begin
  inherited;
  if FForm <> nil then
  begin
    FForm.Tag := 1;
    FForm.Close;
  end;
end;

{***********************************************}

procedure TJvSelectDir.NextClick(Sender: TObject);
begin
  if FForm.BuDirectoryBox1.Directory <> '' then
  begin
    Options.Directory := FForm.BuDirectoryBox1.Directory;
    inherited;
  end
  else
    Beep;
end;

{***********************************************}

procedure TJvSelectDir.OptionsChanged(Sender: TObject);
begin
  if FForm <> nil then
  begin
    if Options.Text1 <> '' then
      FForm.Statictext1.Caption := Options.Text1
    else
      FForm.StaticText1.Caption := RC_Select3 + Installer.Options.ProgramName + RC_Select4;

    if Options.Text2 <> '' then
      FForm.StaticText2.Caption := Options.Text2
    else
      FForm.StaticText2.Caption := RC_Select2;

    if Options.Text3 <> '' then
      FForm.StaticText3.Caption := Options.Text3
    else
      FForm.StaticText3.Caption := RC_Select1;

    FForm.BuDirectoryBox1.Directory := Options.Directory;
  end;
end;

{***********************************************}

procedure TJvSelectDir.PreviousClick(Sender: TObject);
begin
  Options.Directory := FForm.BuDirectoryBox1.Directory;
  inherited;
end;

{***********************************************}

procedure TJvSelectDir.UpdateButtons;
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
// TJvSelectDirOptions
///////////////////////////////////////////////////////////

procedure TJvSelectDirOptions.SetDir(const Value: string);
begin
  FDir := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvSelectDirOptions.SetText1(const Value: string);
begin
  FText1 := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvSelectDirOptions.SetText2(const Value: string);
begin
  FText2 := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvSelectDirOptions.SetText3(const Value: string);
begin
  FText3 := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
