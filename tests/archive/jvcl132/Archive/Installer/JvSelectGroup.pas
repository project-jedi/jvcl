{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSelectGroup.PAS, released on 2001-02-28.

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

unit JvSelectGroup;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Jpeg,
  JvFormGroups, JvInstallerPage;

type
  TJvSelectGroupOptions = class(TPersistent)
  private
    FDirectory: string;
    FGroup: string;
    FText: string;
    FOnChange: TNotifyEvent;
    procedure SetGroup(const Value: string);
    procedure SetDirectory(const Value: string);
    procedure SetText(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Directory: string read FDirectory write SetDirectory;
    property Group: string read FGroup write SetGroup;
    property Text: string read FText write SetText;
  end;

  TJvSelectGroup = class(TJvInstallerPage)
  private
    FForm: TFormGroup;
    FOptions: TJvSelectGroupOptions;
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
    property Options: TJvSelectGroupOptions read FOptions write FOptions;
  end;

implementation

resourcestring
  RC_InstallerFilled = 'Property "Installer" must be filled';
  RC_Group1 = 'Enter the name of the group to add ';
  RC_Group2 = ' icons to :';

  ///////////////////////////////////////////////////////////
  // TJvSelectGroup
  ///////////////////////////////////////////////////////////

constructor TJvSelectGroup.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TJvSelectGroupOptions.Create;
  FOptions.OnChange := OptionsChanged;
end;

{***********************************************}

destructor TJvSelectGroup.Destroy;
begin
  FOptions.Free;
  inherited;
end;

{***********************************************}

procedure TJvSelectGroup.Execute;
begin
  inherited;
  if Installer <> nil then
  begin
    if FForm = nil then
      FForm := TFormGroup.Create(Application);

    //assign buttons events
    FForm.BuButton3.OnClick := Installer.Cancel;
    FForm.BuButton2.OnClick := NextClick;
    FForm.BuButton1.OnClick := PreviousClick;
    UpdateButtons;

    OptionsChanged(nil);

    if ImageNotEmpty then
      FForm.Image1.Picture.Bitmap.Assign(Installer.Options.Picture);

    FForm.Tag := 0;
    FForm.FormStyle := fsStayOnTop;
    FForm.Visible := True;
  end
  else
    raise Exception.Create(RC_InstallerFilled);
end;

{***********************************************}

procedure TJvSelectGroup.Finish;
begin
  inherited;
  if FForm <> nil then
  begin
    FForm.Tag := 1;
    FForm.Close;
  end;
end;

{***********************************************}

procedure TJvSelectGroup.NextClick(Sender: TObject);
begin
  Options.FGroup := FForm.Edit1.Text;
  Options.FDirectory := FForm.BuDirectories1.Programs + FForm.Edit1.Text;
  inherited;
end;

{***********************************************}

procedure TJvSelectGroup.OptionsChanged(Sender: TObject);
begin
  if FForm <> nil then
  begin
    if Options.Text <> '' then
      FForm.StaticText1.Caption := Options.Text
    else
      FForm.StaticText1.Caption := RC_Group1 + Installer.Options.ProgramName + RC_Group2;
    FForm.edit1.Text := Options.Group;
    FForm.GroupName := Options.Group;
  end;
end;

{***********************************************}

procedure TJvSelectGroup.PreviousClick(Sender: TObject);
begin
  Options.Directory := FForm.BuDirectories1.Programs + FForm.Edit1.Text;
  inherited;
end;

{***********************************************}

procedure TJvSelectGroup.UpdateButtons;
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
// TJvSelectGroupOptions
///////////////////////////////////////////////////////////

procedure TJvSelectGroupOptions.SetGroup(const Value: string);
begin
  FGroup := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvSelectGroupOptions.SetDirectory(const Value: string);
begin
  FDirectory := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{***********************************************}

procedure TJvSelectGroupOptions.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
