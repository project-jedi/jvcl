{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvReadme.PAS, released on 2001-02-28.

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

unit JvReadme;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Jpeg,
  JvFormReadme, JvInstallerPage;

type
  TJvReadmeOptions = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FText: TStringList;
    procedure SetText(const Value: TStringList);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Text: TStringList read FText write SetText;
  end;

  TJvReadme = class(TJvInstallerPage)
  private
    FForm: TFormRead;
    FOptions: TJvReadmeOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure Finish; override;
    procedure UpdateButtons; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property Options: TJvReadmeOptions read FOptions write FOptions;
  end;

implementation

resourcestring
  RC_InstallerFilled = 'Property "Installer" must be filled';

  ///////////////////////////////////////////////////////////
  // TJvReadme
  ///////////////////////////////////////////////////////////

constructor TJvReadme.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := TJvReadmeOptions.Create;
  FOptions.OnChange := OptionsChanged;
end;

{***********************************************}

procedure TJvReadme.Execute;
begin
  inherited;
  if Installer <> nil then
  begin
    if FForm = nil then
      FForm := TFormRead.Create(Application);

    //assign buttons events
    FForm.BuButton3.OnClick := Installer.Cancel;
    FForm.BuButton2.OnClick := NextClick;
    FForm.BuButton1.OnClick := PreviousClick;
    UpdateButtons;

    if ImageNotEmpty then
      FForm.Image1.Picture.Bitmap.Assign(Installer.Options.Picture);
    OptionsChanged(nil);

    FForm.Tag := 0;
    FForm.FormStyle := fsStayOnTop;
    FForm.Visible := True;
  end
  else
    raise Exception.Create(RC_InstallerFilled);
end;

{***********************************************}

destructor TJvReadme.Destroy;
begin
  FOptions.Free;
  inherited;
end;

{***********************************************}

procedure TJvReadme.Finish;
begin
  inherited;
  if FForm <> nil then
  begin
    FForm.Tag := 1;
    FForm.Close;
  end;
end;

{***********************************************}

procedure TJvReadme.OptionsChanged(Sender: TObject);
begin
  if FForm <> nil then
    FForm.Memo1.Lines.Text := Options.Text.Text;
end;

{***********************************************}

procedure TJvReadme.UpdateButtons;
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
// TJvReadmeOptions
///////////////////////////////////////////////////////////

constructor TJvReadmeOptions.Create;
begin
  FText := TStringList.Create;
end;

{***********************************************}

destructor TJvReadmeOptions.Destroy;
begin
  FText.Free;
  inherited;
end;

{***********************************************}

procedure TJvReadmeOptions.SetText(const Value: TStringList);
begin
  FText.Assign(Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
