{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAgreement.PAS, released on 2001-02-28.

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

unit JvAgreement;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Jpeg, JvFormAgreement, JvInstallerPage;

type
  TJvAgreementOptions = class(TPersistent)
  private
    FText: string;
    FOnChange: TNotifyEvent;
    FItem: TStringList;
    procedure SetItem(const Value: TStringList);
    procedure SetText(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy;override;
  published
    property Item:TStringList read FItem write SetItem;
    property Text:string read FText write SetText;
  end;

  TJvAgreement = class(TJvInstallerPage)
  private
    FForm:TFormAgreem;
    FOptions: TJvAgreementOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    property Options:TJvAgreementOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';

{***********************************************}
constructor TJvAgreement.Create(AOwner: TComponent);
begin
  inherited;
  FOptions:=TJvAgreementOptions.Create;
  FOptions.OnChange:=OptionsChanged;
end;
{***********************************************}
destructor TJvAgreement.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvAgreement.Execute;
begin
  inherited;
  if Installer<>nil then
  begin
    if FForm=nil then
       FForm:=TFormAgreem.Create(Application);

    //Assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FFOrm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    //Assign text
    OptionsChanged(nil);

    FForm.tag:=0;
    FForm.Formstyle:=fsStayOnTop;
    FForm.Show;
  end
  else
      raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvAgreement.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvAgreement.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    FForm.Memo1.lines.Text:=Options.Item.Text;
    if Options.Text<>'' then
       FForm.StaticText1.Caption:=Options.Text;
  end;
end;
{***********************************************}
procedure TJvAgreement.UpdateButtons;
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


{ TJvAgreementOptions }

{***********************************************}
constructor TJvAgreementOptions.Create;
begin
  FItem := TStringList.Create;
end;
{***********************************************}
destructor TJvAgreementOptions.Destroy;
begin
  FItem.Free;
  inherited;
end;
{***********************************************}
procedure TJvAgreementOptions.SetItem(const Value: TStringList);
begin
  FItem.Assign(Value);
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvAgreementOptions.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}

end.
