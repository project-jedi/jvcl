{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInstallType.PAS, released on 2001-02-28.

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

unit JvInstallType;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,  JvFormType, JvInstallerPage ,JVCLVer;

type
  TJvInstallTypeOptions = class(TPersistent)
  private
    FIndex: Integer;
    FText: string;
    FOnChange: TNotifyEvent;
    FItems: TstringList;
    procedure SetIndex(const Value: integer);
    procedure SetItems(const Value: TstringList);
    procedure SetText(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy;override;
  published
    property Text:string read FText write SetText;
    property ItemIndex:Integer read FIndex write SetIndex;
    property Items:TstringList read FItems write SetItems;
  end;

  TJvInstallType = class(TJvInstallerPage)
  private
    FForm:TFormTyp;
    FOptions: TJvInstallTypeOptions;
  protected
    procedure NextClick(Sender: TObject);override;
    procedure PreviousClick(Sender: TObject);override;
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    property Options:TJvInstallTypeOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';
   RC_Type1                 =       'Choose the installation type for ';
   RC_Type2                 =       '. Just click the installation type you want and then click Next.';

{***********************************************}
constructor TJvInstallType.Create(AOwner: TComponent);
begin
  inherited;
  FOptions:=TJvInstallTypeOptions.Create;
  FOptions.OnChange:=OptionsChanged;
end;
{***********************************************}
procedure TJvInstallType.Execute;
begin
  inherited;
  if Installer<>nil then
  begin
    if FForm=nil then
      FForm:=TFormTyp.Create(Application);

    //assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FForm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    OptionsChanged(nil);

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
destructor TJvInstallType.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvInstallType.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvInstallType.NextClick(Sender: TObject);
begin
  FOptions.ItemIndex:=FForm.RadioGroup1.ItemIndex;
  inherited;
end;
{***********************************************}
procedure TJvInstallType.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    if FOptions.Text<>'' then Fform.StaticText1.caption:=FOptions.Text
    else FForm.Statictext1.Caption:=RC_Type1+Installer.Options.ProgramName+RC_Type2;

    if FForm.RadioGroup1.Items.Text <> FOptions.Items.Text then
    begin
      FForm.RadioGroup1.Items.Assign(FOptions.Items);
      FForm.RadioGroup1.ItemIndex:=FOptions.ItemIndex;
    end;
  end;
end;
{***********************************************}
procedure TJvInstallType.PreviousClick(Sender: TObject);
begin
  FOptions.ItemIndex:=FForm.RadioGroup1.ItemIndex;
  inherited;
end;
{***********************************************}
procedure TJvInstallType.UpdateButtons;
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


{ TJvInstallTypeOptions }


{***********************************************}
constructor TJvInstallTypeOptions.Create;
begin
  FItems:=TStringList.Create;
end;
{***********************************************}
destructor TJvInstallTypeOptions.Destroy;
begin
  FItems.Free;
  inherited;
end;
{***********************************************}
procedure TJvInstallTypeOptions.SetIndex(const Value: integer);
begin
  FIndex := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallTypeOptions.SetItems(const Value: TstringList);
begin
  FItems.Assign(Value);
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvInstallTypeOptions.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}

end.
