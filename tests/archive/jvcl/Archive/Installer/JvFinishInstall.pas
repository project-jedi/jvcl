{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFinishInstall.PAS, released on 2001-02-28.

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

unit JvFinishInstall;

{*******************************************************}
{  Modifications:                                       }
{    3/11/2000  Corrected the problem with options not  }
{               reflecting the real content of the boxes}
{*******************************************************}


{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormFinish, Jpeg, JvInstallerPage, stdctrls;

type
  TJvFinishCheckBox = class(TPersistent)
  private
    FVisible: boolean;
    FChecked: boolean;
    FText: string;
    FOnChange: TNotifyEvent;
    procedure SetVisible(const Value: boolean);
    procedure SetChecked(const Value: boolean);
    procedure SetText(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
  public
  published
    property Visible:boolean read FVisible write SetVisible;
    property Checked:boolean read FChecked write SetChecked;
    property Text:string read FText write SetText;
  end;

  TJvFinishInstallOptions = class(TPersistent)
  private
    Ftext: string;
    FCheckBoxes:array [1..5] of TJvFinishCheckBox;
    FOnChange: TNotifyEvent;
    function GetCheckBox(const Index: Integer): TJvFinishCheckBox;
    procedure SetCheckBox(const Index: Integer;
      const Value: TJvFinishCheckBox);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    procedure OptionsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy;override;
  published
    property Text:string read Ftext write FText;
    property CheckBox1:TJvFinishCheckBox index 1 read GetCheckBox write SetCheckBox;
    property CheckBox2:TJvFinishCheckBox index 2 read GetCheckBox write SetCheckBox;
    property CheckBox3:TJvFinishCheckBox index 3 read GetCheckBox write SetCheckBox;
    property CheckBox4:TJvFinishCheckBox index 4 read GetCheckBox write SetCheckBox;
    property CheckBox5:TJvFinishCheckBox index 5 read GetCheckBox write SetCheckBox;
  end;

  TJvFinishInstall = class(TJvInstallerPage)
  private
    FForm:TFormEnd;
    Ftext: string;
    CheckBoxes: array[1..5] of TCheckBox;
    FOptions: TJvFinishInstallOptions;
  protected
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    property Options:TJvFinishInstallOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';
   RC_Finishing             =       'Setup is now finishing the installation of ';

{***********************************************}
constructor TJvFinishInstall.Create(AOwner: TComponent);
begin
  inherited;

  FForm:=TFormEnd.Create(Application);
  CheckBoxes[1]:=FForm.CheckBox1;
  CheckBoxes[2]:=FForm.CheckBox2;
  CheckBoxes[3]:=FForm.CheckBox3;
  CheckBoxes[4]:=FForm.CheckBox4;
  CheckBoxes[5]:=FForm.CheckBox5;
  FOptions:=TJvFinishInstallOptions.Create;
  FOptions.OnChange:=OptionsChanged;
end;
{***********************************************}
destructor TJvFinishInstall.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvFinishInstall.OptionsChanged(Sender: TObject);
var
 i:Integer;
begin
  if FForm<>nil then
  begin
    for i:=1 to 5 do
    begin
      CheckBoxes[i].Visible:=FOptions.GetCheckBox(i).Visible;
      CheckBoxes[i].Caption:=FOptions.GetCheckBox(i).Text;
      CheckBoxes[i].Checked:=FOptions.GetCheckBox(i).Checked;
    end;
  end;
end;
{***********************************************}
procedure TJvFinishInstall.Execute;
begin
  inherited;
  if Installer<>nil then
  begin
    //assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FFOrm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    if Ftext<>'' then
       FForm.Statictext1.Caption:=Ftext
    else
       FForm.Statictext1.Caption:=RC_Finishing+Installer.Options.ProgramName+'.';

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
procedure TJvFinishInstall.Finish;
var
 i: Integer;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FOptions.OnChange := nil;
    for i:=1 to 5 do
      FOptions.GetCheckBox(i).Checked := CheckBoxes[i].Checked;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvFinishInstall.UpdateButtons;
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

{ TJvFinishCheckBox }

{***********************************************}
procedure TJvFinishCheckBox.SetChecked(const Value: boolean);
begin
  FChecked := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvFinishCheckBox.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvFinishCheckBox.SetVisible(const Value: boolean);
begin
  FVisible := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}


{ TJvFinishInstallOptions }


{***********************************************}
constructor TJvFinishInstallOptions.Create;
var
 i:Integer;
begin
  for i:=1 to 5 do
  begin
    FCheckBoxes[i]:=TJvFinishCheckBox.Create;
    FCheckBoxes[i].OnChange:=OptionsChanged;
  end;
end;
{***********************************************}
destructor TJvFinishInstallOptions.Destroy;
var
 i:Integer;
begin
  for i:=1 to 5 do
    FCheckBoxes[i].Free;
  inherited;
end;
{***********************************************}
function TJvFinishInstallOptions.GetCheckBox(const Index: Integer): TJvFinishCheckBox;
begin
  result:=FCheckBoxes[Index];
end;
{***********************************************}
procedure TJvFinishInstallOptions.OptionsChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}
procedure TJvFinishInstallOptions.SetCheckBox(const Index: Integer;
  const Value: TJvFinishCheckBox);
begin
  FCheckBoxes[Index] := Value;
end;
{***********************************************}


end.
