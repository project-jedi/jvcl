{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCompoInstall.PAS, released on 2001-02-28.

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

unit JvCompoInstall;

{*******************************************************}
{  Modifications:                                       }
{     2/11/2000 Added a second column in the list. To   }
{               display the size of the package         }
{*******************************************************}

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvFormCompo, ComCtrls, JvInstallerPage;

type
  TJvCompoInstall = class;

  TJvCompoInstallOptions = class(TPersistent)
  private
    FAtLeast: boolean;
    Ftext: string;
    FOwner: TJvCompoInstall;
    FOnChange: TNotifyEvent;
    function GetItems: TListItems;
    procedure SetAtLeast(const Value: boolean);
    procedure SetItems(const Value: TListItems);
    procedure SetText(const Value: string);
  protected
    property OnChange:TNotifyEvent read FOnChange write FOnChange;
    property Owner:TJvCompoInstall read FOwner write FOwner;
  public
  published
    property Text:string read Ftext write SetText;
    property Items:TListItems read GetItems write SetItems;
    property AtLeastOne:boolean read FAtLeast write SetAtLeast default true;
  end;

  TJvCompoInstall = class(TJvInstallerPage)
  private
    FFirst:boolean;
    FForm:TFormComp;
    FOptions: TJvCompoInstallOptions;
  protected
    procedure NextClick(Sender: TObject);override;
    procedure OptionsChanged(Sender: TObject);
    procedure Finish;override;
    procedure UpdateButtons;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure Execute;override;
  published
    property Options:TJvCompoInstallOptions read FOptions write FOptions;
  end;

implementation

resourcestring
   RC_InstallerFilled       =       'Property "Installer" must be filled';
   RC_Compo1                =       'Choose the installation components for ';
   RC_Compo2                =       '. When finished, click the button "next".';

{***********************************************}
constructor TJvCompoInstall.Create(AOwner: TComponent);
begin
  inherited;
  FFirst:=true;
  FForm:=TFormComp.Create(Application);
  FOptions:=TJvCompoInstallOptions.Create;
  FOptions.OnChange:=OptionsChanged;
  FOptions.Owner:=self;
end;
{***********************************************}
destructor TJvCompoInstall.Destroy;
begin
  FOptions.Free;
  inherited;
end;
{***********************************************}
procedure TJvCompoInstall.Execute;
var
   i:Integer;
begin
  inherited;
  if Installer<>nil then
  begin
    //assign buttons events
    FForm.BuButton3.OnClick:=Installer.Cancel;
    FForm.BuButton2.OnClick:=NextClick;
    FFOrm.BuButton1.OnClick:=PreviousClick;
    UpdateButtons;

    if FFirst then
    begin
      FFirst:=false;
      for i:=0 to Fform.BuListView1.Items.Count-1 do
          Fform.BuListView1.Items[i].Checked:=true;
    end;

    OptionsChanged(nil);
    if (ImageNotEmpty) then
      FForm.Image1.picture.bitmap.assign(Installer.Options.picture);
      
    FForm.tag:=0;
    FForm.Formstyle:=fsStayOnTop;
    FForm.Visible:=true;
  end
  else
      raise Exception.Create(RC_InstallerFilled);
end;
{***********************************************}
procedure TJvCompoInstall.Finish;
begin
  inherited;
  if FForm<>nil then
  begin
    FForm.Tag:=1;
    FForm.Close;
  end;
end;
{***********************************************}
procedure TJvCompoInstall.NextClick(Sender: TObject);
var
   i,j:Integer;
begin
  if FOptions.AtLeastOne then
  begin
    j:=0;
    for i:=0 to FForm.BuListView1.Items.count-1 do
      if FForm.BuListView1.Items[i].checked then inc(j);
    if j>0 then
      inherited
    else
      Beep;
  end
  else
    inherited;
end;
{***********************************************}
procedure TJvCompoInstall.OptionsChanged(Sender: TObject);
begin
  if FForm<>nil then
  begin
    if FOptions.text<>'' then
       Fform.StaticText1.caption:=FOptions.Text
    else
       FForm.Statictext1.Caption:=RC_Compo1+Installer.Options.ProgramName+RC_Compo2;
  end;
end;
{***********************************************}
procedure TJvCompoInstall.UpdateButtons;
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


{ TJvCompoInstallOptions }


{***********************************************}
function TJvCompoInstallOptions.GetItems: TListItems;
begin
  if Owner<>nil then
    result:=Owner.FForm.BuListView1.items
  else
    result:=nil
end;
{***********************************************}
procedure TJvCompoInstallOptions.SetAtLeast(const Value: boolean);
begin
  FAtLeast := Value;
end;
{***********************************************}
procedure TJvCompoInstallOptions.SetItems(const Value: TListItems);
var
   i:Integer;
begin
  if Owner<>nil then
    Owner.Fform.BuListview1.items.assign(Value);
    for i:=0 to Owner.Fform.BuListview1.items.Count-1 do
        Owner.Fform.BuListview1.items[i].Checked:=true;
end;
{***********************************************}
procedure TJvCompoInstallOptions.SetText(const Value: string);
begin
  Ftext := Value;
  if Assigned(FOnChange) then FOnChange(self);
end;
{***********************************************}

end.
