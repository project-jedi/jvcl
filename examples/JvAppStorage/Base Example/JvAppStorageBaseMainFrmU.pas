{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may  
 not use this file except in compliance with the License. You may 
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}
unit JvAppStorageBaseMainFrmU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvAppXMLStorage, JvFormPlacementSelectList, JvFormPlacement,
  JvAppDBStorage, JvAppStorageSelectList, JvAppStorage, JvAppIniStorage,
  JvComponent, JvAppRegistryStorage, ExtCtrls, ComCtrls, ToolWin, StdCtrls,
  ShellAPI, Menus;

type
  TJvAppStorageBaseMainFrm = class(TForm, IJvAppStorageHandler)
    StatusBar1: TStatusBar;
    JvAppIniFileStorage1: TJvAppIniFileStorage;
    JvFormStorage1: TJvFormStorage;
    PopupMenu1: TPopupMenu;
    AnotherOptiopn1: TMenuItem;
    YetAnotherOption1: TMenuItem;
    Option1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    CheckBox2: TCheckBox;
    TrackBar1: TTrackBar;
    DateTimePicker1: TDateTimePicker;
    Panel3: TPanel;
    Memo2: TMemo;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    JvFormStorageSelectList1: TJvFormStorageSelectList;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    procedure YetAnotherOption1Click(Sender: TObject);
  private
    { Private declarations }
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
  end;

var
  JvAppStorageBaseMainFrm: TJvAppStorageBaseMainFrm;

implementation

{$R *.dfm}

procedure TJvAppStorageBaseMainFrm.YetAnotherOption1Click(Sender: TObject);
begin
  if Sender is TMenuItem
     then TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TJvAppStorageBaseMainFrm.ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  CheckBox1.Checked := AppStorage.ReadBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox1']), CheckBox1.Checked);
  CheckBox2.Checked := AppStorage.ReadBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox2']), CheckBox2.Checked);
end;

procedure TJvAppStorageBaseMainFrm.WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  AppStorage.WriteBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox1']), CheckBox1.Checked);
  AppStorage.WriteBoolean(AppStorage.ConcatPaths([BasePath, 'MyCheckBox2']), CheckBox2.Checked);
end;


end.
