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

unit JvAppStorageSubStorageMainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvAppXMLStorage, JvFormPlacementSelectList, JvFormPlacement,
  JvAppDBStorage, JvAppStorageSelectList, JvAppStorage, JvAppIniStorage,
  JvComponent, JvAppRegistryStorage, ExtCtrls, ComCtrls, ToolWin, StdCtrls,
  ShellAPI, Menus, jvDynControlEngine, jvDynControlEngineJVCL;

type
  TJvAppStorageSubStorageMainForm = class(TForm)
    StatusBar1: TStatusBar;
    JvAppIniFileStorage1: TJvAppIniFileStorage;
    JvFormStorage1: TJvFormStorage;
    PopupMenu1: TPopupMenu;
    AnotherOptiopn1: TMenuItem;
    YetAnotherOption1: TMenuItem;
    Option1: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    Memo2: TMemo;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    JvFormStorageSelectList1: TJvFormStorageSelectList;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    JvAppXMLFileStorage1: TJvAppXMLFileStorage;
    JvAppRegistryStorage1: TJvAppRegistryStorage;
    JvAppStorage1: TJvAppStorage;
    Panel2: TPanel;
    Label1: TLabel;
    rbXML: TRadioButton;
    rbReg: TRadioButton;
    rbINI: TRadioButton;
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    Label3: TLabel;
    procedure YetAnotherOption1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure jvStorageKindClick(Sender: TObject);
    procedure JvFormStorage1RestorePlacement(Sender: TObject);
  private
    procedure CheckStorageKind;
  protected
    procedure loaded; override;
    { Private declarations }
  public
    { Public declarations }
    StoragePath: String;
  end;

var
  JvAppStorageSubStorageMainForm: TJvAppStorageSubStorageMainForm;

implementation

{$R *.dfm}

procedure TJvAppStorageSubStorageMainForm.YetAnotherOption1Click(Sender: TObject);
begin
  if Sender is TMenuItem
     then TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TJvAppStorageSubStorageMainForm.ToolButton1Click(Sender: TObject);
begin
  JvFormStorage1.SaveFormPlacement;
end;

procedure TJvAppStorageSubStorageMainForm.ToolButton2Click(Sender: TObject);
begin
  JvFormStorage1.RestoreFormPlacement;
end;

procedure TJvAppStorageSubStorageMainForm.Button1Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage1;
    FormStorageSelectList.AppStorage := JvAppINIFileStorage1;
    FormStorageSelectList.SelectPath  := 'SelectTest';
    FormStorageSelectList.RestoreFormStorage;
  finally
    FormStorageSelectList.Free;
  end;
end;

procedure TJvAppStorageSubStorageMainForm.Button2Click(Sender: TObject);
var
  FormStorageSelectList: tJvFormStorageSelectList;
begin
  FormStorageSelectList := tJvFormStorageSelectList.Create(Self);
  try
    FormStorageSelectList.FormStorage := JvFormStorage1;
    FormStorageSelectList.AppStorage := JvAppINIFileStorage1;
    FormStorageSelectList.SelectPath  := 'SelectTest';
    FormStorageSelectList.SaveFormStorage;
      finally
    FormStorageSelectList.Free;
  end;
end;

procedure TJvAppStorageSubStorageMainForm.ToolButton5Click(Sender: TObject);
begin
  jvAppStorage1.WriteInteger(StoragePath+'ListBox1Selected', ListBox1.ItemIndex);
end;

procedure TJvAppStorageSubStorageMainForm.ToolButton6Click(Sender: TObject);
begin
  ListBox1.ItemIndex := jvAppStorage1.ReadInteger(StoragePath+'ListBox1Selected', 0);
end;

procedure TJvAppStorageSubStorageMainForm.CheckStorageKind;
begin
  if rbXML.Checked
     then StoragePath := '\XML\';
  if rbINI.Checked
     then StoragePath := '\INI\';
  if rbReg.Checked
     then StoragePath := '\REG\';
  jvFormStorage1.AppStoragePath := StoragePath;
end;

procedure TJvAppStorageSubStorageMainForm.jvStorageKindClick(Sender: TObject);
begin
  CheckStorageKind;
end;

procedure TJvAppStorageSubStorageMainForm.JvFormStorage1RestorePlacement(Sender: TObject);
begin
  CheckStorageKind;
  // here is an excellent place to restore values for a form if they
  // are the sort of properties that are not published, such as
  // selection values etc...
end;

procedure TJvAppStorageSubStorageMainForm.loaded;
begin
  CheckStorageKind;
end;

end.
