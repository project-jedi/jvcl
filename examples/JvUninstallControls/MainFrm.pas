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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvUninstallControls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    JvUninstallListBox1: TJvUninstallListBox;
    chkListShowAll: TCheckBox;
    chkListSorted: TCheckBox;
    chkShowEmpty: TCheckBox;
    memListInfo: TMemo;
    Panel2: TPanel;
    Splitter1: TSplitter;
    JvUninstallComboBox1: TJvUninstallComboBox;
    chkComboShowAll: TCheckBox;
    chkComboSorted: TCheckBox;
    memComboInfo: TMemo;
    chkComboEmptyValues: TCheckBox;
    procedure chkListShowAllClick(Sender: TObject);
    procedure chkComboShowAllClick(Sender: TObject);
    procedure JvUninstallListBox1Click(Sender: TObject);
    procedure JvUninstallComboBox1Click(Sender: TObject);
    procedure chkListSortedClick(Sender: TObject);
    procedure chkComboSortedClick(Sender: TObject);
    procedure chkShowEmptyClick(Sender: TObject);
    procedure chkComboEmptyValuesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.chkListShowAllClick(Sender: TObject);
begin
  JvUninstallListBox1.ShowAll := chkListShowAll.Checked;
  JvUninstallListBox1.RefreshItem;
end;

procedure TForm1.chkComboShowAllClick(Sender: TObject);
begin
  JvUninstallComboBox1.ShowAll := chkComboShowAll.Checked;
  JvUninstallComboBox1.RefreshItem;
end;

procedure TForm1.JvUninstallListBox1Click(Sender: TObject);
begin
  with JvUninstallListBox1 do
    memListInfo.Lines := Properties;
end;

procedure TForm1.JvUninstallComboBox1Click(Sender: TObject);
begin
  with JvUninstallComboBox1 do
    memComboInfo.Lines := Properties;
end;

procedure TForm1.chkListSortedClick(Sender: TObject);
begin
  JvUninstallListBox1.Sorted := chkListSorted.Checked;
  JvUninstallListBox1.RefreshItem;
end;

procedure TForm1.chkComboSortedClick(Sender: TObject);
begin
  JvUninstallComboBox1.Sorted := chkComboSorted.Checked;
  JvUninstallComboBox1.RefreshItem;
end;

procedure TForm1.chkShowEmptyClick(Sender: TObject);
begin
  JvUninstallListBox1.ShowEmptyValues := chkShowEmpty.Checked;
  JvUninstallListBox1.RefreshItem;
end;

procedure TForm1.chkComboEmptyValuesClick(Sender: TObject);
begin
  JvUninstallComboBox1.ShowEmptyValues := chkComboEmptyValues.Checked;
  JvUninstallComboBox1.RefreshItem;
end;

end.
