{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit CsvDataSourceDemoFm;

{ Demo by Warren Postma, warrenpstma@hotmail.com }

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, DB, QGrids, QDBGrids, QExtCtrls, QDBCtrls, QStdCtrls,
  QComCtrls, JvQCSVParse, JvQCsvData, JvCsvData;


type
  TCsvDataSourceForm = class(TForm)
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    JvCsvDataSet1: TJvCsvDataSet;
    JvCsvDataSet1NAME: TStringField;
    JvCsvDataSet1ADDRESS: TStringField;
    JvCsvDataSet1ADDRESS2: TStringField;
    JvCsvDataSet1TELEPHONE: TStringField;
    JvCsvDataSet1AGE: TIntegerField;
    JvCsvDataSet1LASTPHONECALL: TDateTimeField;
    JvCsvDataSet1PRIVATENUMBER: TBooleanField;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Button4: TButton;
    OpenDialog1: TOpenDialog;
    Memo2: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  public
  end;

var
  CsvDataSourceForm: TCsvDataSourceForm;

implementation

{$R *.xfm}

procedure TCsvDataSourceForm.Button2Click(Sender: TObject);
begin
  // OutputDebugString(PChar(JvCsvDataSet1.GetCsvHeader)); // neat trick. reads first line of file only.

  JvCsvDataSet1.Flush; // flush current contents to disk. okay, maybe it should be called save,
                       // then you wouldn't see jokes in the code about how this component has
                       // been tested, and shown to last for over two thousand flushes. <grin>
end;

procedure TCsvDataSourceForm.Button1Click(Sender: TObject);
begin
   Memo1.Clear;
   JvCsvDataSet1.AssignToStrings(Memo1.Lines);
end;

procedure TCsvDataSourceForm.Button3Click(Sender: TObject);
begin
   JvCsvDataSet1.Active := False;
   JvCsvDataSet1.AssignFromStrings(Memo1.Lines);
   JvCsvDataSet1.Active := True;
end;

procedure TCsvDataSourceForm.FormCreate(Sender: TObject);
begin
   JvCsvDataSet1.Active := True; // ensure it's opened when the app is started.

   // second component tests what happens if you don't set up the CsvDef field

//  JvCsvDataSet2.Active := True; // ensure it's opened when the app is started.
end;

procedure TCsvDataSourceForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if DataSource1.State = dsEdit then
    JvCsvDataSet1.Post;
end;

procedure TCsvDataSourceForm.ComboBox1Change(Sender: TObject);
begin
   JvCsvDataSet1LASTPHONECALL.DisplayFormat := StrStrip(ComboBox1.Text);
end;

procedure TCsvDataSourceForm.Button4Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
end;

end.
