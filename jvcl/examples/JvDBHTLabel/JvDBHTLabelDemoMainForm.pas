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
unit JvDBHTLabelDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DBCtrls, DB, StdCtrls, JvExStdCtrls,
  JvHTControls, JvCsvData, JvDBHTLabel;

type
  TJvDBHTLabelDemoMainFrm = class(TForm)
    JvDBHTLabel1: TJvDBHTLabel;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    memFormat: TMemo;
    Button1: TButton;
    JvCsvDataSet1: TJvCsvDataSet;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  JvDBHTLabelDemoMainFrm: TJvDBHTLabelDemoMainFrm;

implementation

{$R *.dfm}

procedure TJvDBHTLabelDemoMainFrm.Button1Click(Sender: TObject);
begin
  JvDBHTLabel1.Mask := StringReplace(memFormat.Lines.Text, #13#10, '', [rfReplaceAll]);
end;

procedure TJvDBHTLabelDemoMainFrm.FormCreate(Sender: TObject);
begin
  if not FileExists(JvCsvDataSet1.FileName) then
    ShowMessageFmt('Data file "%s" not found!', [JvCsvDataSet1.FileName])
  else
    JvCsvDataSet1.Open;
  memFormat.Lines.Text := JvDBHTLabel1.Mask;
end;

end.

