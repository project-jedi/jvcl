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

unit JvDbMaskEditDemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvMaskEdit, JvDBControls, ExtCtrls, DBCtrls,
  dbcgrids, DB, JvCsvData, JvEdit, JvValidateEdit, JvExMask, JvToolEdit;

type
  TJvDbMaskEditDemoFrm = class(TForm)
    JvCsvDataSet1: TJvCsvDataSet;
    DataSource1: TDataSource;
    JvCsvDataSet1NAME: TStringField;
    JvCsvDataSet1PHONE: TStringField;
    DBCtrlGrid1: TDBCtrlGrid;
    DBNavigator1: TDBNavigator;
    Label3: TLabel;
    Label4: TLabel;
    Label1: TLabel;
    EditNAME: TJvDBMaskEdit;
    EditPHONE: TJvDBMaskEdit;
    procedure EditNAMEAcceptNewValue(Sender: TObject;
      oldValue: String; var newValue: String; var Accept, Post: Boolean);
    procedure EditNAMEKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditNAMEExit(Sender: TObject);
  end;

var
  JvDbMaskEditDemoFrm: TJvDbMaskEditDemoFrm;

implementation

{$R *.dfm}

procedure TJvDbMaskEditDemoFrm.EditNAMEAcceptNewValue(Sender: TObject;
  oldValue: String; var newValue: String; var Accept, Post: Boolean);
begin
 if UpperCase(newValue) = 'BOB' then begin
    // you would probably not put up a modal dialog box EVER in a real application, because
    // modal dialog boxes are annoying, but it does make this demo more fun...
    Application.MessageBox('But you hate Bob, you can''t put him in your phone book. I insist. I''m not going to let you do that, Dave.','Your Computer Loves You and Takes Care of You', MB_OK);
    // newValue := 'Dave'; // you could also accept a value, but substitute another value here, via lookup of a code or something.
    Accept := false;
    exit;
 end;
 Post := true; // A valid name is posted automatically, no need to click Post button.
end;

procedure TJvDbMaskEditDemoFrm.EditNAMEKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OutputDebugString(PChar(IntToStr(Key)));
end;

procedure TJvDbMaskEditDemoFrm.EditNAMEExit(Sender: TObject);
begin
  OutputDebugString('OnExit');
end;

end.
