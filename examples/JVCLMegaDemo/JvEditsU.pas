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

unit JvEditsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvEdit, JvValidateEdit, Mask,
  ExtCtrls, JvComponent, JvCaptionPanel, JvComCtrls, JvMaskEdit, JvSpin,
  JvExControls, JvToolEdit, JvBaseEdits, JvExStdCtrls, JvExMask;

type
  TJvEdits = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label3: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label8: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label23: TLabel;
    Label25: TLabel;
    Label28: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    JvFileNameBox1: TJvFilenameEdit;
    JvButtonBox1: TJvComboEdit;
    JvFloatEdit1: TJvValidateEdit;
    JvCalculatorBox1: TJvCalcEdit;
    JvCurrencyEdit1: TJvValidateEdit;
    JvFloatEdit21: TJvValidateEdit;
    JvIntegerEdit1: TJvValidateEdit;
    JvYearEdit1: TJvValidateEdit;
    JvDirectoryEdit1: TJvDirectoryEdit;
    JvDateEdit1: TJvDateEdit;
    JvSpinEdit1: TJvSpinEdit;
    JvIpAddress1: TJvIpAddress;
    procedure JvButtonBox1ButtonClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TJvEdits.JvButtonBox1ButtonClick(Sender: TObject);
begin
  ShowMessage('Here you can do something :)');
  jvButtonBox1.Text := 'Something done !';
end;

end.
