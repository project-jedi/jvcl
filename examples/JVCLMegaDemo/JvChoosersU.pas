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

unit JvChoosersU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvGammaPanel, JvColorCombo, StdCtrls, JvCombobox, JvComponent,
  JvColorBox, JvLabel, JvDialogs, JvExControls, JvExStdCtrls,
  JvColorButton;

type
  TJvChoosersFrm = class(TForm)
    JvLabel1: TJvLabel;
    JvLabel2: TJvLabel;
    JvLabel3: TJvLabel;
    Label5: TLabel;
    JvLabel4: TJvLabel;
    JvColorButton1: TJvColorButton;
    JvFontCombobox1: TJvFontComboBox;
    JvColorComboBox1: TJvColorComboBox;
    JvGammaPanel1: TJvGammaPanel;
    JvxLabel1: TJvLabel;
    JvColorDialog1: TJvColorDialog;
    Label2: TLabel;
    Label1: TLabel;
    procedure JvFontCombobox1Change(Sender: TObject);
    procedure Label1Click(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TJvChoosersFrm.JvFontCombobox1Change(Sender: TObject);
begin
 JvxLabel1.Font.Name := JvFontCombobox1.Items[JvFontCombobox1.itemindex];
end;

procedure TJvChoosersFrm.Label1Click(Sender: TObject);
begin
 JvColorDialog1.Execute;
end;

end.
