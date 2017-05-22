{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.delphi-jedi.org

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

unit JvPanelDemoFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvPanel, StdCtrls, ComCtrls, Mask, JvToolEdit,
  JvComponent, JvFormPlacement, JvExExtCtrls, JvExMask, JvComponentBase, JvExtComponent;

type
  TJvPanelDemoMainFrm = class(TForm)
    JvPanel1: TJvPanel;
    Edit1: TEdit;
    JvFilenameEdit1: TJvFilenameEdit;
    JvFormStorage1: TJvFormStorage;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure JvPanel1Resize(Sender: TObject);
  end;

var
  JvPanelDemoMainFrm: TJvPanelDemoMainFrm;

implementation

{$R *.DFM}

procedure TJvPanelDemoMainFrm.CheckBox1Click(Sender: TObject);
begin
  JvPanel1.Transparent := CheckBox1.Checked;
end;

procedure TJvPanelDemoMainFrm.CheckBox2Click(Sender: TObject);
begin
  JvPanel1.ArrangeSettings.AutoArrange := CheckBox2.Checked;
end;

procedure TJvPanelDemoMainFrm.JvPanel1Resize(Sender: TObject);
begin
  JvPanel1.Caption := Format('JvPanel Demo'#13#10+
  'Constraint Width %d - %d / Height %d - %d'#13#10+
  'Width %d / Height %d', [JvPanel1.Constraints.MinWidth, JvPanel1.Constraints.MaxWidth,
                           JvPanel1.Constraints.MinHeight, JvPanel1.Constraints.MaxHeight,
                           JvPanel1.Width, JvPanel1.Height]);
end;

end.
