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

unit JvTreeViewAsMenuMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, JvComCtrls, JvComponent, JvCaptionPanel,
  JvExComCtrls;
  
type
  TJvTreeViewAsMenuMainForm = class(TForm)
    JvPageControl1: TJvPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    TabSheet2: TTabSheet;
    ListBox1: TListBox;
    TabSheet3: TTabSheet;
    ListBox2: TListBox;
    TabSheet4: TTabSheet;
    RadioGroup1: TRadioGroup;
    TabSheet5: TTabSheet;
    Panel1: TPanel;
    JvTreeView1: TJvTreeView;
    Label1: TLabel;
    procedure JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode; Page: TTabSheet);
  end;

var
  JvTreeViewAsMenuMainForm: TJvTreeViewAsMenuMainForm;

implementation

{$R *.DFM}

procedure TJvTreeViewAsMenuMainForm.JvTreeView1PageChanged(Sender: TObject; Item: TTreeNode;
  Page: TTabSheet);
begin
  Caption := Item.Text+' - '+Page.Caption;
end;

end.
