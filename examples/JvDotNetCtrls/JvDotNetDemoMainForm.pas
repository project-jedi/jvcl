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

unit JvDotNetDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, ComCtrls, CheckLst, Mask,
  JvDotNetControls, JvDBDotNetControls, JvExForms, JvScrollBox,
  JvExCheckLst, JvCheckListBox, JvExStdCtrls, JvListBox, JvMemo, JvExMask,
  JvToolEdit, JvMaskEdit, JvExComCtrls, JvHotKey, JvEdit;

type
  TJvDotNetDemoMainFrm = class(TForm)
    dxDNCheckListBox1: TJvDotNetCheckListBox;
    JvDotNetDbEdit1: TJvDotNetDBEdit;
    JvDotNetDbListBox1: TJvDotNetDBListBox;
    JvDotNetDbMemo1: TJvDotNetDBMemo;
    JvDotNetDbRichEdit1: TJvDotNetDBRichEdit;
    JvDotNetEdit1: TJvDotNetEdit;
    dxDNHotKey1: TJvDotNetHotKey;
    dxDNListBox1: TJvDotNetListBox;
    JvDotNetMaskEdit1: TJvDotNetMaskEdit;
    JvDotNetMemo1: TJvDotNetMemo;
    dxDNScrollBox1: TJvDotNetScrollBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Image2: TImage;
    Label8: TLabel;
    PageControl1: TPageControl;
    Shape1: TPanel;
    TabSheet4: TTabSheet;
    procedure FormCreate(Sender: TObject);
  end;

var
  JvDotNetDemoMainFrm: TJvDotNetDemoMainFrm;

implementation

{$R *.dfm}

procedure TJvDotNetDemoMainFrm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  PageControl1.ActivePageIndex := 0;
end;

end.








