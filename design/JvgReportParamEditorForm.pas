{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgReportParamEditorForm.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgReportParamEditorForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, Mask;

type
  TJvgReportParamEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    BitBtn1: TBitBtn;
    rgParameterType: TRadioGroup;
    Notebook: TNotebook;
    gbTextMask: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    meTestMask: TMaskEdit;
    eTextMask: TEdit;
    gbRadioItems: TGroupBox;
    Label5: TLabel;
    lbRadioItems: TListBox;
    pbAddItem: TButton;
    pbDeleteItem: TButton;
    eItemToAdd: TEdit;
    pbInsertItem: TButton;
    gbCheckbox: TGroupBox;
    Label7: TLabel;
    eCheckBox: TEdit;
    gbDataSource: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    eTableName: TEdit;
    eFieldName: TEdit;
    procedure rgParameterTypeClick(Sender: TObject);
    procedure pbAddItemClick(Sender: TObject);
    procedure lbRadioItemsClick(Sender: TObject);
    procedure eTextMaskChange(Sender: TObject);
    procedure pbDeleteItemClick(Sender: TObject);
  public
  end;

//var
//  ReportParamEditor: TJvgReportParamEditor;

implementation

{$R *.dfm}

{procedure SetEnabledState(WC: TWinControl; State: Boolean);
var
  I: Integer;
begin
  for I :=0 to WC.ControlCount-1 do
    WC.Controls[I].Enabled := State;
end;}

procedure TJvgReportParamEditor.rgParameterTypeClick(Sender: TObject);
begin
  Notebook.PageIndex := rgParameterType.ItemIndex;
end;

procedure TJvgReportParamEditor.pbAddItemClick(Sender: TObject);
begin
  eItemToAdd.Text := Trim(eItemToAdd.Text);
  if Length(eItemToAdd.Text) > 0 then
    if TButton(Sender).Tag = 0 then
      lbRadioItems.Items.Append(eItemToAdd.Text)
    else
    if lbRadioItems.ItemIndex <> -1 then
      lbRadioItems.Items.Insert(lbRadioItems.ItemIndex, eItemToAdd.Text);
end;

procedure TJvgReportParamEditor.lbRadioItemsClick(Sender: TObject);
begin
  pbDeleteItem.Enabled := lbRadioItems.ItemIndex <> -1;
  pbInsertItem.Enabled := pbDeleteItem.Enabled;
end;

procedure TJvgReportParamEditor.eTextMaskChange(Sender: TObject);
begin
  meTestMask.EditMask := eTextMask.Text;
end;

procedure TJvgReportParamEditor.pbDeleteItemClick(Sender: TObject);
begin
  lbRadioItems.Items.Delete(lbRadioItems.ItemIndex);
end;

end.
