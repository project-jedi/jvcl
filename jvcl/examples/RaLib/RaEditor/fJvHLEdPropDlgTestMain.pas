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

unit fJvHLEdPropDlgTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, JvButtons, JvEditor, JvHLEditor,
  fJvHLEdPropDlgTestParams, JvComponent, JvHLEditorPropertyForm,
  JvExButtons, JvExControls;

type
  TForm1 = class(TForm)
    RAHLEditor1: TJvHLEditor;
    RAhtButton1: TJvHTButton;
    JvHLEdPropDlg1: TJvHLEdPropDlg;
    procedure RAhtButton1Click(Sender: TObject);
    procedure RAHLEdPropDlg1DialogPopup(Sender: TObject; Form: TForm);
    procedure RAHLEdPropDlg1DialogClosed(Sender: TObject; Form: TForm;
      Apply: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    MyParamsForm: TMyParams ;
    procedure ApplyParams(Form: TMyParams );
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.RAhtButton1Click(Sender: TObject);
begin
  JvHLEdPropDlg1.Execute;
end;

procedure TForm1.RAHLEdPropDlg1DialogPopup(Sender: TObject; Form: TForm);
begin
  MyParamsForm := TMyParams .Create(Self);
  MyParamsForm.Load;
  MyParamsForm.TabSheet1.PageControl := TJvHLEditorParamsForm(Form).Pages;
  MyParamsForm.TabSheet2.PageControl := TJvHLEditorParamsForm(Form).Pages;
  MyParamsForm.TabSheet1.PageIndex := 0;
  MyParamsForm.TabSheet2.PageIndex := 1;
  Form.Caption := 'Properties';
end;

procedure TForm1.RAHLEdPropDlg1DialogClosed(Sender: TObject; Form: TForm;
  Apply: Boolean);
begin
  if Apply then
  begin
    MyParamsForm.Save;
    ApplyParams(MyParamsForm);
  end;
  MyParamsForm.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if JvHLEdPropDlg1.Storage <> nil then
  begin
    JvHLEdPropDlg1.Restore;
    JvHLEdPropDlg1.LoadCurrentHighlighterColors;
  end;

  MyParamsForm := TMyParams .Create(Self);
  try
    MyParamsForm.Load;
    ApplyParams(MyParamsForm);
  finally
    MyParamsForm.Free;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if JvHLEdPropDlg1.Storage <> nil then
    JvHLEdPropDlg1.Save;
end;

procedure TForm1.ApplyParams(Form: TMyParams );
begin
  Caption := Form.GetAppCaption;
end;


end.
