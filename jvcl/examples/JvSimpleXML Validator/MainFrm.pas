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

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, JvComponent, JvSimpleXml, ExtCtrls;

type
  TForm1 = class(TForm)
    JvSimpleXml1: TJvSimpleXml;
    reXML: TRichEdit;
    btnLoad: TButton;
    btnValidate: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    sbResults: TStatusBar;
    chkAutoValidate: TCheckBox;
    procedure btnLoadClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure reXMLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadXML(const Filename:string);
  protected
    { Private declarations }
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses ShellAPI;

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadXML(OpenDialog1.Filename);
end;

procedure TForm1.btnValidateClick(Sender: TObject);
begin
  sbResults.Panels[0].Text := 'Validating...';
  sbResults.Update;
  Screen.Cursor := crHourGlass;
  try
    try
      JvSimpleXML1.LoadFromString(reXML.Lines.Text);
      sbResults.Panels[0].Text := 'No errors encountered in XML';
    except
      on E: Exception do
        sbResults.Panels[0].Text := E.Message;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.reXMLChange(Sender: TObject);
begin
  btnValidate.Enabled := reXML.GetTextLen > 0;
end;

procedure TForm1.WMDropFiles(var Message: TWMDropFiles);
var buf:string;
begin
  SetLength(buf, DragQueryFile(Message.Drop, 0, nil, 0) + 1);
  DragQueryFile(Message.Drop, 0, PChar(buf), Length(buf));
  LoadXML(PChar(buf));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
end;

procedure TForm1.LoadXML(const Filename: string);
begin
  if FileExists(Filename) then
  begin
    reXML.Lines.LoadFromFile(Filename);
    reXMLChange(nil);
    if chkAutoValidate.Checked then
      btnValidate.Click;
  end;
end;

end.

