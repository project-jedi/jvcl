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
  Dialogs, JvEditor, StdCtrls, JvComponent, JvSimpleXml, JvDialogs,
  ComCtrls, JvComCtrls, JvExComCtrls;

type
  TForm1 = class(TForm)
    JvSimpleXml1: TJvSimpleXml;
    Label1: TLabel;
    edXMLFile: TEdit;
    Button1: TButton;
    JvOpenDialog1: TJvOpenDialog;
    JvTreeView1: TJvTreeView;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure LoadFromFile(const Filename: string);
    procedure ParseIntoTreeView(AnXMLNode: TJvSimpleXmlElem; ATreeNode: TTreeNode);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvOpenDialog1.FileName := edXMLFile.Text;
  if JvOpenDialog1.Execute then
  begin
    LoadFromFile(JvOpenDialog1.FileName);
    edXMLFile.Text := JvOpenDialog1.FileName;
  end;
end;

procedure TForm1.ParseIntoTreeView(AnXMLNode: TJvSimpleXmlElem; ATreeNode: TTreeNode);
var
  i, j: integer;
  S, T: string;
begin
  if AnXMLNode <> nil then
  begin
    if AnXMLNode.Value <> '' then
      S := AnXMLNode.Name + '=' + AnXMLNode.Value
    else
      S := AnXMLNode.Name;
    T := '';
    for j := 0 to AnXMLNode.Properties.Count - 1 do
      T := T + ' ' + AnXMLNode.Properties[j].Name + '="' + AnXMLNode.Properties[j].Value + '"';
    ATreeNode := JvTreeView1.Items.AddChild(ATreeNode, S + ' (' + trim(T) + ')');
    for i := 0 to AnXMLNode.Items.Count - 1 do
      ParseIntoTreeView(AnXMLNode.Items[i], ATreeNode);
  end;
end;

procedure TForm1.LoadFromFile(const Filename: string);
begin
  Screen.Cursor := crHourGlass;
  Enabled := false;
  try
    JvTreeView1.Items.BeginUpdate;
    try
      JvSimpleXML1.LoadFromFile(Filename);
      JvTreeView1.Items.Clear;
      ParseIntoTreeView(JvSimpleXML1.Root, JvTreeView1.Items.Add(nil, ExtractFilename(Filename)));
    finally
      JvTreeView1.Items.EndUpdate;
    end;
    JvTreeView1.FullExpand;
  finally
    Screen.Cursor := crDefault;
    Enabled := true;
  end;

end;

end.

