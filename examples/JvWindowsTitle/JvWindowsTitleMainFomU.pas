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

unit JvWindowsTitleMainFomU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TJvWindowsTitleMainForm = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  end;

var
  JvWindowsTitleMainForm: TJvWindowsTitleMainForm;

implementation
uses
  JvJCLUtils;

{$R *.DFM}

procedure TJvWindowsTitleMainForm.Button1Click(Sender: TObject);
var
  S : TStringlist;
  i : integer;
begin
  S := TStringlist.Create;
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;
    GetVisibleWindows(S);
    for i := 0 to S.Count - 1 do
      ListBox1.Items.Add(Format('%s (%d)',[S[i],integer(S.Objects[i])]));
  finally
    ListBox1.Items.EndUpdate;
    S.Free;
  end;
end;

end.
