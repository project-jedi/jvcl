{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormPatch.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFormPatch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, JvSpeedButton, JvToolEdit, Mask;

type
  TfoPatch = class(TForm)
    BUSpeedButton1: TJvSpeedButton;
    BUSpeedButton2: TJvSpeedButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    BUFileNameBox1: TJvFileNameEdit;
    BUFileNameBox2: TJvFileNameEdit;
    procedure BUButton1Click(Sender: TObject);
  private
    FPos: Integer;
    function Crypt(Value: Byte): Byte;
  public
    Res: TStringList;
    procedure LoadFromStr(Value: TStringList);
    function SetFromStr: TStringList;
  end;

implementation

{$R *.DFM}

{*********************************************************************}

procedure TfoPatch.LoadFromStr(Value: TStringList);
begin
  if Value.Count > 2 then
  begin
    BUFileNameBox1.FileName := Value[0];
    BUFileNameBox2.FileName := Value[1];
  end;
end;

{*********************************************************************}

function TfoPatch.SetFromStr: TStringList;
begin
  Result := Res;
end;

{*********************************************************************}

function TfoPatch.Crypt(Value: Byte): Byte;
begin
  if Edit1.Text = '' then
    Result := Value
  else
  begin
    FPos := (FPos + 1) mod Length(Edit1.Text);
    Result := Value xor (Byte(Edit1.Text[FPos + 1]));
  end;
end;

{*********************************************************************}

procedure TfoPatch.BUButton1Click(Sender: TObject);
var
  f, g: file of Byte;
  buf1, buf2: array[0..1024] of Byte;
  i, l: Integer;
  res1, res2: Integer;
  icount, lastcount: Integer;
begin
  FPos := -1;
  Tag := 0;
  Res := TStringList.Create;
  Res.Add(BUFileNameBox1.FileName);
  Res.Add(BUFileNameBox2.FileName);
  AssignFile(f, BUFileNameBox1.FileName);
  AssignFile(g, BUFileNameBox2.FileName);
  Reset(f);
  Reset(g);
  Caption := 'BU - Patcher Editor : Comparing files 0%';
  Application.ProcessMessages;
  l := Res.Add(IntToStr(FileSize(f)));
  Res.Add(IntToStr(FileSize(g)));
  icount := 0;
  lastcount := 0;
  while not Eof(f) and not Eof(g) do
  begin
    Caption := 'BU - Patcher Editor : Comparing files ' + IntToStr(icount div l) + '%';
    Application.ProcessMessages;
    BlockRead(f, buf1, 1024, res1); //f = original file
    BlockRead(g, buf2, 1024, res2); //g = patched file
    if res1 = res2 then
    begin
      for i := 0 to res1 - 1 do
      begin
        Inc(icount);
        if buf1[i] <> buf2[i] then
        begin
          Res.Add(IntToStr(icount - lastcount) + '|' + Char(Crypt(buf2[i])));
          lastcount := icount;
        end;
      end;
    end;
  end;

  Caption := 'BU - Patcher Editor : end step ...';
  Application.ProcessMessages;
  if res1 > res2 then
  begin
    //f>g original>patched
    for i := 0 to res2 - 1 do
    begin
      Inc(icount);
      if buf1[i] <> buf2[i] then
      begin
        Res.Add(IntToStr(icount - lastcount) + '|' + Char(Crypt(buf2[i])));
        lastcount := icount;
      end;
    end;

    //telling it's the end ...
    Res.Add('end%' + IntToStr(icount));
  end
  else if res2 > res1 then
  begin
    //g>f patched>original

    //comparing last bytes
    for i := 0 to res1 - 1 do
    begin
      Inc(icount);
      if buf1[i] <> buf2[i] then
      begin
        Res.Add(IntToStr(icount - lastcount) + '|' + Char(Crypt(buf2[i])));
        lastcount := icount;
      end;
    end;

    //adding the rest
    for i := res1 to res2 - 1 do
      Res.Add(Char(Crypt(buf2[i])));

    //adding the rest of the file
    while not eof(g) do
    begin
      BlockRead(g, buf2, 1024, res2);
      for i := 0 to res2 - 1 do
        Res.Add(Char(Crypt(buf2[i])));
    end;
  end;
  CloseFile(f);
  CloseFile(g);
  Close;
end;

end.
