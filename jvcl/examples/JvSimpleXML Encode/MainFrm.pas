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
{$I jvcl.inc}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, JvSimpleXML, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    pnlBottom: TPanel;
    Label2: TLabel;
    reResult: TMemo;
    btnCopy: TButton;
    pnlTop: TPanel;
    Label1: TLabel;
    reSource: TMemo;
    chkTrim: TCheckBox;
    btnEncode: TButton;
    btnDecode: TButton;
    Bevel1: TBevel;
    chkUseUTF8: TCheckBox;
    StatusBar1: TStatusBar;
    chkUseClipboard: TCheckBox;
    procedure btnEncodeClick(Sender: TObject);
    procedure btnDecodeClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure DisplayTime(MS: Cardinal);
  end;

var
  Form1: TForm1;

implementation
uses
  ShellAPI, JvJCLUtils, JvJVCLUtils, Clipbrd;

{$R *.dfm}

procedure TForm1.btnEncodeClick(Sender: TObject);
var
  S: string;
  FStartValue: Cardinal;
begin
  WaitCursor;
  reSource.Lines.BeginUpdate;
  reResult.Lines.BeginUpdate;
  try
  // assign to S to take the visual control out of the equation
    {$IFDEF COMPILER6_UP}
    if chkUseUTF8.Checked then
    begin
      if chkUseClipboard.Checked then
      begin
        reSource.CopyToClipboard;
        S := Clipboard.AsText;
      end
      else
        S := reSource.Lines.Text;
      FStartValue := GetTickCount;
      S := UTF8Encode(S);
      FStartValue := GetTickCount - FStartValue;
      if chkUseClipboard.Checked then
      begin
        Clipboard.AsText := S;
        reSource.PasteFromClipboard;
      end
      else
        reResult.Lines.Text := S;
    end
    else
   {$ENDIF}
    begin
      if chkUseClipboard.Checked then
      begin
        reSource.CopyToClipboard;
        S := Clipboard.AsText;
      end
      else
        S := reSource.Lines.Text;
      FStartValue := GetTickCount;
      S := XMLEncode(S);
      FStartValue := GetTickCount - FStartValue;
      if chkUseClipboard.Checked then
      begin
        Clipboard.AsText := S;
        reSource.PasteFromClipboard;
      end
      else
        reResult.Lines.Text := S;
    end;
    DisplayTime(FStartValue);
  finally
    reSource.Lines.EndUpdate;
    reResult.Lines.EndUpdate;
  end;
end;

procedure TForm1.btnDecodeClick(Sender: TObject);
var
  S: string;
  FStartValue: Cardinal;
begin
  WaitCursor;
  // assign to S to take the visual control out of the equation
  reSource.Lines.BeginUpdate;
  reResult.Lines.BeginUpdate;
  try
    {$IFDEF COMPILER6_UP}
    if chkUseUTF8.Checked then
    begin
      S := reSource.Lines.Text;
      FStartValue := GetTickCount;
      S := UTF8Decode(S);
      FStartValue := GetTickCount - FStartValue;
      reResult.Lines.Text := S;
    end
    else
    {$ENDIF}
    begin
      S := reSource.Lines.Text;
      FStartValue := GetTickCount;
      SimpleXMLDecode(S, chkTrim.Checked);
      FStartValue := GetTickCount - FStartValue;
      reResult.Lines.Text := S;
    end;
    DisplayTime(FStartValue);
  finally
    reSource.Lines.EndUpdate;
    reResult.Lines.EndUpdate;
  end;
end;

procedure TForm1.btnCopyClick(Sender: TObject);
begin
  reSource.Lines := reResult.Lines;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, true);
  {$IFNDEF COMPILER6_UP}
  chkUseUTF8.Enabled := false;
  {$ENDIF}
end;

procedure TForm1.WMDropFiles(var Message: TWMDropFiles);
var
  i, Count: integer;
  FileList: TStringlist;
  FileBuf: array[0..MAX_PATH] of char;

  function StringFromFile(const FileName: string): string;
  begin
    with TFileStream.Create(FileName, fmOpenRead) do
    try
      SetLength(Result, Size);
      if Size > 0 then
        Read(Result[1], Size);
    finally
      Free;
    end;
  end;

begin
  Count := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
  try
    if Count > 0 then
    begin
      FileList := TStringlist.Create;
      try
        for i := 0 to Count - 1 do
        begin
          DragQueryFile(Message.Drop, i, FileBuf, sizeof(FileBuf));
          FileList.Add(FileBuf);
        end;
        FileList.Sort;
        reSource.Lines.BeginUpdate;
        try
          reSource.Lines.Clear;
          for i := 0 to FileList.Count - 1 do
            reSource.Lines.Add(StringFromFile(FileList[i]));
        finally
          reSource.Lines.EndUpdate;
        end;
      finally
        FileList.Free;
      end;
    end;
  finally
    DragFinish(Message.Drop);
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pnlBottom.Height := ClientHeight div 2;
end;

procedure TForm1.DisplayTime(MS: Cardinal);
var
  kB: Cardinal;
begin
  kB := reSource.GetTextLen div 1024;
  if (kB = 0) then Exit;
  if MS = 0 then MS := 1;
  StatusBar1.Panels[0].Text := Format('Conversion of %dkB took %d msecs -> %f kB/sec', [kB, MS, kB / MS * 1000]);
end;

end.

