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

unit InfoFrm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmInfo = class(TForm)
    reInfo: TRichEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure LoadFile(const Filename: string);
  public
    class procedure View(const Filename, Title: string);
  end;

implementation

{$R *.dfm}


procedure TfrmInfo.LoadFile(const Filename: string);
var S: TStringlist; i: integer;
begin
  if not FileExists(Filename) then
  begin
    reInfo.Lines.Text := Format('"%s": file not found', [Filename]);
    Exit;
  end;
  S := TStringlist.Create;
  try
    S.LoadFromFile(Filename);
    for i := 0 to S.Count - 1 do
      if Pos('EXPORT', S[i]) = 1 then
      begin
        S[i] := Copy(S[i], Pos(#39, S[i]) + 1, MaxInt);
        S[i] := trim(Copy(S[i], 1, Length(S[i]) - 1));
      end
      else
        S[i] := trim(S[i]);
    if S.Count < 2 then
      // tdump.exe errors on some files, such as Shell32.dll
      // When it errors, it outputs less than 2 lines
      S.Add(#13#10'An error occurred running tdump.exe on this DLL')
    else
      S.Insert(2, 'EXPORTED FUNCTIONS:'#13#10'===================');
    reInfo.Lines := S;
    reInfo.SelStart := 0;
    reInfo.Perform(EM_SCROLLCARET, 0, 0);
  finally
    S.Free;
  end;
end;

class procedure TfrmInfo.View(const Filename, Title: string);
var
  frmInfo: TfrmInfo;
  i:integer;
begin
  frmInfo := nil;
  for i := 0 to Screen.FormCount -1 do
    if Screen.Forms[i] is TfrmInfo then
    begin
      frmInfo := TfrmInfo(Screen.Forms[i]);
      Break;
    end;
  if frmInfo = nil then
    frmInfo := self.Create(Application);
  frmInfo.Caption := Format('Viewing content of %s', [Title]);
  frmInfo.LoadFile(Filename);
  frmInfo.Show;
end;

procedure TfrmInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
