{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPatchForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQPatchForm;

interface

uses
  SysUtils, Classes,  
  QControls, QForms, QStdCtrls, QMask, 
  JvQToolEdit, JvQComponent, JvQExMask;

type
  TPatchFrm = class(TJvForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edPassword: TEdit;
    edSource: TJvFilenameEdit;
    edDest: TJvFilenameEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    ClearBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
  private
    FPos: Integer;
    function Crypt(Value: Byte): Byte;
  public
    FPatch: TStringList;
    procedure LoadFromStr(Value: TStringList);
    function SetFromStr: TStringList;
  end;

implementation

uses  
  QDialogs,  
  JvQConsts, JvQResources;



{$R *.xfm}


procedure TPatchFrm.LoadFromStr(Value: TStringList);
begin
  if Value.Count > 2 then
  begin
    edSource.FileName := Value[0];
    edDest.FileName := Value[1];
  end;
end;

function TPatchFrm.SetFromStr: TStringList;
begin
  Result := FPatch;
end;

function TPatchFrm.Crypt(Value: Byte): Byte;
begin
  if edPassword.Text = '' then
    Result := Value
  else
  begin
    FPos := (FPos + 1) mod Length(edPassword.Text);
    Result := Value xor (Byte(edPassword.Text[FPos + 1]));
  end;
end;

// (rom) needs modernizing

procedure TPatchFrm.OkBtnClick(Sender: TObject);
var
  Src, Dest: TFileStream;
  buf1, buf2: array [0..1023] of Byte;
  i, j: Integer;
  res1, res2: Integer;
  iCount, LastCount: Integer;
begin
  if not FileExists(edSource.FileName) or not FileExists(edDest.FileName) then
  begin
    ModalResult := mrNone;
    MessageDlg(RsErrJvPatcherEditorInvalidFilename, mtError, [mbOK], 0);
    Exit;
  end;
  Src := TFileStream.Create(edSource.FileName, fmOpenRead or fmShareDenyNone);
  Dest := TFileStream.Create(edDest.FileName, fmOpenRead or fmShareDenyNone);
  try
    res1 := 0;
    res2 := 0;
    FPos := -1;
    Tag := 0;

    FPatch.Clear;
    FPatch.Add(edSource.FileName);
    FPatch.Add(edDest.FileName);
    Caption := Format(RsJvPatcherEditorComparingFilesd, [0]);
    Repaint;
    j := FPatch.Add(IntToStr(Src.Size));
    FPatch.Add(IntToStr(Dest.Size));
    iCount := 0;
    LastCount := 0;
    while (Src.Position < Src.Size) and (Dest.Position < Dest.Size) do
    begin
      Caption := Format(RsJvPatcherEditorComparingFilesd, [iCount div j]);
      Application.ProcessMessages;
      res1 := Src.Read(buf1, sizeof(buf1)); // original file
      res2 := Dest.Read(buf2, sizeof(buf2)); // patched file
      if res1 = res2 then
      begin
        for i := 0 to res1 - 1 do
        begin
          Inc(iCount);
          if buf1[i] <> buf2[i] then
          begin
            FPatch.Add(IntToStr(iCount - LastCount) + '|' + Char(Crypt(buf2[i])));
            LastCount := iCount;
          end;
        end;
      end;
    end;

    Caption := RsJvPatcherEditorEndStep;
    Repaint;
    if res1 > res2 then
    begin
      //f>g original>patched
      for i := 0 to res2 - 1 do
      begin
        Inc(iCount);
        if buf1[i] <> buf2[i] then
        begin
          FPatch.Add(IntToStr(iCount - LastCount) + '|' + Char(Crypt(buf2[i])));
          LastCount := iCount;
        end;
      end;

      //telling it's the end ...
      FPatch.Add('end%' + IntToStr(iCount));
    end
    else
    if res2 > res1 then
    begin
      //g>f patched>original

      //comparing last bytes
      for i := 0 to res1 - 1 do
      begin
        Inc(iCount);
        if buf1[i] <> buf2[i] then
        begin
          FPatch.Add(IntToStr(iCount - LastCount) + '|' + Char(Crypt(buf2[i])));
          LastCount := iCount;
        end;
      end;

      //adding the rest
      for i := res1 to res2 - 1 do
        FPatch.Add(Char(Crypt(buf2[i])));

      //adding the rest of the file
      while Dest.Position < Dest.Size do
      begin
        res2 := Dest.Read(buf2, sizeof(buf2));
        for i := 0 to res2 - 1 do
          FPatch.Add(Char(Crypt(buf2[i])));
      end;
    end;
  finally
    Src.Free;
    Dest.Free;
  end;
  // Close;
end;

procedure TPatchFrm.FormCreate(Sender: TObject);
begin
  FPatch := TStringList.Create;
end;

procedure TPatchFrm.FormDestroy(Sender: TObject);
begin
  FPatch.Free;
end;

procedure TPatchFrm.ClearBtnClick(Sender: TObject);
begin
  FPatch.Clear;
end;

end.

