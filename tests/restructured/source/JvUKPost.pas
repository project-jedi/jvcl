{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUKPost.PAS, released on 2002-07-03.

The Initial Developer of the Original Code is Mark Beachill [mark@unacode.com]
Portions created by Mark Beachill are Copyright (C) 2002 Mark Beachill.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

* Ensures input is correct UK postcode format

-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvUKPost;

interface

uses
  Windows, Messages, Classes, Controls, Dialogs, StdCtrls, Sysutils, JVCLVer;

type
  TJvUKPostCodeEdit = class(TEdit)
  private
    fShowMessage: boolean;
    fAllowShortCode: boolean;
    fAllowBlank: boolean;
    FAboutJVCL: TJVCLAboutInfo;

    { Private declarations }

  protected
    { Protected declarations }
    procedure KeyPress(var Key: Char); override;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function IsUKPostCode(S: string; BlankIsOK: boolean): boolean;
    function IsUKShortPostCode(S: string; BlankIsOK: boolean): boolean;
    function GetPostCodeFormat(s: string): string;

  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property AllowBlank: boolean read fAllowBlank write fAllowBlank default false;
    property ShowMessage: boolean read fShowMessage write fShowMessage default True;
    property AllowShortCode: boolean read fAllowShortCode write fAllowShortCode default False;
    function CheckValidPostCode(BlankIsOK: boolean): boolean;
    function CheckValidShortPostCode(BlankIsOK: boolean): boolean;
  end;



const
  Space = #$20;

implementation

constructor TJvUKPostCodeEdit.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  CharCase := ecUpperCase;
  MaxLength := 8;
  ShowMessage := False;
  AllowShortCode := False;
  AllowBlank := true;
end;


procedure TJvUKPostCodeEdit.KeyPress(var Key: Char);
var IsAlpha: boolean;
  IsNumber: boolean;
  IsSpace: boolean;
  IsBackSpace: boolean;
  TextPosition: integer;
begin
  {limits input to vaild chars}
  IsAlpha := Key in ['A'..'Z', 'a'..'z'];
  IsNumber := Key in ['0'..'9'];
  IsSpace := Key in [#32];
  IsbackSpace := Key in [#8];
  TextPosition := length(text);
  if not IsBackSpace then
  begin
    if not (IsAlpha or IsNumber or IsSpace) then
      Key := #0;
    if SelLength = 0 then
    begin
      if (TextPosition = 0) and not IsAlpha then
        Key := #0;
      if (TextPosition = 1) and not (IsAlpha or IsNumber) then
        Key := #0;
      if ((TextPosition = 2) or (TextPosition = 3) or (TextPosition = 4)) and not (IsAlpha or IsNumber or IsSpace) then
        Key := #0;
      if (TextPosition > 4) and not (IsAlpha or IsNumber) then
        Key := #0;
      if (TextPosition > 7) then
        Key := #0;
    end;
  end;
  inherited KeyPress(Key);
end;

function TJvUKPostCodeEdit.CheckValidPostCode(BlankIsOK: boolean): boolean;
begin
  Result := IsUkpostcode(text, BlankIsOK);
end;

function TJvUKPostCodeEdit.CheckValidShortPostCode(BlankIsOK: boolean): boolean;
begin
  Result := IsUkShortPostcode(text, BlankISOK);
end;

procedure TJvUKPostCodeEdit.CMExit(var Message: TCMExit);
var IsValid: boolean;
begin
  if AllowShortCode then
    IsValid := IsUkShortpostcode(text, AllowBlank) or IsUkpostcode(text, AllowBlank)
  else
    IsValid := IsUkpostcode(text, AllowBlank);
  if not IsValid and ShowMessage then
  begin
    {checks UK postcode,error if not ok, refocusses on control}
    MessageDlg('Not a valid postcode', mterror, [mbok], 0);
    self.setfocus;
  end
  else
    {calls usual exit procedure if ok}
    text := trim(text);
  inherited;
end;

function TJvUKPostCodeEdit.IsUKShortPostCode(S: string; BlankIsOK: boolean): boolean;
var
  P: string;
begin
  P := getPostCodeFormat(S);
  Result := (BlankIsOK and (P = '')) or (P = 'A9') or (P = 'A99') or (P = 'AA9') or (P = 'AA99');
end;

function TJvUKPostCodeEdit.IsUKPostCode(S: string; BlankIsOK: boolean): boolean;
var
  P: string;
begin
  P := getPostCodeFormat(S);
  Result := (BlankIsOK and (P = '')) or
    (P = 'A9 9AA') or (P = 'AA9 9AA') or (P = 'AA99 9AA') or (P = 'A99 9AA');
end;

function TJvUKPostCodeEdit.GetPostCodeFormat(s: string): string;
var
  P: string;
  l: integer;
  i: integer;
  c: char;
begin
  p := '';
  l := length(S);
  for i := 1 to l do
  begin
    c := S[i];
    if c = space then
      P := P + ' '
    else if (C in ['A'..'Z', 'a'..'z']) then
      P := P + 'A'
    else if (C in ['0'..'9']) then
      P := P + '9'
    else
      P := P + 'X';
  end;
  result := P;
end;

end.

