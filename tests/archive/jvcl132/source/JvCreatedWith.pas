{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCreatedWith.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvCreatedWith;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, JVCLVer;

type
  // (rom) TJvIm type simplified
  TJvIm = 1..15;
  TJvCreatedWith = class(TImage)
  private
    FIm: TJvIm;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetIm(Value: TJvIm);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property ImageNumber: TJvIm read FIm write SetIm default 1;
  end;

implementation

{$R RES_CreatedWith.res}

{************************************************************}

constructor TJvCreatedWith.Create(AOwner: TComponent);
begin
  inherited;
  FIm := 1;
  SetIm(Fim);
  if csDesigning in ComponentState then
    Self.AutoSize := True;
end;

{************************************************************}

procedure TJvCreatedWith.SetIm(Value: TJvIm);
var
  s: string;
begin
  // (rom) simplified!
  FIm := Value;
  s := 'D' + IntToStr(FIm);
  Picture.Bitmap.Handle := LoadBitmap(HInstance, PChar(s));
end;

end.
