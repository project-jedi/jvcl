{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageListBox.PAS, released on 2001-02-28.

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

unit JvImageListBox;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Imglist,
  JvListBox2;

type
  TJvImageListBox = class(TJvListbox2)
  private
    FImage: TImageList;
    procedure SetImage(const Value: TImageList);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  published
    property Images: TImageList read FImage write SetImage;
  end;

implementation

{***************************************************}

constructor TJvImageListBox.Create(AOwner: TComponent);
begin
  inherited;
  Style := lbOwnerDrawVariable;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

{***************************************************}

procedure TJvImageListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  FBmp: TBitmap;
  d: TDrawingStyle;
begin
  if FImage <> nil then
  begin
    d := FImage.DrawingStyle;
    if (odSelected in State) or (odGrayed in State) or (odDisabled in State) or
      (odChecked in State) or (odFocused in State) then
      FImage.DrawingStyle := dsFocus;
    FBmp := TBitMap.Create;
    FImage.GetBitmap(index, FBmp);
    ItemHeight := FBmp.Height;
    with Canvas do
    begin
      FillRect(Rect);
      Draw(Rect.Left - 1, Rect.Top + 0, FBmp);
      TextOut(Rect.Left + 2 + FBmp.Width, Rect.Top + 2, Items.Strings[Index]);
    end;
    FBmp.Free;
    FImage.DrawingStyle := d;
  end;
end;

{***************************************************}

procedure TJvImageListBox.SetImage(const Value: TImageList);
begin
  FImage := Value;
  Invalidate;
end;

end.
