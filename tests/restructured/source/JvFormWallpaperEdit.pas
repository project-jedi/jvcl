{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFormWallpaperEdit.PAS, released on 2001-02-28.

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

unit JvFormWallpaperEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls,
  JvSpeedButton, JvToolEdit, JvComponent, Mask, JvSearchFiles;

type
  TfoWallpaperChooser = class(TForm)
    GroupBox1: TGroupBox;
    BUSpeedButton1: TJvSpeedButton;
    BUSpeedButton2: TJvSpeedButton;
    BUDirectoryBox1: TJvDirectoryEdit;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    BUSpeedButton3: TJvSpeedButton;
    BUSearchFiles1: TJvSearchFiles;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BUSearchFile1Found(Sender: TObject; Path: string);
    procedure BUSpeedButton3Click(Sender: TObject);
    procedure BUDirectoryBox1AfterDialog(Sender: TObject; var Name: String;
      var Action: Boolean);
  private
    FList: TList;
    FLastBtn: TSpeedButton;
    FMaxHeight: Integer;
    procedure GlyphClick(Sender: TObject);
  public
    Image: TPicture;
    OnGlyph: TNotifyEvent;
  end;

implementation

{$R *.DFM}

{******************************************************}

procedure TfoWallpaperChooser.FormCreate(Sender: TObject);
begin
  Image := TPicture.Create;
  FList := TList.Create;
end;

{******************************************************}

procedure TfoWallpaperChooser.FormDestroy(Sender: TObject);
begin
  Image.Free;
  FList.Free;
end;

{******************************************************}


{******************************************************}

procedure TfoWallpaperChooser.BUSearchFile1Found(Sender: TObject;
  Path: string);
var
  Btn: TSpeedButton;
begin
  Btn := TSpeedButton.Create(ScrollBox1);
  Btn.Parent := ScrollBox1;
  Btn.Flat := True;
  Btn.ShowHint := True;
  Btn.Hint := ChangeFileExt(ExtractFileName(Path), '');
  try
    Btn.Glyph.LoadFromFile(Path);
    Btn.Width := Btn.Glyph.Width + 2;
    Btn.Height := Btn.Glyph.Height + 2;
  except
    Btn.Free;
    Exit;
  end;
  if FLastBtn = nil then
  begin
    Btn.Left := 0;
    Btn.Top := 0;
    FMaxHeight := 0;
  end
  else
  begin
    if FLastBtn.Left + FLastBtn.Width + Btn.Width + 20 > ScrollBox1.Width then
    begin
      Btn.Left := 0;
      Btn.Top := FLastBtn.Top + FMaxHeight;
      FMaxHeight := 0;
    end
    else
    begin
      Btn.Left := FLastBtn.Left + FLastBtn.Width;
      Btn.Top := FLastBtn.Top;
    end;
  end;
  FLastBtn := Btn;
  Btn.OnClick := GlyphClick;
  FList.Add(Btn);
  if Btn.Height > FMaxHeight then
    FMaxHeight := Btn.Height;
end;

{******************************************************}

procedure TfoWallpaperChooser.GlyphClick(Sender: TObject);
begin
  Image.Bitmap.Assign((Sender as TSpeedButton).Glyph);
  if Assigned(OnGlyph) then
    OnGlyph(Image.Bitmap);
end;

{******************************************************}

procedure TfoWallpaperChooser.BUSpeedButton3Click(Sender: TObject);
begin
  Image.Assign(nil);
  if Assigned(OnGlyph) then
    OnGlyph(nil);
end;

procedure TfoWallpaperChooser.BUDirectoryBox1AfterDialog(Sender: TObject;
  var Name: String; var Action: Boolean);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    TSpeedButton(FList.Items[i]).Free;
  FList.Clear;
  FLastBtn := nil;
  { TODO : Test if this works }
  BUSearchFiles1.RootDirectory := Name;
  BUSearchFiles1.Search;
end;

end.
