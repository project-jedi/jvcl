{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWallpaperEditForm.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvWallpaperEditForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Buttons, StdCtrls, Mask,
  JvToolEdit, JvComponent, JvSearchFiles, JvButton, JvExMask;

type
  TFoWallpaperChooser = class(TJvForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DirectoryBox1: TJvDirectoryEdit;
    Label1: TLabel;
    ScrollBox1: TScrollBox;
    SearchFiles1: TJvSearchFiles;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchFile1Found(Sender: TObject; Path: string);
    procedure Button3Click(Sender: TObject);
    procedure DirectoryBox1AfterDialog(Sender: TObject; var Name: string;
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

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{$R *.dfm}

procedure TFoWallpaperChooser.FormCreate(Sender: TObject);
begin
  Image := TPicture.Create;
  FList := TList.Create;
end;

procedure TFoWallpaperChooser.FormDestroy(Sender: TObject);
begin
  Image.Free;
  FList.Free;
end;

procedure TFoWallpaperChooser.SearchFile1Found(Sender: TObject; Path: string);
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

procedure TFoWallpaperChooser.GlyphClick(Sender: TObject);
begin
  Image.Bitmap.Assign((Sender as TSpeedButton).Glyph);
  if Assigned(OnGlyph) then
    OnGlyph(Image.Bitmap);
end;

procedure TFoWallpaperChooser.Button3Click(Sender: TObject);
begin
  Image.Assign(nil);
  if Assigned(OnGlyph) then
    OnGlyph(nil);
end;

procedure TFoWallpaperChooser.DirectoryBox1AfterDialog(Sender: TObject;
  var Name: string; var Action: Boolean);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TSpeedButton(FList.Items[I]).Free;
  FList.Clear;
  FLastBtn := nil;
  { TODO : Test if this works }
  SearchFiles1.RootDirectory := Name;
  SearchFiles1.Search;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
