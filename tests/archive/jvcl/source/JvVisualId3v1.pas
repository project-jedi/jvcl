{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVisualId3v1.PAS, released on 2001-02-28.

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

unit JvVisualId3v1;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  JvTypes, JvId3v1, JVCLVer;

type
  TJvId3v1Tag = class(TPersistent)
  private
    FAlbum: string;
    FComment: string;
    FArtist: string;
    FSongName: string;
    FYear: string;
    FGenre: TGenre;
    FOnChange: TNotifyEvent;
    procedure SetAlbum(const Value: string);
    procedure SetArtist(const Value: string);
    procedure SetComment(const Value: string);
    procedure SetGenre(const Value: TGenre);
    procedure SetSongName(const Value: string);
    procedure SetYear(const Value: string);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent);
  published
    property Album: string read FAlbum write SetAlbum;
    property Artist: string read FArtist write SetArtist;
    property Comment: string read FComment write SetComment;
    property Genre: TGenre read FGenre write SetGenre;
    property SongName: string read FSongName write SetSongName;
    property Year: string read FYear write SetYear;
  end;

  TJvVisualId3v1 = class(TWinControl)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FFileName: TFileName;
    FLabelList: array [1..6] of TLabel;
    FEditList: array [1..5] of TEdit;
    FCombo1: TComboBox;
    FId3v1: TJvId3v1;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FId3Tag: TJvId3v1Tag;
    FEditFont: TFont;
    FLabelFont: TFont;
    procedure SetFileName(const Value: TFileName);
    procedure SetReadOnly(const Value: Boolean);
    procedure Changed(Sender: TObject);
    procedure UserChanged(Sender: TObject);
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    function GetEditColor: TColor;
    procedure SetEditColor(const Value: TColor);
    procedure SetEditFont(const Value: TFont);
    procedure SetLabelFont(const Value: TFont);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure RemoveTagFromFile(FileName: string);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property Align;
    property Color;
    property EditColor: TColor read GetEditColor write SetEditColor;
    property EditFont: TFont read FEditFont write SetEditFont;
    property FileName: TFileName read FFileName write SetFileName;
    property Id3Tag: TJvId3v1Tag read FId3Tag write FId3Tag;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

//=== TJvVisualId3v1 =========================================================

constructor TJvVisualId3v1.Create(AOwner: TComponent);
const
  CaptionLabels: array [1..6] of PChar =
    ('Artist', 'SongName', 'Album', 'Year', 'Comment', 'Genre');
var
  I: Byte;
  J: Integer;
  St: string;
begin
  inherited Create(AOwner);
  Width := 229;
  Height := 160;
  FReadOnly := True;

  FEditFont := TFont.Create;
  FLabelFont := TFont.Create;

  // (rom) arrays where arrays are due
  for J := Low(FLabelList) to High(FLabelList) do
  begin
    FLabelList[J] := TLabel.Create(Self);
    FLabelList[J].Caption := CaptionLabels[J];
    FLabelList[J].Parent := Self;
  end;
  for J := Low(FEditList) to High(FEditList) do
  begin
    FEditList[J] := TEdit.Create(Self);
    FEditList[J].Text := '';
    FEditList[J].Enabled := False;
    FEditList[J].MaxLength := 30;
    FEditList[J].Parent := Self;
    FEditList[J].OnChange := UserChanged;
  end;
  FEditList[4].MaxLength := 4;

  FCombo1 := TCombobox.Create(Self);
  FCombo1.Text := '';
  FCombo1.Style := csDropDownList;
  FCombo1.Parent := Self;
  FCombo1.Enabled := False;

  // (rom) Huh?
  Self.Parent := TWinControl(AOwner);

  FId3Tag := TJvId3v1Tag.Create(Self);
  FId3Tag.OnChange := Changed;

  FId3v1 := TJvId3v1.Create(Self);
  for I := Integer(Low(TGenre)) to Integer(High(TGenre)) do
  begin
    St := FId3v1.GenreToString(TGenre(I));
    if St <> '' then
      FCombo1.Items.Add(St);
  end;
  FEditFont.OnChange := FontChanged;
  FLabelFont.OnChange := FontChanged;
  FontChanged(Self);
end;

destructor TJvVisualId3v1.Destroy;
begin
  // (rom) removed Free of components
  FEditFont.Free;
  FLabelFont.Free;
  inherited Destroy;
end;

procedure TJvVisualId3v1.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  FEditList[1].Text := FId3Tag.Artist;
  FEditList[2].Text := FId3Tag.SongName;
  FEditList[3].Text := FId3Tag.Album;
  FEditList[4].Text := FId3Tag.Year;
  FEditList[5].Text := FId3Tag.Comment;
  FCombo1.ItemIndex := Byte(FId3Tag.Genre);
end;

procedure TJvVisualId3v1.FontChanged(Sender: TObject);
var
  I: Integer;
  LabelHeight, LabelWidth: Integer;
begin
  for I := Low(FLabelList) to High(FLabelList) do
    FLabelList[I].Font := FLabelFont;
  for I := Low(FEditList) to High(FEditList) do
    FEditList[I].Font := FEditFont;
  FCombo1.Font := FEditList[Low(FEditList)].Font;

  LabelHeight := 0;
  for I := Low(FLabelList) to High(FLabelList) do
    if FLabelList[I].Height > LabelHeight then
      LabelHeight := FLabelList[I].Height;

  LabelWidth := 0;
  for I := Low(FLabelList) to High(FLabelList) do
    if FLabelList[I].Width > LabelWidth then
      LabelWidth := FLabelList[I].Width;

  for I := Low(FLabelList) to High(FLabelList) do
    FLabelList[I].Left := 5;

  for I := Low(FEditList) to High(FEditList) do
    FEditList[I].Left := LabelWidth + FLabelList[Low(FLabelList)].Left + 10;
  FCombo1.Left := LabelWidth + FLabelList[Low(FLabelList)].Left + 10;

  if LabelHeight > FEditList[Low(FEditList)].Height then
  begin
    FLabelList[Low(FLabelList)].Top := 5;
    for I := Low(FLabelList)+1 to High(FLabelList) do
      FLabelList[I].Top := FLabelList[I-1].Top + LabelHeight + 5;

    for I := Low(FEditList) to High(FEditList) do
      FEditList[I].Top := FLabelList[I].Top +
        (LabelHeight - FEditList[Low(FEditList)].Height) div 2;
    FCombo1.Top := FLabelList[High(FLabelList)].Top +
      (LabelHeight - FEditList[Low(FEditList)].Height) div 2;
  end
  else
  begin
    FEditList[Low(FEditList)].Top := 5;
    for I := Low(FEditList)+1 to High(FEditList) do
      FEditList[I].Top := FEditList[I-1].Top + FEditList[I-1].Height + 5;

    { (rb) Copy-paste error: ?? }
    {for I := Low(FEditList) to High(FEditList) do
      FEditList[I].Top := FLabelList[I].Top +
        (LabelHeight - FEditList[Low(FEditList)].Height) div 2;}
    FCombo1.Top := FEditList[High(FEditList)].Top + FEditList[High(FEditList)].Height + 5;

    for I := Low(FLabelList) to High(FLabelList)-1 do
      FLabelList[I].Top := FEditList[I].Top + (FEditList[I].Height - LabelHeight) div 2;
    FLabelList[High(FLabelList)].Top := FCombo1.Top +
      (FEditList[Low(FEditList)].Height - LabelHeight) div 2;
  end;

  Self.Width := Self.Width + 1;
  Self.Width := Self.Width - 1;
end;

function TJvVisualId3v1.GetEditColor: TColor;
begin
  Result := FEditList[Low(FEditList)].Color;
end;

procedure TJvVisualId3v1.LoadFromFile(FileName: string);
begin
  FId3v1.FileName := FileName;

  FId3Tag.Album := FId3v1.Album;
  FId3Tag.Artist := FId3v1.Artist;
  FId3Tag.Comment := FId3v1.Comment;
  FId3Tag.Year := FId3v1.Year;
  FId3Tag.SongName := FId3v1.SongName;
  FId3Tag.Genre := FId3v1.Genre;
end;

procedure TJvVisualId3v1.RemoveTagFromFile(FileName: string);
begin
  FId3v1.RemoveTag(FileName);
end;

procedure TJvVisualId3v1.SaveToFile(FileName: string);
begin
  with FId3Tag do
    FId3v1.WriteTag(FileName, SongName, Artist, Album, Year, Comment, Genre);
end;

procedure TJvVisualId3v1.SetEditColor(const Value: TColor);
var
  I: Integer;
begin
  for I := Low(FEditList) to High(FEditList) do
    FEditList[I].Color := Value;
  FCombo1.Color := Value;
end;

procedure TJvVisualId3v1.SetEditFont(const Value: TFont);
var
  I: Integer;
begin
  for I := Low(FEditList) to High(FEditList) do
    FEditList[I].Font := Value;
  FEditFont := Value;
  FCombo1.Font := Value;
end;

procedure TJvVisualId3v1.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  FId3v1.FileName := Value;

  FId3Tag.Album := Trim(FId3v1.Album);
  FId3Tag.Artist := Trim(FId3v1.Artist);
  FId3Tag.Comment := Trim(FId3v1.Comment);
  FId3Tag.Year := Trim(FId3v1.Year);
  FId3Tag.SongName := Trim(FId3v1.SongName);
  FId3Tag.Genre := FId3v1.Genre;
end;

procedure TJvVisualId3v1.SetLabelFont(const Value: TFont);
var
  I: Integer;
begin
  for I := Low(FLabelList) to High(FLabelList) do
    FLabelList[I].Font := Value;
  FLabelFont := Value;
end;

procedure TJvVisualId3v1.SetReadOnly(const Value: Boolean);
var
  I: Integer;
begin
  FReadOnly := Value;
  for I := Low(FEditList) to High(FEditList) do
    FEditList[I].Enabled := not FReadOnly;
  FCombo1.Enabled := not FReadOnly;
end;

procedure TJvVisualId3v1.UserChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);

  FId3Tag.FArtist := FEditList[1].Text;
  FId3Tag.FSongName := FEditList[2].Text;
  FId3Tag.FAlbum := FEditList[3].Text;
  FId3Tag.FYear := FEditList[4].Text;
  FId3Tag.FComment := FEditList[5].Text;
  FId3Tag.FGenre := TGenre(FCombo1.ItemIndex);
end;

procedure TJvVisualId3v1.WMSize(var Msg: TWMSize);
const
  LeftLabels: array [1..6] of Integer = (10, 10, 10, 10, 10, 10);
  TopLabels: array [1..6] of Integer = (10, 35, 60, 85, 110, 135);
var
  I: Integer;
begin
  if Width - FEditList[Low(FEditList)].Left - 4 < 0 then
  begin
    for I := Low(FEditList) to High(FEditList) do
      FEditList[I].Width := 40;
    FCombo1.Width := 40;
  end
  else
  begin
    for I := Low(FEditList) to High(FEditList) do
      FEditList[I].Width :=  Self.Width - FEditList[I].Left - 4;
    FCombo1.Width := Self.Width - FCombo1.Left - 4;
  end;
end;

//=== TJvId3v1Tag ============================================================

constructor TJvId3v1Tag.Create(AOwner: TComponent);
begin
  // (rom) inherited added
  inherited Create;
  FAlbum := '';
  FComment := '';
  FArtist := '';
  FSongName := '';
  FYear := '';
  FGenre := grBlues;
end;

procedure TJvId3v1Tag.SetAlbum(const Value: string);
begin
  FAlbum := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvId3v1Tag.SetArtist(const Value: string);
begin
  FArtist := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvId3v1Tag.SetComment(const Value: string);
begin
  FComment := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvId3v1Tag.SetGenre(const Value: TGenre);
begin
  FGenre := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvId3v1Tag.SetSongName(const Value: string);
begin
  FSongName := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvId3v1Tag.SetYear(const Value: string);
begin
  FYear := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.

