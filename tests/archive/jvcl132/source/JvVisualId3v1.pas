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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvVisualId3v1;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  Dialogs, StdCtrls, JvTypes, JvId3v1, JVCLVer;

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
    FFileName: TFileName;
    FLabel1: TLabel;
    FLabel2: TLabel;
    FLabel3: TLabel;
    FLabel4: TLabel;
    FLabel5: TLabel;
    FLabel6: TLabel;
    FEdit1: TEdit;
    FEdit2: TEdit;
    FEdit3: TEdit;
    FEdit4: TEdit;
    FEdit5: TEdit;
    FCombo1: TComboBox;
    FId3v1: TJvId3v1;
    FReadOnly: Boolean;
    FOnChange: TNotifyEvent;
    FId3Tag: TJvId3v1Tag;
    FEditFont: TFont;
    FLabelFont: TFont;
    FAboutJVCL: TJVCLAboutInfo;
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
  protected
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure RemoveTagFromFile(FileName: string);
  end;

implementation

///////////////////////////////////////////////////////////
// TJvVisualId3v1
///////////////////////////////////////////////////////////

procedure TJvVisualId3v1.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  FEdit1.Text := FId3Tag.Artist;
  FEdit2.Text := FId3Tag.SongName;
  FEdit3.Text := FId3Tag.Album;
  FEdit4.Text := FId3Tag.Year;
  FEdit5.Text := FId3Tag.Comment;
  FCombo1.ItemIndex := Byte(FId3Tag.Genre);
end;

{******************************************************************************}

constructor TJvVisualId3v1.Create(AOwner: TComponent);
const
 CaptionLabels: array[1..6] of string = ('Artist', 'SongName', 'Album', 'Year', 'Comment', 'Genre');
begin
  inherited;
  Width := 229;
  Height := 160;
  FReadOnly := True;

  FEditFont := TFont.Create;
  FLabelFont := TFont.Create;

  FLabel1 := TLabel.Create(Self);
  FLabel2 := TLabel.Create(Self);
  FLabel3 := TLabel.Create(Self);
  FLabel4 := TLabel.Create(Self);
  FLabel5 := TLabel.Create(Self);
  FLabel6 := TLabel.Create(Self);

  FLabel1.Caption := CaptionLabels[1];
  FLabel2.Caption := CaptionLabels[2];
  FLabel3.Caption := CaptionLabels[3];
  FLabel4.Caption := CaptionLabels[4];
  FLabel5.Caption := CaptionLabels[5];
  FLabel6.Caption := CaptionLabels[6];

  FLabel1.Parent := Self;
  FLabel2.Parent := Self;
  FLabel3.Parent := Self;
  FLabel4.Parent := Self;
  FLabel5.Parent := Self;
  FLabel6.Parent := Self;

  FEdit1 := TEdit.Create(Self);
  FEdit2 := TEdit.Create(Self);
  FEdit3 := TEdit.Create(Self);
  FEdit4 := TEdit.Create(Self);
  FEdit5 := TEdit.Create(Self);

  FEdit1.Text := '';
  FEdit2.Text := '';
  FEdit3.Text := '';
  FEdit4.Text := '';
  FEdit5.Text := '';

  FEdit1.Enabled := False;
  FEdit2.Enabled := False;
  FEdit3.Enabled := False;
  FEdit4.Enabled := False;
  FEdit5.Enabled := False;

  FEdit1.Parent := Self;
  FEdit2.Parent := Self;
  FEdit3.Parent := Self;
  FEdit4.Parent := Self;
  FEdit5.Parent := Self;

  FEdit1.MaxLength := 30;
  FEdit2.MaxLength := 30;
  FEdit3.MaxLength := 30;
  FEdit4.MaxLength := 4;
  FEdit5.MaxLength := 30;

  FCombo1 := TCombobox.Create(Self);
  FCombo1.Text := '';
  FCombo1.Style := csDropDownList;
  FCombo1.Parent := Self;
  FCombo1.Enabled := False;

  FId3Tag := TJvId3v1Tag.Create(Self);
  FId3Tag.OnChange := Changed;

  FId3v1 := TJvId3v1.Create(Self);

  FEditFont.OnChange := FontChanged;
  FLabelFont.OnChange := FontChanged;
  FontChanged(Self);

  FEdit1.OnChange := UserChanged;
  FEdit2.OnChange := UserChanged;
  FEdit3.OnChange := UserChanged;
  FEdit4.OnChange := UserChanged;
  FEdit5.OnChange := UserChanged;
  FCombo1.OnChange := UserChanged;
end;

procedure TJvVisualId3v1.CreateWnd;
var
  i: Byte;
begin
  inherited CreateWnd;
  for i := 0 to ord(High(TGenre)) do
    FCombo1.Items.Add(FId3v1.GenreToString(TGenre(i)));
end;

{******************************************************************************}

destructor TJvVisualId3v1.Destroy;
begin
  FLabel1.Free;
  FLabel2.Free;
  FLabel3.Free;
  FLabel4.Free;
  FLabel5.Free;
  FLabel6.Free;
  FEdit1.Free;
  FEdit2.Free;
  FEdit3.Free;
  FEdit4.Free;
  FEdit5.Free;
  FCombo1.Free;
  FId3v1.Free;
  FId3Tag.Free;
  FEditFont.Free;
  FLabelFont.Free;
  inherited;
end;

{******************************************************************************}

procedure TJvVisualId3v1.FontChanged(Sender: TObject);
var
  LabelHeight, LabelWidth: Integer;
begin
  FLabel1.Font := FLabelFont;
  FLabel2.Font := FLabel1.Font;
  FLabel3.Font := FLabel1.Font;
  FLabel4.Font := FLabel1.Font;
  FLabel5.Font := FLabel1.Font;
  FLabel6.Font := FLabel1.Font;

  FEdit1.Font := FEditFont;
  FEdit2.Font := FEdit1.Font;
  FEdit3.Font := FEdit1.Font;
  FEdit4.Font := FEdit1.Font;
  FEdit5.Font := FEdit1.Font;
  FCombo1.Font := FEdit1.Font;

  LabelHeight := FLabel1.Height;
  if FLabel2.Height > LabelHeight then
    LabelHeight := FLabel2.Height;
  if FLabel3.Height > LabelHeight then
    LabelHeight := FLabel3.Height;
  if FLabel4.Height > LabelHeight then
    LabelHeight := FLabel4.Height;
  if FLabel5.Height > LabelHeight then
    LabelHeight := FLabel5.Height;
  if FLabel6.Height > LabelHeight then
    LabelHeight := FLabel6.Height;
  LabelWidth := FLabel1.Width;
  if FLabel2.Width > LabelWidth then
    LabelWidth := FLabel2.Width;
  if FLabel3.Width > LabelWidth then
    LabelWidth := FLabel3.Width;
  if FLabel4.Width > LabelWidth then
    LabelWidth := FLabel4.Width;
  if FLabel5.Width > LabelWidth then
    LabelWidth := FLabel5.Width;
  if FLabel6.Width > LabelWidth then
    LabelWidth := FLabel6.Width;

  FLabel1.Left := 5;
  FLabel2.Left := 5;
  FLabel3.Left := 5;
  FLabel4.Left := 5;
  FLabel5.Left := 5;
  FLabel6.Left := 5;

  FEdit1.Left := LabelWidth + FLabel1.Left + 10;
  FEdit2.Left := LabelWidth + FLabel1.Left + 10;
  FEdit3.Left := LabelWidth + FLabel1.Left + 10;
  FEdit4.Left := LabelWidth + FLabel1.Left + 10;
  FEdit5.Left := LabelWidth + FLabel1.Left + 10;
  FCombo1.Left := LabelWidth + FLabel1.Left + 10;

  if LabelHeight > FEdit1.Height then
  begin
    FLabel1.Top := 5;
    FLabel2.Top := FLabel1.Top + LabelHeight + 5;
    FLabel3.Top := FLabel2.Top + LabelHeight + 5;
    FLabel4.Top := FLabel3.Top + LabelHeight + 5;
    FLabel5.Top := FLabel4.Top + LabelHeight + 5;
    FLabel6.Top := FLabel5.Top + LabelHeight + 5;

    FEdit1.Top := FLabel1.Top + (LabelHeight - FEdit1.Height) div 2;
    FEdit2.Top := FLabel2.Top + (LabelHeight - FEdit1.Height) div 2;
    FEdit3.Top := FLabel3.Top + (LabelHeight - FEdit1.Height) div 2;
    FEdit4.Top := FLabel4.Top + (LabelHeight - FEdit1.Height) div 2;
    FEdit5.Top := FLabel5.Top + (LabelHeight - FEdit1.Height) div 2;
    FCombo1.Top := FLabel6.Top + (LabelHeight - FEdit1.Height) div 2;
  end
  else
  begin
    FEdit1.Top := 5;
    FEdit2.Top := FEdit1.Top + FEdit1.Height + 5;
    FEdit3.Top := FEdit2.Top + FEdit2.Height + 5;
    FEdit4.Top := FEdit3.Top + FEdit3.Height + 5;
    FEdit5.Top := FEdit4.Top + FEdit4.Height + 5;
    FCombo1.Top := FEdit5.Top + FEdit5.Height + 5;

    FLabel1.Top := FEdit1.Top + (FEdit1.Height - LabelHeight) div 2;
    FLabel2.Top := FEdit2.Top + (FEdit1.Height - LabelHeight) div 2;
    FLabel3.Top := FEdit3.Top + (FEdit1.Height - LabelHeight) div 2;
    FLabel4.Top := FEdit4.Top + (FEdit1.Height - LabelHeight) div 2;
    FLabel5.Top := FEdit5.Top + (FEdit1.Height - LabelHeight) div 2;
    FLabel6.Top := FCombo1.Top + (FEdit1.Height - LabelHeight) div 2;
  end;

  Self.Width := Self.Width + 1;
  Self.Width := Self.Width - 1;
end;

{******************************************************************************}

function TJvVisualId3v1.GetEditColor: TColor;
begin
  Result := FEdit1.Color;
end;

{******************************************************************************}

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

{******************************************************************************}

procedure TJvVisualId3v1.RemoveTagFromFile(FileName: string);
begin
  FId3v1.RemoveTag(FileName);
end;

{******************************************************************************}

procedure TJvVisualId3v1.SaveToFile(FileName: string);
begin
  with FId3Tag do
    FId3v1.WriteTag(FileName, SongName, Artist, Album, Year, Comment, Genre);
end;

{******************************************************************************}

procedure TJvVisualId3v1.SetEditColor(const Value: TColor);
begin
  FEdit1.Color := Value;
  FEdit2.Color := Value;
  FEdit3.Color := Value;
  FEdit4.Color := Value;
  FEdit5.Color := Value;
  FCombo1.Color := Value;
end;

{******************************************************************************}

procedure TJvVisualId3v1.SetEditFont(const Value: TFont);
begin
  FEditFont := Value;
  FEdit1.Font := Value;
  FEdit2.Font := Value;
  FEdit3.Font := Value;
  FEdit4.Font := Value;
  FEdit5.Font := Value;
  FCombo1.Font := Value;
end;

{******************************************************************************}

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

{******************************************************************************}

procedure TJvVisualId3v1.SetLabelFont(const Value: TFont);
begin
  FLabelFont := Value;
  FLabel1.Font := Value;
  FLabel2.Font := Value;
  FLabel3.Font := Value;
  FLabel4.Font := Value;
  FLabel5.Font := Value;
  FLabel6.Font := Value;
end;

{******************************************************************************}

procedure TJvVisualId3v1.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  FEdit1.Enabled := not FReadOnly;
  FEdit2.Enabled := not FReadOnly;
  FEdit3.Enabled := not FReadOnly;
  FEdit4.Enabled := not FReadOnly;
  FEdit5.Enabled := not FReadOnly;
  FCombo1.Enabled := not FReadOnly;
end;

{******************************************************************************}

procedure TJvVisualId3v1.UserChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);

  FId3Tag.FArtist := FEdit1.Text;
  FId3Tag.FSongName := FEdit2.Text;
  FId3Tag.FAlbum := FEdit3.Text;
  FId3Tag.FYear := FEdit4.Text;
  FId3Tag.FComment := FEdit5.Text;
  FId3Tag.FGenre := TGenre(FCombo1.ItemIndex);
end;

{******************************************************************************}

procedure TJvVisualId3v1.WMSize(var Msg: TWMSize);
const
  LeftLabels: array[1..6] of Integer = (10, 10, 10, 10, 10, 10);
  TopLabels: array[1..6] of Integer = (10, 35, 60, 85, 110, 135);
begin
  if Width - FEdit1.Left - 4 < 0 then
  begin
    FEdit1.Width := 40;
    FEdit2.Width := 40;
    FEdit3.Width := 40;
    FEdit4.Width := 40;
    FEdit5.Width := 40;
    FCombo1.Width := 40;
  end
  else
  begin
    FEdit1.Width := Self.Width - FEdit1.Left - 4;
    FEdit2.Width := Self.Width - FEdit2.Left - 4;
    FEdit3.Width := Self.Width - FEdit3.Left - 4;
    FEdit4.Width := Self.Width - FEdit4.Left - 4;
    FEdit5.Width := Self.Width - FEdit5.Left - 4;
    FCombo1.Width := Self.Width - FCombo1.Left - 4;
  end;
end;

///////////////////////////////////////////////////////////
// TJvId3v1Tag
///////////////////////////////////////////////////////////

constructor TJvId3v1Tag.Create(AOwner: TComponent);
begin
  FAlbum := '';
  FComment := '';
  FArtist := '';
  FSongName := '';
  FYear := '';
  FGenre := grNone;
end;

{******************************************************************************}

procedure TJvId3v1Tag.SetAlbum(const Value: string);
begin
  FAlbum := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{******************************************************************************}

procedure TJvId3v1Tag.SetArtist(const Value: string);
begin
  FArtist := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{******************************************************************************}

procedure TJvId3v1Tag.SetComment(const Value: string);
begin
  FComment := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{******************************************************************************}

procedure TJvId3v1Tag.SetGenre(const Value: TGenre);
begin
  FGenre := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{******************************************************************************}

procedure TJvId3v1Tag.SetSongName(const Value: string);
begin
  FSongName := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{******************************************************************************}

procedure TJvId3v1Tag.SetYear(const Value: string);
begin
  FYear := Value;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
