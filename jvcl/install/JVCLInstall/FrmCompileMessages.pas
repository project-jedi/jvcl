{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmCompileMessages.pas, released on 2004-12-13.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit FrmCompileMessages;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, ShellAPI, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus;

type
  TMsgType = (msgFatal, msgError, msgWarning, msgHint, msgText);

  TFormCompileMessages = class(TForm)
    ListBox: TListBox;
    PanelTop: TPanel;
    LabelHelp: TLabel;
    PopupMenu: TPopupMenu;
    Open1: TMenuItem;
    MenuNotepad: TMenuItem;
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxDblClick(Sender: TObject);
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
    FList: TObjectList;
    procedure ExtractText(Typ: TMsgType; const Text: string);
  public
    { Public-Deklarationen }
    procedure Clear;

    procedure AddHint(const Text: string);
    procedure AddWarning(const Text: string);
    procedure AddError(const Text: string);
    procedure AddFatal(const Text: string);

    procedure AddMsg(Typ: TMsgType; const Filename: string; Line: Integer; const Msg: string);
    procedure AddText(const Msg: string);
  end;

var
  FormCompileMessages: TFormCompileMessages;

implementation

{$R *.dfm}

uses
  InstallerConsts;

type
  TMsgItem = class(TObject)
  private
    FTyp: TMsgType;
    FFilename: string;
    FLine: Integer;
    FMsg: string;
  public
    constructor Create(const AFilename: string; ALine: Integer; const AMsg: string; ATyp: TMsgType);

    property Typ: TMsgType read FTyp;
    property Filename: string read FFilename;
    property Line: Integer read FLine;
    property Msg: string read FMsg;
  end;

{ TMsgItem }

constructor TMsgItem.Create(const AFilename: string; ALine: Integer;
  const AMsg: string; ATyp: TMsgType);
begin
  inherited Create;
  FFilename := AFilename;
  FLine := ALine;
  FMsg := AMsg;
  FTyp := ATyp;
end;

{ TFormCompileMessages }

procedure TFormCompileMessages.ExtractText(Typ: TMsgType; const Text: string);
var
  ps, psEnd, Line, Err: Integer;
  Filename, Msg: string;
begin
  ps := Pos(': ', Text);
  Msg := Copy(Text, ps + 2, MaxInt);

  Filename := Copy(Text, 1, ps - 1);
  psEnd := 0;
  while ps > 0 do
  begin
    if Filename[ps] = ')' then
      psEnd := ps;
    if (Filename[ps] = '(') and (psEnd > 0) then
    begin
      Val(Copy(Filename, ps + 1, psEnd - ps - 1), Line, Err);
      if Err <> 0 then
        Break;
      Delete(Filename, ps, MaxInt);

      AddMsg(Typ, Trim(Filename), Line, Msg);
      Exit;
    end;
    Dec(ps);
  end;
  AddText(Text);
end;

procedure TFormCompileMessages.AddFatal(const Text: string);
begin
  ExtractText(msgFatal, Text);
end;

procedure TFormCompileMessages.AddError(const Text: string);
begin
  ExtractText(msgError, Text);
end;

procedure TFormCompileMessages.AddWarning(const Text: string);
begin
  ExtractText(msgWarning, Text);
end;

procedure TFormCompileMessages.AddHint(const Text: string);
begin
  ExtractText(msgHint, Text);
end;

procedure TFormCompileMessages.AddMsg(Typ: TMsgType;
  const Filename: string; Line: Integer; const Msg: string);
var
  Item: TMsgItem;
begin
  Item := TMsgItem.Create(Filename, Line, Msg, Typ);
  FList.Add(Item);
  ListBox.Items.AddObject('', Item);
end;

procedure TFormCompileMessages.AddText(const Msg: string);
var
  Item: TMsgItem;
begin
  Item := TMsgItem.Create('', 0, Msg, msgText);
  FList.Add(Item);
  ListBox.Items.AddObject('', Item);
  Show;
end;

procedure TFormCompileMessages.Clear;
begin
  ListBox.Clear;
  FList.Clear;
  Hide;
  Application.ProcessMessages;
end;

procedure TFormCompileMessages.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Item: TMsgItem;
  S, TypName: string;
  W, MaxW: Integer;
begin
  Item := TMsgItem(ListBox.Items.Objects[Index]);

  with ListBox.Canvas do
  begin
    case Item.Typ of
      msgFatal:
        begin
          TypName := RsCompilerFatal;
          Font.Style := [fsBold];
          Font.Color := clRed;
        end;
      msgError:
        begin
          TypName := RsCompilerError;
          Font.Style := [];
          Font.Color := clRed;
        end;
      msgWarning:
        begin
          TypName := RsCompilerWarning;
          Font.Style := [];
          Font.Color := clMaroon;
        end;
      msgHint:
        begin
          TypName := RsCompilerHint;
          Font.Style := [];
          Font.Color := clGreen;
        end;
      msgText:
        begin
          Font.Style := [fsBold];
          Font.Color := clWindowText;
        end;
    end;

    if odSelected in State then
      Font.Color := clHighlightText;

    FillRect(Rect);
    Brush.Style := bsClear;

    if Item.Typ <> msgText then
    begin
      W := 0;
      S := TypName + ': ';
      TextRect(Rect, Rect.Left + 2 + W, Rect.Top + 1, S);
      Inc(W, TextWidth(S));
      if not (odSelected in State) then
        Font.Color := clWindowText;

      Font.Style := [];
      TextRect(Rect, Rect.Left + 2 + W, Rect.Top + 1, Item.Filename);
      Inc(W, TextWidth(Item.Filename));
      Font.Style := [fsBold];
      S := '(' + IntToStr(Item.Line) + '): ';
      TextRect(Rect, Rect.Left + 2 + W, Rect.Top + 1, S);
      Inc(W, TextWidth(S));
      Font.Style := [];
      TextRect(Rect, Rect.Left + 2 + W, Rect.Top + 1, Item.Msg);
      Inc(W, TextWidth(Item.Msg));
    end
    else
    begin
      TextRect(Rect, Rect.Left + 2, Rect.Top + 2, Item.Msg);
      W := TextWidth(Item.Msg);
    end;

    {$IFDEF COMPILER6_UP}
    MaxW := 2 + ListBox.ClientWidth + 2;
    if (W > MaxW) and (W - MaxW > ListBox.ScrollWidth) then
      ListBox.ScrollWidth := W - MaxW;
    {$ELSE}
    // this code's purpose is to prevent the Delphi 5 compiler hint
    MaxW := W;
    if MaxW <> -1 then
      ;
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TFormCompileMessages.FormCreate(Sender: TObject);
begin
  FList := TObjectList.Create;
end;

procedure TFormCompileMessages.FormDestroy(Sender: TObject);
begin
  FList.Free;
end;

procedure TFormCompileMessages.ListBoxDblClick(Sender: TObject);
var
  Item: TMsgItem;
begin
  if ListBox.ItemIndex <> -1 then
  begin
    Item := TMsgItem(ListBox.Items.Objects[ListBox.ItemIndex]);
    if (Item.Filename <> '') and FileExists(Item.Filename) then
    begin
      if Sender = MenuNotepad then
      begin
        if ShellExecute(Handle, 'open', 'notepad.exe', PChar(Item.Filename), nil, SW_SHOW) < 32 then // do not localize
          MessageDlg(RsErrorOpeningFile, mtError, [mbOk], 0);
      end
      else
        if ShellExecute(Handle, 'open', PChar(Item.Filename), nil, nil, SW_SHOW) < 32 then // do not localize
          MessageDlg(RsErrorOpeningFile, mtError, [mbOk], 0);
    end;
  end;
end;

procedure TFormCompileMessages.ListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    ListBox.ItemIndex := ListBox.ItemAtPos(Point(X, Y), True);
end;

end.
