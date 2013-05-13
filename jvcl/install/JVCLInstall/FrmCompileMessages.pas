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
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit FrmCompileMessages;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Windows, ShellAPI, Types, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus,
  JclSysUtils, JVCLData,
  FrmCompile;

type
  TMsgType = (msgFatal, msgError, msgWarning, msgHint, msgText);

  TFormCompileMessages = class(TForm, ICompileMessages)
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
    FList: TObjectList;
    FPaths: TStrings;
    FLogLines: TJclSimpleLog;
    FCurrentTarget: TTargetConfig;
    
    procedure ExtractText(Typ: TMsgType; const Text: string);
    function GetCount: Integer;
    procedure SetPaths(const Value: TStrings);

    procedure WriteLog(const Msg: string);
  public
    destructor Destroy; override;
    procedure Clear;

    procedure AddHint(const Text: string);
    procedure AddWarning(const Text: string);
    procedure AddError(const Text: string);
    procedure AddFatal(const Text: string);
    procedure SetCurrentTarget(ATarget: TTargetConfig);

    procedure AddMsg(Typ: TMsgType; const Filename: string; Line: Integer; const Msg: string);
    procedure AddText(const Msg: string);

    property Count: Integer read GetCount;
    property Paths: TStrings read FPaths write SetPaths;
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

function TypToStr(Typ: TMsgType): string;
begin
  case Typ of
    msgFatal:
      Result := RsCompilerFatal;
    msgError:
      Result := RsCompilerError;
    msgWarning:
      Result := RsCompilerWarning;
    msgHint:
      Result := RsCompilerHint;
    msgText:
      Result := '';
    else
      raise Exception.CreateFmt('Unsupported type %d', [Integer(Typ)]);
  end;
end;

procedure TFormCompileMessages.AddMsg(Typ: TMsgType;
  const Filename: string; Line: Integer; const Msg: string);
var
  Item: TMsgItem;
  Prefix: string;
begin
  Item := TMsgItem.Create(Filename, Line, Msg, Typ);
  FList.Add(Item);
  ListBox.Items.AddObject('', Item);

  Prefix := '';
  if FileName <> '' then
    Prefix := Prefix + FileName;
  if Line > 0 then
    Prefix := Prefix + ' (' + IntToStr(Line) + ')';
  if Typ <> msgText then
    Prefix := Prefix + ' - ' + TypToStr(Typ) + ': ';

  WriteLog(Prefix + Msg);
end;

procedure TFormCompileMessages.AddText(const Msg: string);
begin
  AddMsg(msgText, '', 0, Msg);
  Show;
end;

procedure TFormCompileMessages.Clear;
begin
  ListBox.Clear;
  FList.Clear;
  Hide;
  Application.ProcessMessages;
end;

destructor TFormCompileMessages.Destroy;
begin
  FLogLines.Free;

  inherited Destroy;
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

    MaxW := 2 + ListBox.ClientWidth + 2;
    if (W > MaxW) and (W - MaxW > ListBox.ScrollWidth) then
      ListBox.ScrollWidth := W - MaxW;
  end;
end;

procedure TFormCompileMessages.FormCreate(Sender: TObject);
begin
  FList := TObjectList.Create;
  FPaths := TStringList.Create;
end;

procedure TFormCompileMessages.FormDestroy(Sender: TObject);
begin
  FPaths.Free;
  FList.Free;
end;

procedure TFormCompileMessages.ListBoxDblClick(Sender: TObject);
var
  Item: TMsgItem;
  Filename: string;
  I: Integer;
begin
  if ListBox.ItemIndex <> -1 then
  begin
    Item := TMsgItem(ListBox.Items.Objects[ListBox.ItemIndex]);
    if Item.Filename <> '' then
    begin
      // find file
      Filename := Item.Filename;
      if Pos(':', Filename) = 0 then
      begin
        for I := 0 to Paths.Count - 1 do
        begin
          if FileExists(Paths[I] + '\' + Filename) then
          begin
            Filename := Paths[I] + '\' + Filename;
            Break;
          end;
        end;
      end;

      if FileExists(Filename) then
      begin
        if Sender = MenuNotepad then
        begin
          if ShellExecute(Handle, 'open', 'notepad.exe', PChar(Filename), nil, SW_SHOW) < 32 then // do not localize
            MessageDlg(RsErrorOpeningFile, mtError, [mbOk], 0);
        end
        else
        begin
          if ShellExecute(Handle, 'open', PChar(Filename), nil, nil, SW_SHOW) < 32 then // do not localize
            MessageDlg(RsErrorOpeningFile, mtError, [mbOk], 0);
        end;
      end;
      
    end;
  end;
end;

procedure TFormCompileMessages.ListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    ListBox.ItemIndex := ListBox.ItemAtPos(Point(X, Y), True);
end;

function TFormCompileMessages.GetCount: Integer;
begin
  Result := ListBox.Items.Count;
end;

procedure TFormCompileMessages.SetCurrentTarget(ATarget: TTargetConfig);
begin
  if FCurrentTarget <> ATarget then
  begin
    FCurrentTarget := ATarget;
    FreeAndNil(FLogLines);
  end;
end;

procedure TFormCompileMessages.SetPaths(const Value: TStrings);
begin
  if Value <> FPaths then
    FPaths.Assign(Value);
end;

procedure TFormCompileMessages.WriteLog(const Msg: string);
begin
  try
    if Assigned(FCurrentTarget) then
    begin
      if not Assigned(FLogLines) then
      begin
        FLogLines := TJclSimpleLog.Create(FCurrentTarget.LogFileName);
        FLogLines.ClearLog;
      end;
      FLogLines.Write(Msg);
    end;
  except
    ; // writing log should not prevent the rest from working
  end;
end;

end.