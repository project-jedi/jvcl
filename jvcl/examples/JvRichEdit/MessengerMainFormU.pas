{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}
{$I jvcl.inc}
unit MessengerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, ComCtrls, ToolWin, StdCtrls, JvRichEdit,
  ExtCtrls, JvExStdCtrls;

{ Emoticons are copyright Mozilla (www.mozilla.org) }

type
  TMessengerMainForm = class(TForm)
    Panel1: TPanel;
    JvRichEdit1: TJvRichEdit;
    Panel2: TPanel;
    edtNewText: TEdit;
    Button1: TButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    actCool: TAction;
    actCry: TAction;
    actEmbarressed: TAction;
    actFoot: TAction;
    actFrown: TAction;
    actInnocent: TAction;
    actKiss: TAction;
    actLaughing: TAction;
    actMoney: TAction;
    actSealed: TAction;
    actSurprised: TAction;
    actTongue: TAction;
    actUndecided: TAction;
    actWink: TAction;
    actYell: TAction;
    actSmile: TAction;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    actBold: TAction;
    actItalic: TAction;
    actUnderline: TAction;
    actSend: TAction;
    Timer1: TTimer;
    procedure actBoldExecute(Sender: TObject);
    procedure actItalicExecute(Sender: TObject);
    procedure actUnderlineExecute(Sender: TObject);
    procedure OnEmoticonClick(Sender: TObject);
    procedure actSendExecute(Sender: TObject);
    procedure actBoldUpdate(Sender: TObject);
    procedure actItalicUpdate(Sender: TObject);
    procedure actUnderlineUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentFont: TFont;
    FHeaderFont: TFont;
    FYourLines: Integer;
    FReadOnlySend: Boolean;
    FReadOnlyHandled: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HandleReadOnly;
    procedure HandleReadOnlyOff;
    procedure HandleReadOnlyOn;

    procedure AddImageToEdit(const AImageIndex: Integer);
    procedure AddImageToRichEdit(const AImageIndex: Integer);
    procedure AddTextToRichEdit(P: PChar; const ALength: Integer; AFont: TFont);
    procedure ParseString(const S: string);
    procedure SendString(const S: string; const You: Boolean);
  end;

var
  MessengerMainForm: TMessengerMainForm;

implementation

uses
  RichEdit
  {$IFDEF COMPILER6_UP}
  , DateUtils
  {$ENDIF};

{$R *.dfm}

const
  CImageToString: array[0..15] of string = (
 {0}':-@', { Yell }
    '8-)', { Cool }
    ':,(', { Cry }
    '(blush)', { Embarressed }
    ':-&', { Foot }
 {5}':-(', { Frown }
    'O :-)', { Innocent }
    ':*', { Kiss }
    'LOL', { Laughing }
    ':-$', { Money }
{10}':-x', { Sealed }
    ':-)', { Smile }
    ':-o', { Surprised }
    ':-p', { Tongue }
    ':-\', { Undecided }
    ';-)' { Wink }
    );

var
  ReadOnlyMessages: array[0..2] of string = (
    'You can''t resize and/or drag-drop the images because the rich ' +
    'edit control is read-only 8-) Type "readonly" to set the ReadOnly property ' +
    'of the rich edit to false',
    'Try to drag-drop the images, and then to resize the images',
    '(blush) I meant "read-only on" or "read-only off"'
    );

procedure TMessengerMainForm.actBoldExecute(Sender: TObject);
begin
  if fsBold in FCurrentFont.Style then
    FCurrentFont.Style := FCurrentFont.Style - [fsBold]
  else
    FCurrentFont.Style := FCurrentFont.Style + [fsBold];
end;

procedure TMessengerMainForm.actBoldUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := fsBold in FCurrentFont.Style;
end;

procedure TMessengerMainForm.actItalicExecute(Sender: TObject);
begin
  if fsItalic in FCurrentFont.Style then
    FCurrentFont.Style := FCurrentFont.Style - [fsItalic]
  else
    FCurrentFont.Style := FCurrentFont.Style + [fsItalic];
end;

procedure TMessengerMainForm.actItalicUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := fsItalic in FCurrentFont.Style;
end;

procedure TMessengerMainForm.actSendExecute(Sender: TObject);
begin
  SendString(edtNewText.Text, True);
end;

procedure TMessengerMainForm.actUnderlineExecute(Sender: TObject);
begin
  if fsUnderline in FCurrentFont.Style then
    FCurrentFont.Style := FCurrentFont.Style - [fsUnderline]
  else
    FCurrentFont.Style := FCurrentFont.Style + [fsUnderline];
end;

procedure TMessengerMainForm.actUnderlineUpdate(Sender: TObject);
begin
  if Sender is TAction then
    TAction(Sender).Checked := fsUnderline in FCurrentFont.Style;
end;

procedure TMessengerMainForm.AddImageToEdit(const AImageIndex: Integer);
var
  S: string;
  LSelLength, LSelStart: Integer;
begin
  if (AImageIndex < 0) and (AImageIndex >= 16) then
    Exit;

  S := edtNewText.Text;
  LSelLength := edtNewText.SelLength;
  LSelStart := edtNewText.SelStart;

  if LSelLength > 0 then
  begin
    Delete(S, LSelStart + 1, LSelLength);
    Insert(CImageToString[AImageIndex], S, LSelStart + 1);
    edtNewText.Text := S;
    edtNewText.SelStart := LSelStart;
    edtNewText.SelLength := Length(CImageToString[AImageIndex]);
  end
  else
  begin
    Insert(CImageToString[AImageIndex], S, LSelStart + 1);
    edtNewText.Text := S;
    edtNewText.SelStart := LSelStart + Length(CImageToString[AImageIndex]);
    edtNewText.SelLength := 0;
  end;
end;

procedure TMessengerMainForm.AddImageToRichEdit(const AImageIndex: Integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    ImageList1.GetBitmap(AImageIndex, Bitmap);
    JvRichEdit1.InsertGraphic(Bitmap, False);

    { Move cursor }
    with JvRichEdit1.GetSelection do
      JvRichEdit1.SetSelection(cpMin + 1, cpMin + 1, False);
  finally
    Bitmap.Free;
  end;
end;

procedure TMessengerMainForm.AddTextToRichEdit(P: PChar; const ALength: Integer; AFont: TFont);
var
  S: string;
begin
  if ALength < 0 then
    Exit;
  SetString(S, P, ALength);
  JvRichEdit1.InsertFormatText(-1, S, AFont);
  { Move cursor }
  with JvRichEdit1.GetSelection do
    JvRichEdit1.SetSelection(cpMin + ALength, cpMin + ALength, False);
end;

constructor TMessengerMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentFont := TFont.Create;
  FHeaderFont := TFont.Create;
  FHeaderFont.Color := clRed;
  FHeaderFont.Style := [fsBold];
  FHeaderFont.Name := 'Verdana';
end;

destructor TMessengerMainForm.Destroy;
begin
  FCurrentFont.Free;
  FHeaderFont.Free;
  inherited Destroy;
end;

procedure TMessengerMainForm.FormShow(Sender: TObject);
begin
  SendString('Hello, how are you doing :)', False);
  FocusControl(edtNewText);
  Randomize;
end;

procedure TMessengerMainForm.HandleReadOnly;
begin
  FReadOnlySend := True;
  ReadOnlyMessages[0] := 'read-only off :-p';
end;

procedure TMessengerMainForm.HandleReadOnlyOff;
begin
  FReadOnlyHandled := True;
  ReadOnlyMessages[0] := 'read-only off :-p';
  if JvRichEdit1.ReadOnly then
  begin
    { cheap effect :-p }
    JvRichEdit1.Color := clYellow;
    Application.ProcessMessages;
    Sleep(50);
    JvRichEdit1.ReadOnly := False;
    JvRichEdit1.Color := clWindow;
  end;
end;

procedure TMessengerMainForm.HandleReadOnlyOn;
begin
  FReadOnlyHandled := True;
  if not JvRichEdit1.ReadOnly then
  begin
    { cheap effect :-p }
    JvRichEdit1.Color := clRed;
    Application.ProcessMessages;
    Sleep(50);
    JvRichEdit1.ReadOnly := True;
    JvRichEdit1.Color := clWindow;
  end;
end;

procedure TMessengerMainForm.OnEmoticonClick(Sender: TObject);
begin
  if Sender is TAction then
    AddImageToEdit(TAction(Sender).ImageIndex);
end;

procedure TMessengerMainForm.ParseString(const S: string);
var
  P, Q: PChar;
  State: Integer;

  procedure AddImage(const SmileIndex, SmileLength: Integer);
  begin
    AddTextToRichEdit(Q, P - Q - SmileLength + 1, FCurrentFont);
    AddImageToRichEdit(SmileIndex);

    State := 0;
    Q := P + 1;
  end;
begin
  P := PChar(S);
  Q := P;

  State := 0;

  // State =  1..  then looking at ":-A" with A = @x)( etc.
  //         10..                  "LOL"
  //         20..                  "O :-)"
  //         30..                  "(blush)"
  //         40..                  ":'-("
  //         50..                  "8-)"
  //         60..                  ";-)"
  //         70..                  "readonly"
  //         80..                  "read-only on" or "read-only off"
  //
  // State =  1 -> ":" read
  // State =  2 -> ":-" read
  // State = 11 -> "LO" read
  // State = 22 -> "O :" read
  // State = 23 -> "O :-" read
  //
  // etc.

  while P^ <> #0 do
  begin
    case P^ of
      '$':
        if State in [2, 23] then
          AddImage(9, 3) // :-$
        else
          State := 0;
      '&':
        if State in [2, 23] then
          AddImage(4, 3) // :-&
        else
          State := 0;
      '(':
        case State of
          1, 22: AddImage(5, 2); // :(
          2, 23: AddImage(5, 3); // :-(
          40: AddImage(2, 3); // :,(
          41: AddImage(2, 4); // :'-(
        else
          State := 30;
        end;
      ')':
        case State of
          1, 22: AddImage(11, 2); // :)
          2: AddImage(11, 3); // :-)
          23: AddImage(6, 5); // O :-)
          35: AddImage(3, 7); // (blush)
          51: AddImage(1, 3); // 8-)
          61: AddImage(15, 3); // ;-)
        else
          State := 0;
        end;
      '*':
        if State = 1 then
          AddImage(7, 2) // :*
        else
          State := 0;
      ',':
        if State in [1, 22] then
          State := 40
        else
          State := 0;
      '-':
        case State of
          1, 22, 40, 50, 60: Inc(State);
          73: State := 80;
        else
          State := 0;
        end;
      '8': State := 50;
      '"': State := -1; // to prevent "read.." etc will be triggered by the program
      ':':
        if State = 21 then
          State := 22
        else
          State := 1;
      ';': State := 60;
      '@':
        case State of
          1, 22: AddImage(0, 2); // :@
          2, 23: AddImage(0, 3) // :-@
        else
          State := 0;
        end;
      'D':
        case State of
          1, 22: AddImage(8, 2); // :D = LOL
          2, 23: AddImage(8, 3) // :-D = LOL
        else
          State := 0;
        end;
      'L':
        if State = 11 then
          AddImage(8, 3) // LOL
        else
          State := 10;
      'O':
        if State = 10 then
          State := 11
        else
          State := 20;
      '\', '/':
        if State in [2, 23] then
          AddImage(14, 3) // :-\
        else
          State := 0;
      'a':
        if State = 71 then
          State := 72
        else
          State := 0;
      'b':
        if State = 30 then
          State := 31
        else
          State := 0;
      'd':
        if State = 72 then
          State := 73
        else
          State := 0;
      'e':
        if State = 70 then
          State := 71
        else
          State := 0;
      'f':
        case State of
          86: State := 87;
          87: HandleReadOnlyOff;
        else
          State := 0;
        end;
      'h':
        if State = 34 then
          State := 35
        else
          State := 0;
      'l':
        if State in [31, 75, 82] then
          Inc(State)
        else
          State := 0;
      'n':
        case State of
          74, 81: Inc(State);
          86: HandleReadOnlyOn;
        else
          State := 0;
        end;
      'o':
        case State of
          2, 23: AddImage(12, 3); // :-o
          73, 80, 85: Inc(State);
        else
          State := 0;
        end;
      'p':
        if State in [2, 23] then
          AddImage(13, 3) // :-p
        else
          State := 0;
      'r':
        if State <> -1 then
          State := 70
        else
          State := 0;
      's':
        if State = 33 then
          State := 34
        else
          State := 0;
      'u':
        if State = 32 then
          State := 33
        else
          State := 0;
      'x', 'X':
        if State in [2, 23] then
          AddImage(10, 3) // :-x
        else
          State := 0;
      'y':
        case State of
          76: HandleReadOnly;
          83: State := 84;
        else
          State := 0;
        end;
      ' ':
        case State of
          20, 84: Inc(State);
          11: State := 21;
        else
          State := 0;
        end;
    end;
    Inc(P);
  end;

  if Q < P then
    AddTextToRichEdit(Q, P - Q, FCurrentFont);
end;

procedure TMessengerMainForm.SendString(const S: string; const You: Boolean);
var
  OldActive: TWinControl;
begin
  OldActive := ActiveControl;
  try
    { Ensure rich edit control is focused before moving }
    FocusControl(JvRichEdit1);

    { Goto end }
    JvRichEdit1.SetSelection(MaxInt, MaxInt, False);
    if You then
    begin
      FHeaderFont.Color := clRed;
      AddTextToRichEdit('You: ', 5, FHeaderFont);
      Inc(FYourLines);
    end
    else
    begin
      FHeaderFont.Color := clGreen;
      AddTextToRichEdit('JVCL: ', 5, FHeaderFont);
      FYourLines := 0;
    end;
    ParseString(S + #13#10);
    { Goto end & scroll }
    JvRichEdit1.SetSelection(MaxInt, MaxInt, True);
  finally
    FocusControl(OldActive);
  end;
end;

procedure TMessengerMainForm.Timer1Timer(Sender: TObject);
const
  CLameResponse: array[0..4] of string = (
    'What?',
    'Huh??',
    ':)',
    'Really :-o',
    'Good 8-)');
begin
  if Random(10 * (1 + FYourLines)) < 9 then
    Exit;

  if FReadOnlySend and not FReadOnlyHandled then
  begin
    SendString(ReadOnlyMessages[2], False);
    FReadOnlyHandled := True;
  end
  else
  if FYourLines = 0 then
    SendString('Hello :-@', False)
  else
  if Random(10) < 4 then
  begin
    if JvRichEdit1.ReadOnly then
      SendString(ReadOnlyMessages[0], False)
    else
    begin
      SendString(ReadOnlyMessages[1], False);
      ReadOnlyMessages[1] := 'read-only on :-p';
    end;
  end
  else
    SendString(CLameResponse[Random(5)], False);
end;

end.
