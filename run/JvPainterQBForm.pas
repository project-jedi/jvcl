{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a Copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPainterQBU.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPainterQBForm;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QTypes, QGraphics, QControls, Types,
  QForms, QDialogs, QStdCtrls, QExtCtrls, QMenus, QComCtrls, QWindows,
  {$ENDIF VisualCLX}
  JvDrawImage, JvComponent;

type
  TPainterQBForm = class(TJvForm)
    Bevel1: TBevel;
    qbpresets: TComboBox;
    presetspop: TPopupMenu;
    AddBackdrop1: TMenuItem;
    DeleteBackdrop1: TMenuItem;
    Panel1: TPanel;
    QBList: TListBox;
    UpdateBackdrop1: TMenuItem;
    redradio: TRadioButton;
    greenradio: TRadioButton;
    blueradio: TRadioButton;
    trkred: TScrollBar;
    trkgreen: TScrollBar;
    trkblue: TScrollBar;
    trkfactor: TScrollBar;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    procedure QBListClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    {$IFDEF VCL}
    procedure qbpresetsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    {$ENDIF VCL}
    procedure qbpresetsClick(Sender: TObject);
    procedure SetLabels;
    procedure AddBackdrop1Click(Sender: TObject);
    procedure DeleteBackdrop1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure trkRedChange(Sender: TObject);
    procedure trkGreenChange(Sender: TObject);
    procedure trkBlueChange(Sender: TObject);
    procedure trkFactorChange(Sender: TObject);
    procedure UpdateBackdrop1Click(Sender: TObject);
    procedure redradioClick(Sender: TObject);
    procedure greenradioClick(Sender: TObject);
    procedure blueradioClick(Sender: TObject);
    procedure QuickBack;
    {$IFDEF VisualCLX}
    procedure qbpresetsDrawItem(Sender: TObject; Index: Integer;
      Rect: TRect; State: TOwnerDrawState; var Handled: Boolean);
    {$ENDIF VisualCLX}
  private
    FPainterForm: TJvDrawImage;
  public
    function StrToQuickBack(S: string): Boolean;
    procedure SetDrawImage(ADrawImage: TJvDrawImage);
  end;

var
  QBFile: string;
  QBDRed, QBDBlue, QBDGreen: Byte;

implementation

uses
  JvConsts, JvResources, JvTypes;

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

type
  TColorProc = function(OutLoop, InLoop: Integer): Integer;

var
  RedBack, GreenBack, BlueBack: TColorProc;
  QBFuncs: array [0..50] of TColorProc;
  ImgDrawFactor: Byte;
  QBRedFn, QBGreenFn, QBBlueFn: Integer;
  ApplDir: string;

function BGProd(Inner, Outer: Integer): Integer;
begin
  Result := Outer * Inner mod ImgDrawFactor;
end;

function BGSum(Inner, Outer: Integer): Integer;
begin
  Result := (Outer + Inner) mod ImgDrawFactor;
end;

function BGSub(Inner, Outer: Integer): Integer;
begin
  Result := (Outer - Inner) mod ImgDrawFactor;
end;

function BGXor(Inner, Outer: Integer): Integer;
begin
  Result := (Outer xor Inner) mod ImgDrawFactor;
end;

function BGAnd(Inner, Outer: Integer): Integer;
begin
  Result := (Outer and Inner) mod ImgDrawFactor;
end;

function BGOutXor(Inner, Outer: Integer): Integer;
begin
  Result := Outer xor ImgDrawFactor;
end;

function BGInXor(Inner, Outer: Integer): Integer;
begin
  Result := Inner xor ImgDrawFactor;
end;

function BGOutAnd(Inner, Outer: Integer): Integer;
begin
  Result := Outer and ImgDrawFactor;
end;

function BGInAnd(Inner, Outer: Integer): Integer;
begin
  Result := Inner and ImgDrawFactor;
end;

function BGOutMod(Inner, Outer: Integer): Integer;
begin
  Result := Outer mod ImgDrawFactor;
end;

function BGInMod(Inner, Outer: Integer): Integer;
begin
  Result := Inner mod ImgDrawFactor;
end;

function BGProdXor(Inner, Outer: Integer): Integer;
begin
  Result := (Outer * Inner) xor ImgDrawFactor;
end;

function BGSumXor(Inner, Outer: Integer): Integer;
begin
  Result := (Outer + Inner) xor ImgDrawFactor;
end;

function BGSubXor(Inner, Outer: Integer): Integer;
begin
  Result := (Outer - Inner) xor ImgDrawFactor;
end;

function BGProdAnd(Inner, Outer: Integer): Integer;
begin
  Result := (Outer * Inner) and ImgDrawFactor;
end;

function BGSumAnd(Inner, Outer: Integer): Integer;
begin
  Result := (Outer + Inner) and ImgDrawFactor;
end;

function BGSubAnd(Inner, Outer: Integer): Integer;
begin
  Result := (Outer - Inner) and ImgDrawFactor;
end;

function BGInner(Inner, Outer: Integer): Integer;
begin
  Result := Inner;
end;

function BGOuter(Inner, Outer: Integer): Integer;
begin
  Result := Outer;
end;

function BGOutRed(Inner, Outer: Integer): Integer;
begin
  Result := QBDRed * Outer;
end;

function BGInRed(Inner, Outer: Integer): Integer;
begin
  Result := QBDRed * Inner;
end;

function BGOutGreen(Inner, Outer: Integer): Integer;
begin
  Result := QBDGreen * Outer;
end;

function BGInGreen(Inner, Outer: Integer): Integer;
begin
  Result := QBDGreen * Inner;
end;

function BGOutBlue(Inner, Outer: Integer): Integer;
begin
  Result := QBDBlue * Outer;
end;

function BGInBlue(Inner, Outer: Integer): Integer;
begin
  Result := QBDBlue * Inner;
end;

function BGInModOut(Inner, Outer: Integer): Integer;
begin
  if Outer < ImgDrawFactor then
    Outer := ImgDrawFactor;
  Result := Inner mod Outer;
end;

function BGOutModIn(Inner, Outer: Integer): Integer;
begin
  if Inner < ImgDrawFactor then
    Inner := ImgDrawFactor;
  Result := Outer mod Inner;
end;

function BGOutModIn2(Inner, Outer: Integer): Integer;
begin
  Result := Outer mod (2 + Inner) mod (2 + Outer + Inner);
end;

function BGModMod(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) * (Inner mod ImgDrawFactor);
end;

function BGModModXor(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) xor (Inner mod ImgDrawFactor);
end;

function BGMod3(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) mod ((Inner mod ImgDrawFactor) + 1);
end;

function BGModModSub(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) - (Inner mod ImgDrawFactor);
end;

function BGModModAdd(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) + (Inner mod ImgDrawFactor);
end;

function BGModModAnd(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) and (Inner mod ImgDrawFactor);
end;

function BGModModOr(Inner, Outer: Integer): Integer;
begin
  Result := (Outer mod ImgDrawFactor) or (Inner mod ImgDrawFactor);
end;

function BGXOr3(Inner, Outer: Integer): Integer;
begin
  Result := Outer xor ImgDrawFactor xor Inner;
end;

function BGXOr3Mod(Inner, Outer: Integer): Integer;
begin
  Result := (Outer xor Inner mod ImgDrawFactor) xor (Inner mod ImgDrawFactor);
end;

function BGSubXorSum(Inner, Outer: Integer): Integer;
begin
  Result := (Outer - Inner) xor (Outer + Inner);
end;

function BGSubProdSum(Inner, Outer: Integer): Integer;
begin
  Result := (Outer - Inner) * (Outer + Inner);
end;

function BGProdProdSum(Inner, Outer: Integer): Integer;
begin
  Result := (Outer * Inner) * (Outer + Inner);
end;

function BGDrawXor(Inner, Outer: Integer): Integer;
begin
  Result := (Outer - imgDrawFactor) xor (ImgDrawFactor + Inner);
end;

// end of functions used in Quick Background

procedure SetQBFuncs;
begin
  QBFuncs[0] := BGProd;
  QBFuncs[1] := BGSum;
  QBFuncs[2] := BGSub;
  QBFuncs[3] := BGXor;
  QBFuncs[4] := BGAnd;
  QBFuncs[5] := BGOutAnd;
  QBFuncs[6] := BGInAnd;
  QBFuncs[7] := BGOutXor;
  QBFuncs[8] := BGInXor;
  QBFuncs[9] := BGOutMod;
  QBFuncs[10] := BGInMod;
  QBFuncs[11] := BGProdXor;
  QBFuncs[12] := BGSumXor;
  QBFuncs[13] := BGSubXor;
  QBFuncs[14] := BGProdAnd;
  QBFuncs[15] := BGSumAnd;
  QBFuncs[16] := BGSubAnd;
  QBFuncs[17] := BGInner;
  QBFuncs[18] := BGOuter;
  QBFuncs[19] := BGOutRed;
  QBFuncs[20] := BGInRed;
  QBFuncs[21] := BGOutGreen;
  QBFuncs[22] := BGInGreen;
  QBFuncs[23] := BGOutblue;
  QBFuncs[24] := BGInBlue;
  QBFuncs[25] := BGInModOut;
  QBFuncs[26] := BGOutModIn;
  QBFuncs[27] := BGOutModIn2;
  QBFuncs[28] := BGModMod;
  QBFuncs[29] := BGModModXor;
  QBFuncs[30] := BGMod3;
  QBFuncs[31] := BGModModSub;
  QBFuncs[32] := BGModModAdd;
  QBFuncs[33] := BGModModAnd;
  QBFuncs[34] := BGModModOr;
  QBFuncs[35] := BGXOr3;
  QBFuncs[36] := BGXOr3Mod;
  QBFuncs[37] := BGSubXorSum;
  QBFuncs[38] := BGSubProdSum;
  QBFuncs[39] := BGProdProdSum;
  QBfuncs[40] := BGDrawXor;
end;

procedure TPainterQBForm.QuickBack;
var
  Bmp: TBitmap;
  I, J: Integer;
  Line: PJvRGBArray;
begin
  RedBack := QBFuncs[QBRedFn];
  GreenBack := QBFuncs[QBGreenFn];
  BlueBack := QBFuncs[QBBlueFn];
  Bmp := TBitmap.Create;
  try
    Bmp.Assign(FPainterForm.Picture.Bitmap);
    Bmp.PixelFormat := pf24bit;
    for I := 0 to Bmp.Height - 1 do
    begin
      Line := Bmp.ScanLine[I];
      for J := 0 to Bmp.Width - 1 do
      begin
        Line[J].rgbRed   := QBDRed + RedBack(I, J);
        Line[J].rgbGreen := QBDGreen + GreenBack(I, J);
        Line[J].rgbBlue  := QBDBlue + BlueBack(I, J);
      end;
    end;
    FPainterForm.Preview(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TPainterQBForm.QBListClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := QBList.ItemIndex;
  if redradio.Checked then
    QBRedFn := Index;
  if greenradio.Checked then
    QBGreenFn := Index;
  if blueradio.Checked then
    QBBlueFn := Index;
  SetLabels;
  QuickBack;
end;

procedure TPainterQBForm.FormShow(Sender: TObject);
begin
  SetLabels;
end;

{$IFDEF VCL}
procedure TPainterQBForm.qbpresetsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TPainterQBForm.qbpresetsDrawItem(Sender: TObject; Index: Integer;
  Rect: TRect; State: TOwnerDrawState; var Handled: Boolean);
{$ENDIF VisualCLX}
var
  S: string;
  P: Integer;
begin
  S := qbpresets.Items[Index];
  P := Pos('=', S);
  S := Copy(S, 1, P - 1);
  qbpresets.Canvas.TextRect(Rect, Rect.Left, Rect.Top, S);
  {$IFDEF VisualCLX}
  Handled := True;
  {$ENDIF VisualCLX}
end;

procedure TPainterQBForm.qbpresetsClick(Sender: TObject);
begin
  if qbpresets.ItemIndex >= 0 then
    StrToQuickBack(qbpresets.Items[qbpresets.ItemIndex]);
end;

procedure TPainterQBForm.SetLabels;
begin
  redradio.Caption := qblist.Items[QBRedFn];
  greenradio.Caption := qblist.Items[QBGreenFn];
  blueradio.Caption := qblist.Items[QBBlueFn];
end;

procedure TPainterQBForm.AddBackdrop1Click(Sender: TObject);
var
  S: string;
begin
  S := InputBox(RsPainterQuickBackdrops, RsEnterName, '');
  if S = '' then
    Exit;
  S := S + '=' +
    IntToStr(QBRedFn) + ',' +
    IntToStr(QBGreenFn) + ',' +
    IntToStr(QBBlueFn) + ',' +
    IntToStr(QBDRed) + ',' +
    IntToStr(QBDGreen) + ',' +
    IntToStr(QBDBlue) + ',' +
    IntToStr(ImgDrawFactor);
  qbpresets.Items.Append(S);
  qbpresets.Items.SaveToFile(QBFile);
end;

procedure TPainterQBForm.DeleteBackdrop1Click(Sender: TObject);
begin
  if qbpresets.ItemIndex >= 0 then
  begin
    qbpresets.Items.Delete(qbpresets.ItemIndex);
    qbpresets.Items.SaveToFile(QBFile);
  end;
end;

procedure TPainterQBForm.FormCreate(Sender: TObject);
begin
  ImgDrawFactor := 255;
  QBDRed := 0;
  QBDBlue := 0;
  QBDGreen := 0;
  QBRedFn := 0;
  QBGreenFn := 0;
  QBBlueFn := 0;
  SetQBFuncs;
  QBFile := ApplDir + 'PainterQB.txt';
  if FileExists(QBFile) then
    qbpresets.Items.LoadFromFile(QBFile);
end;

procedure TPainterQBForm.trkRedChange(Sender: TObject);
begin
  QBDRed := trkRed.Position;
  QuickBack;
end;

procedure TPainterQBForm.trkGreenChange(Sender: TObject);
begin
  QBDGreen := trkGreen.Position;
  QuickBack;
end;

procedure TPainterQBForm.trkBlueChange(Sender: TObject);
begin
  QBDBlue := trkBlue.Position;
  QuickBack;
end;

procedure TPainterQBForm.trkFactorChange(Sender: TObject);
begin
  ImgDrawFactor := trkFactor.Position;
  QuickBack;
end;

procedure TPainterQBForm.UpdateBackdrop1Click(Sender: TObject);
var
  S: string;
  P: Integer;
begin
  if qbpresets.ItemIndex < 0 then
  begin
    ShowMessage(RsNoItemSelected);
    Exit;
  end;
  S := qbpresets.Items[qbpresets.ItemIndex];
  P := Pos('=', S);
  S := Copy(S, 1, P - 1);
  S := InputBox(RsPainterQuickBackdrops, RsEnterName, S);
  if S = '' then
    Exit;
  S := S + '=' +
    IntToStr(QBRedFn) + ',' +
    IntToStr(QBGreenFn) + ',' +
    IntToStr(QBBlueFn) + ',' +
    IntToStr(QBDRed) + ',' +
    IntToStr(QBDGreen) + ',' +
    IntToStr(QBDBlue) + ',' +
    IntToStr(ImgDrawFactor);
  qbpresets.Items[qbpresets.ItemIndex] := S;
  qbpresets.Items.SaveToFile(QBFile);
end;

function TPainterQBForm.StrToQuickBack(S: string): Boolean;
var
  P: Integer;
  List: TStringList;
begin
  Result := False;
  P := Pos('=', S);
  if P = 0 then
    Exit;
  S := Copy(S, P + 1, Length(S));
  List := TStringList.Create;
  try
    try
      List.CommaText := S;
      QBRedFn := StrToInt(List[0]);
      QBGreenFn := StrToInt(List[1]);
      QBBlueFn := StrToInt(List[2]);
      QBDRed := StrToInt(List[3]);
      trkRed.Position := QBDRed;
      QBDGreen := StrToInt(List[4]);
      trkGreen.Position := QBDGreen;
      QBDBlue := StrToInt(List[5]);
      trkBlue.Position := QBDBlue;
      ImgDrawFactor := StrToInt(List[6]);
      trkFactor.Position := ImgDrawFactor;
      SetLabels;
      QuickBack;
      Result := True;
    except
      ShowMessage(RsErrorInPresets);
      Result := False;
    end;
  finally
    List.Free;
  end;
end;

procedure TPainterQBForm.redradioClick(Sender: TObject);
begin
  QBList.ItemIndex := QBRedFn;
end;

procedure TPainterQBForm.greenradioClick(Sender: TObject);
begin
  QBList.ItemIndex := QBGreenFn;
end;

procedure TPainterQBForm.blueradioClick(Sender: TObject);
begin
  QBList.ItemIndex := QBBlueFn;
end;

procedure TPainterQBForm.SetDrawImage(ADrawImage: TJvDrawImage);
begin
  FPainterForm := ADrawImage;
end;

end.
