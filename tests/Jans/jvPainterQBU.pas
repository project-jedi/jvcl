{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPainterQBU.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}

unit JvPainterQBU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls, JvDrawImage;

type
  TPainterQBF = class(TForm)
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
    procedure qbpresetsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure qbpresetsClick(Sender: TObject);
    procedure setlabels;
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
  private
    { Private declarations }
    PainterF: TJvDrawImage;
  public
    { Public declarations }
    function strtoquickback(s: string): boolean;
    procedure setDrawImage(aDrawImage: TJvDrawImage);
  end;

var
  PainterQBF: TPainterQBF;
  QBFile: string;
  QBDRed, QBDBlue, QBDGreen: byte;

implementation

{$R *.DFM}

type
  TColorProc = function(outloop, inloop: integer): integer;

var
  RedBack, GreenBack, BlueBack: TcolorProc;
  QBFuncs: array[0..50] of TcolorProc;
  ImgDrawFactor: byte;
  QBRedfn, QBGreenfn, QBBluefn: integer;
  appldir: string;

  // start of functions used in Quick Background

function BGProd(inner, outer: Integer): integer;
begin
  result := outer * inner mod ImgDrawFactor;
end;

function BGSum(inner, outer: Integer): integer;
begin
  result := (outer + inner) mod ImgDrawFactor;
end;

function BGSub(inner, outer: Integer): integer;
begin
  result := (outer - inner) mod ImgDrawFactor;
end;

function BGXor(inner, outer: Integer): integer;
begin
  result := (outer xor inner) mod ImgDrawFactor;
end;

function BGAnd(inner, outer: Integer): integer;
begin
  result := (outer and inner) mod ImgDrawFactor;
end;

function BGOutXor(inner, outer: Integer): integer;
begin
  result := outer xor ImgDrawFactor;
end;

function BGInXor(inner, outer: Integer): integer;
begin
  result := inner xor ImgDrawFactor;
end;

function BGOutAnd(inner, outer: Integer): integer;
begin
  result := outer and ImgDrawFactor;
end;

function BGInAnd(inner, outer: Integer): integer;
begin
  result := inner and ImgDrawFactor;
end;

function BGOutMod(inner, outer: Integer): integer;
begin
  result := outer mod ImgDrawFactor;
end;

function BGInMod(inner, outer: Integer): integer;
begin
  result := inner mod ImgDrawFactor;
end;

function BGProdXor(inner, outer: Integer): integer;
begin
  result := (outer * inner) xor ImgDrawFactor;
end;

function BGSumXor(inner, outer: Integer): integer;
begin
  result := (outer + inner) xor ImgDrawFactor;
end;

function BGSubXor(inner, outer: Integer): integer;
begin
  result := (outer - inner) xor ImgDrawFactor;
end;

function BGProdAnd(inner, outer: Integer): integer;
begin
  result := (outer * inner) and ImgDrawFactor;
end;

function BGSumAnd(inner, outer: Integer): integer;
begin
  result := (outer + inner) and ImgDrawFactor;
end;

function BGSubAnd(inner, outer: Integer): integer;
begin
  result := (outer - inner) and ImgDrawFactor;
end;

function BGInner(inner, outer: Integer): integer;
begin
  result := inner;
end;

function BGOuter(inner, outer: Integer): integer;
begin
  result := outer;
end;

function BGOutRed(inner, outer: Integer): integer;
begin
  result := QBDRed * outer;
end;

function BGInRed(inner, outer: Integer): integer;
begin
  result := QBDRed * inner;
end;

function BGOutGreen(inner, outer: Integer): integer;
begin
  result := QBDGreen * outer;
end;

function BGInGreen(inner, outer: Integer): integer;
begin
  result := QBDGreen * inner;
end;

function BGOutBlue(inner, outer: Integer): integer;
begin
  result := QBDBlue * outer;
end;

function BGInBlue(inner, outer: Integer): integer;
begin
  result := QBDBlue * inner;
end;

function BGInModOut(inner, outer: Integer): integer;
begin
  if outer < ImgDrawFactor then
    outer := ImgDrawFactor;
  result := inner mod outer;
end;

function BGOutModIn(inner, outer: Integer): integer;
begin
  if inner < ImgDrawFactor then
    inner := ImgDrawFactor;
  result := outer mod inner;
end;

function BGOutModIn2(inner, outer: Integer): integer;
begin
  result := outer mod (2 + inner) mod (2 + outer + inner);
end;

function BGModMod(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) * (inner mod ImgDrawFactor);
end;

function BGModModXor(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) xor (inner mod ImgDrawFactor);
end;

function BGMod3(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) mod ((inner mod ImgDrawFactor) + 1);
end;

function BGModModSub(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) - (inner mod ImgDrawFactor);
end;

function BGModModAdd(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) + (inner mod ImgDrawFactor);
end;

function BGModModAnd(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) and (inner mod ImgDrawFactor);
end;

function BGModModOr(inner, outer: Integer): integer;
begin
  result := (outer mod ImgDrawFactor) or (inner mod ImgDrawFactor);
end;

function BGXOr3(inner, outer: Integer): integer;
begin
  result := outer xor ImgDrawFactor xor inner;
end;

function BGXOr3Mod(inner, outer: Integer): integer;
begin
  result := (outer xor inner mod ImgDrawFactor) xor (inner mod ImgDrawFactor);
end;

function BGSubXorSum(inner, outer: integer): integer;
begin
  result := (outer - inner) xor (outer + inner);
end;

function BGSubProdSum(inner, outer: integer): integer;
begin
  result := (outer - inner) * (outer + inner);
end;

function BGProdProdSum(inner, outer: integer): integer;
begin
  result := (outer * inner) * (outer + inner);
end;

function BGDrawXor(inner, outer: integer): integer;
begin
  result := (outer - imgDrawFactor) xor (ImgDrawFactor + inner);
end;

// end of functions used in Quick Background

procedure setQBFuncs;
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

procedure TPainterQBF.QuickBack;
var
  bmp: tbitmap;
  i, j: integer;
  line: Pbytearray;
  dest, src: trect;
begin
  RedBack := QBFuncs[QBRedfn];
  GreenBack := QBFuncs[QBGreenfn];
  BlueBack := QBFuncs[QBBluefn];
  Bmp := TBitMap.create;
  bmp.Assign(PainterF.picture.bitmap);
  bmp.pixelformat := pf24bit;
  for i := 0 to bmp.height - 1 do
  begin
    line := pbytearray(bmp.scanline[i]);
    for j := 0 to bmp.width - 1 do
    begin
      line[j * 3] := QBDRed + RedBack(i, j);
      line[j * 3 + 1] := QBDGreen + GreenBack(i, j);
      line[j * 3 + 2] := QBDBlue + BlueBack(i, j);
    end;
  end;
  PainterF.Preview(bmp);
  Bmp.free;
end;

procedure TPainterQBF.QBListClick(Sender: TObject);
var
  index: integer;
begin
  index := QBList.ItemIndex;
  if redradio.checked then
    QBRedfn := index;
  if greenradio.checked then
    QBGreenfn := index;
  if blueradio.checked then
    QBBluefn := index;
  setlabels;
  QuickBack;
end;

procedure TPainterQBF.FormShow(Sender: TObject);
begin
  setlabels;
end;

procedure TPainterQBF.qbpresetsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  s: string;
  p: integer;
begin
  s := qbpresets.Items[index];
  p := pos('=', s);
  s := copy(s, 1, p - 1);
  qbpresets.Canvas.TextRect(rect, rect.left, rect.top, s);
end;

procedure TPainterQBF.qbpresetsClick(Sender: TObject);
begin
  if qbpresets.itemindex >= 0 then
    strtoQuickBack(qbpresets.items[qbpresets.itemindex]);
end;

procedure TPainterQBF.setlabels;
begin
  redradio.caption := qblist.items[QBRedfn];
  greenradio.caption := qblist.items[QBGreenfn];
  blueradio.caption := qblist.items[QBbluefn];
end;

procedure TPainterQBF.AddBackdrop1Click(Sender: TObject);
var
  s: string;
begin
  s := inputbox('Painter Quick Backdrops', 'Enter Name:', '');
  if s = '' then exit;
  s := s + '=' + inttostr(QBRedfn) + ','
    + inttostr(QBGreenfn) + ','
    + inttostr(QBBluefn) + ','
    + inttostr(QBDRed) + ','
    + inttostr(QBDGreen) + ','
    + inttostr(QBDBlue) + ','
    + inttostr(ImgDrawFactor);
  qbpresets.Items.Append(s);
  qbpresets.Items.SaveToFile(QBFile);
end;

procedure TPainterQBF.DeleteBackdrop1Click(Sender: TObject);
begin
  if qbpresets.itemindex >= 0 then
  begin
    qbpresets.Items.Delete(qbpresets.itemindex);
    qbpresets.Items.SaveToFile(QBFile);
  end;

end;

procedure TPainterQBF.FormCreate(Sender: TObject);
begin
  ImgDrawFactor := 255;
  QBDRed := 0;
  QBDBlue := 0;
  QBDGreen := 0;
  QBRedfn := 0;
  QBGreenfn := 0;
  QBBluefn := 0;
  SetQBFuncs;
  QBFile := appldir + 'PainterQB.txt';
  if fileexists(QBFile) then
    qbpresets.Items.LoadFromFile(QBfile);
end;

procedure TPainterQBF.trkRedChange(Sender: TObject);
begin
  QBDRed := trkRed.position;
  QuickBack;
end;

procedure TPainterQBF.trkGreenChange(Sender: TObject);
begin
  QBDGreen := trkGreen.position;
  QuickBack;
end;

procedure TPainterQBF.trkBlueChange(Sender: TObject);
begin
  QBDBlue := trkBlue.position;
  QuickBack;
end;

procedure TPainterQBF.trkFactorChange(Sender: TObject);
begin
  ImgDrawFactor := trkFactor.position;
  QuickBack;
end;

procedure TPainterQBF.UpdateBackdrop1Click(Sender: TObject);
var
  s: string;
  p: integer;
begin
  if not (qbpresets.ItemIndex >= 0) then
  begin
    showmessage('No item selected!');
    exit;
  end;
  s := qbpresets.items[qbpresets.itemindex];
  p := pos('=', s);
  s := copy(s, 1, p - 1);
  s := inputbox('Painter Quick Backdrops', 'Enter Name:', s);
  if s = '' then exit;
  s := s + '=' + inttostr(QBRedfn) + ','
    + inttostr(QBGreenfn) + ','
    + inttostr(QBBluefn) + ','
    + inttostr(QBDRed) + ','
    + inttostr(QBDGreen) + ','
    + inttostr(QBDBlue) + ','
    + inttostr(ImgDrawFactor);
  qbpresets.Items[qbpresets.itemindex] := s;
  qbpresets.Items.SaveToFile(QBFile);
end;

function TPainterQBF.strtoquickback(s: string): boolean;
var
  p: integer;
  Alist: tstringlist;
begin
  result := false;
  p := pos('=', s);
  if p = 0 then exit;
  s := copy(s, p + 1, length(s));
  Alist := tstringlist.create;
  try
    Alist.commatext := s;
    QBRedfn := strtoint(Alist[0]);
    QBGreenfn := strtoint(Alist[1]);
    QBBluefn := strtoint(Alist[2]);
    QBDRed := strtoint(Alist[3]);
    trkRed.position := QBDRed;
    QBDGreen := strtoint(Alist[4]);
    trkGreen.position := QBDGreen;
    QBDBlue := strtoint(Alist[5]);
    trkBlue.position := QBDBlue;
    ImgDrawFactor := strtoint(Alist[6]);
    trkFactor.Position := ImgDrawFactor;
    setlabels;
    QuickBack;
    result := true;
  except
    showmessage('Error in Presets');
    result := false;
  end;
  Alist.free;
end;

procedure TPainterQBF.redradioClick(Sender: TObject);
begin
  QBList.ItemIndex := QBRedfn;
end;

procedure TPainterQBF.greenradioClick(Sender: TObject);
begin
  QBList.ItemIndex := QBGreenfn;
end;

procedure TPainterQBF.blueradioClick(Sender: TObject);
begin
  QBList.ItemIndex := QBBluefn;
end;

procedure TPainterQBF.setDrawImage(aDrawImage: TJvDrawImage);
begin
  PainterF := aDrawImage;
end;

end.
