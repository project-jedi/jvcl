{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2004 Project JEDI

 Original author: Florent Ouchet [ouchet dott florent att laposte dott net]

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

unit JvFullColorCircleDialogMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, JvFullColorSpaces, JvFullColorCircleForm,
  JvFullColorDialogs, JvFullColorRotate;

type
  TFormMain = class(TForm)
    Image: TImage;
    Bevel: TBevel;
    Memo: TMemo;
    LabelImage: TLabel;
    ComboBoxFileName: TComboBox;
    JvFullColorCircleDialog: TJvFullColorCircleDialog;
    procedure FormCreate(Sender: TObject);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MemoKeyPress(Sender: TObject; var Key: Char);
    procedure ComboBoxFileNameSelect(Sender: TObject);
  public
    Images: array [0..6] of TImage;
    Memos: array [0..6] of TMemo;
    procedure CustomizeDblClick(Sender: TObject);
    procedure RotateCustomValues;
    procedure FormatMemo(AMemo: TMemo; const Delta: TJvColorDelta);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

resourcestring
  RsCustomize = 'Dbl-click to customize';

var
  ColorDeltas: array [0..5] of TJvColorDelta =
  ( (ColorID: csRGB; AxisRed:  ((Value:100; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value: 0; SaturationMethod:smRange));
                     AxisGreen:((Value:  0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value: 0; SaturationMethod:smRange));
                     AxisBlue: ((Value:  0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value:50; SaturationMethod:smRange)) ),
    (ColorID: csHLS; AxisRed:  ((Value:  0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value: 0; SaturationMethod:smRange));
                     AxisGreen:((Value: 40; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value: 0; SaturationMethod:smRange));
                     AxisBlue: ((Value:  0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value: 0; SaturationMethod:smRange)) ),
    (ColorID: csHSV; AxisRed:  ((Value:  0; SaturationMethod:smRange), (Value:-176; SaturationMethod:smRange), (Value:-180; SaturationMethod:smRange));
                     AxisGreen:((Value:  0; SaturationMethod:smRange), (Value:   0; SaturationMethod:smRange), (Value:   0; SaturationMethod:smRange));
                     AxisBlue: ((Value:  0; SaturationMethod:smRange), (Value:   0; SaturationMethod:smRange), (Value:   0; SaturationMethod:smRange)) ),
    (ColorID: csYUV; AxisRed:  ((Value:0; SaturationMethod:smRange), (Value: 38; SaturationMethod:smRange), (Value:-100; SaturationMethod:smRange));
                     AxisGreen:((Value:0; SaturationMethod:smRange), (Value:168; SaturationMethod:smLoop),  (Value:   0; SaturationMethod:smRange));
                     AxisBlue: ((Value:0; SaturationMethod:smRange), (Value:  0; SaturationMethod:smRange), (Value:   0; SaturationMethod:smRange)) ),
    (ColorID: csHLS; AxisRed:  ((Value:0; SaturationMethod:smRange), (Value:-30; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange));
                     AxisGreen:((Value:0; SaturationMethod:smRange), (Value:-30; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange));
                     AxisBlue: ((Value:0; SaturationMethod:smRange), (Value:-30; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange)) ),
    (ColorID: csXYZ; AxisRed:  ((Value: 0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange));
                     AxisGreen:((Value: 0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange), (Value:0; SaturationMethod:smRange));
                     AxisBlue: ((Value:80; SaturationMethod:smLoop),  (Value:0; SaturationMethod:smLoop),  (Value:0; SaturationMethod:smLoop)) ) );

procedure TFormMain.FormCreate(Sender: TObject);
var
  X, Y: Integer;
  PitchX, PitchY: Integer;
  LImage: TImage;
  LMemo: TMemo;
  LBevel: TBevel;
  Index: Integer;
  LSearchRec: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(GetCurrentDir)+'*.bmp',faAnyFile,LSearchRec) = 0 then
    repeat
      ComboBoxFileName.Items.Add(LSearchRec.Name);
    until FindNext(LSearchRec) <> 0;
  FindClose(LSearchRec);
  
  PitchX := Memo.Width + 32;
  PitchY := Memo.Top + Memo.Height - Image.Top + 31;
  Index := 0;
  Image.Picture.Bitmap := TBitmap.Create;
  for X := 0 to 3 do
    for Y := 0 to 1 do
      if (X <> 0) or (Y <> 0) then
  begin
    LBevel := TBevel.Create(Self);
    LBevel.Parent := Self;
    LBevel.Style := bsRaised;
    LBevel.SetBounds(Bevel.Left+X*PitchX, Bevel.Top+Y*PitchY, Bevel.Width, Bevel.Height);
    LImage := TImage.Create(Self);
    LImage.Parent := Self;
    LImage.Stretch := False;
    LImage.Picture.Bitmap := TBitmap.Create;
    LImage.SetBounds(Image.Left+X*PitchX, Image.Top+Y*PitchY, Image.Width, Image.Height);
    LMemo := TMemo.Create(Self);
    LMemo.Parent := Self;
    LMemo.BorderStyle := bsNone;
    LMemo.ParentColor := True;
    LMemo.OnKeyDown := MemoKeyDown;
    LMemo.OnKeyPress := MemoKeyPress;
    LMemo.SetBounds(Memo.Left+X*PitchX, Memo.Top+Y*PitchY, Memo.Width, Memo.Height);
    LMemo.Alignment := taCenter;
    if (X = 3) and (Y = 1) then
    begin
      LImage.OnDblClick := CustomizeDblClick;
      LMemo.OnDblClick := CustomizeDblClick;
      ClientWidth := LMemo.Left+LMemo.Width-1+Memo.Left;
      ClientHeight := LMemo.Top+LMemo.Height-1+Image.Top;
    end;
    Memos[Index] := LMemo;
    Images[Index] := LImage;
    Inc(Index);
  end;
  ComboBoxFileName.ItemIndex := 0;
  ComboBoxFileNameSelect(ComboBoxFileName);
end;

procedure TFormMain.CustomizeDblClick(Sender: TObject);
begin
  if JvFullColorCircleDialog.Execute then
    RotateCustomValues;
end;

procedure TFormMain.MemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Key := 0;          // discard any key but Enabled=False affects the text rendering
end;

procedure TFormMain.MemoKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;         // discard any key but Enabled=False affects the text rendering
end;

procedure TFormMain.ComboBoxFileNameSelect(Sender: TObject);
var
  Index: Integer;
begin
  if Image.Picture.Bitmap <> nil then
    Image.Picture.Bitmap := TBitmap.Create;
  Image.Picture.Bitmap.LoadFromFile(ComboBoxFileName.Items[ComboBoxFileName.ItemIndex]);
  with Memos[6].Lines do
  begin
    Clear;
    Add(RsCustomize);
  end;
  Images[6].Picture.Bitmap.FreeImage;
  for Index := Low(Images) to High(Images)-1 do
  begin
    Images[Index].Picture.Bitmap.FreeImage;
    RotateBitmap(Image.Picture.Bitmap,Images[Index].Picture.Bitmap,ColorDeltas[Index]);
    FormatMemo(Memos[Index],ColorDeltas[Index]);
  end;
  RotateCustomValues;
end;

procedure TFormMain.RotateCustomValues;
begin
  RotateBitmap(Image.Picture.Bitmap,Images[6].Picture.Bitmap,JvFullColorCircleDialog.Delta);
  FormatMemo(Memos[6],JvFullColorCircleDialog.Delta);
end;

procedure TFormMain.FormatMemo(AMemo: TMemo; const Delta: TJvColorDelta);
var
  Index: TJvAxisIndex;
begin
  AMemo.Lines.Clear;
  with ColorSpaceManager, ColorSpace[Delta.ColorID], AMemo.Lines do
  begin
    Add(Format('%s (%s)',[Name, ShortName]));
    for Index := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      Add(Format('%s : %d, %d, %d',[AxisName[Index],Delta.AxisRed[Index].Value,
                 Delta.AxisGreen[Index].Value,Delta.AxisBlue[Index].Value]));
    if AMemo = Memos[6] then
      Add(RsCustomize);
  end;
end;

end.
