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

unit JvFullColorDialogMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvFullColorSpaces, JvFullColorCtrls, StdCtrls, JvFullColorForm,
  JvFullColorDialogs;

type
  TMainForm = class(TForm)
    JvFullColorLabel: TJvFullColorLabel;
    LabelInfo: TLabel;
    JvFullColorDialog: TJvFullColorDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvFullColorLabelDblClick(Sender: TObject);
    procedure JvFullColorDialogApply(Sender: TObject;
      AFullColor: TJvFullColor);
  private
    { Déclarations privées }
  public
    procedure UpdateCaption (ALabel: TJvFullColorLabel);
    procedure UpdateAllCaptions;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Math;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  JvFullColorLabel.Caption := 'test';
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Index: Integer;
  LDEFColorSpace: TJvDEFColorSpace;
  LColorLabel: TJvFullColorLabel;
  X, Y: Integer;
  procedure CreateLabel(AFullColor: TJvFullColor);
  begin
    Inc(Y,JvFullColorLabel.Height+10);
    if Y > ClientHeight then
    begin
      Y := JvFullColorLabel.Top;
      Inc(X,JvFullColorLabel.Width+10);
    end;
    LColorLabel := TJvFullColorLabel.Create(Self);
    LColorLabel.Parent := Self;
    LColorLabel.SetBounds(X,Y,JvFullColorLabel.Width,JvFullColorLabel.Height);
    LColorLabel.LabelColor := AFullColor;
    LColorLabel.OnDblClick := JvFullColorLabelDblClick;
  end;
begin
  X := JvFullColorLabel.Left;
  Y := JvFullColorLabel.Top;
  with ColorSpaceManager do
  begin
    LDEFColorSpace := TJvDEFColorSpace(ColorSpace[csDEF]);
    for Index := 1 to Min(LDEFColorSpace.ColorCount-1,9) do
      CreateLabel(ConvertFromColor(LDEFColorSpace.ColorValue[Index]));
    with ColorSpace[csHLS] do
    begin
      CreateLabel(ConvertFromColor(clRed));
      CreateLabel(ConvertFromColor(clLime));
      CreateLabel(ConvertFromColor(clBlue));
      CreateLabel(ConvertFromColor(clYellow));
    end;
    CreateLabel(ColorSpace[csYUV].ConvertFromColor(clWhite));
    CreateLabel(ColorSpace[csYCC].ConvertFromColor(clPurple));
    CreateLabel(ColorSpace[csHSV].ConvertFromColor(clAqua));
    CreateLabel(ColorSpace[csYIQ].ConvertFromColor(clOlive));
    CreateLabel(ColorSpace[csLAB].ConvertFromColor(clMaroon));
    CreateLabel(ColorSpace[csDEF].ConvertFromColor(clAppWorkSpace));
  end;
  UpdateAllCaptions;
end;

procedure TMainForm.UpdateAllCaptions;
var
  Index: Integer;
begin
  for Index := 0 to ControlCount-1 do
    if Controls[Index] is TJvFullColorLabel then
       UpdateCaption(TJvFullColorLabel(Controls[Index]));
end;

procedure TMainForm.UpdateCaption(ALabel: TJvFullColorLabel);
var
  Index: Cardinal;
  LColor: TColor;
begin
  with ColorSpaceManager, ColorSpace[GetColorSpaceID(ALabel.LabelColor)] do
    if ID = csDEF then
      with TJvDEFColorSpace(ColorSpace[csDEF]) do
    begin
      LColor := ConvertToColor(ALabel.LabelColor);
      for Index:=0 to ColorCount-1 do
        if ColorValue[Index]=LColor then
      begin
        ALabel.Caption := Format('%s : %s',[ShortName,ColorPrettyName[Index]]);
        Break;
      end;
      if Index = ColorCount then
        ALabel.Caption := ShortName+' : Invalid color';
    end
    else ALabel.Caption := Format('%s : %s=%d ; %s=%d ; %s=%d',[ShortName,
      AxisName[axIndex0],GetAxisValue(ALabel.LabelColor,axIndex0),
      AxisName[axIndex1],GetAxisValue(ALabel.LabelColor,axIndex1),
      AxisName[axIndex2],GetAxisValue(ALabel.LabelColor,axIndex2)]);
end;

procedure TMainForm.JvFullColorLabelDblClick(Sender: TObject);
begin
  with TJvFullColorLabel(Sender) do
  begin
    JvFullColorDialog.FullColor := LabelColor;
    JvFullColorDialog.Tag := Integer(Sender);
    if JvFullColorDialog.Execute then
    begin
      LabelColor := JvFullColorDialog.FullColor;
      UpdateCaption(TJvFullColorLabel(Sender));
    end;
  end;
end;

procedure TMainForm.JvFullColorDialogApply(Sender: TObject;
  AFullColor: TJvFullColor);
begin
  with TJvFullColorDialog(Sender) do
  begin
    TJvFullColorLabel(Tag).LabelColor := FullColor;
    UpdateCaption(TJvFullColorLabel(Tag));
  end;
end;

end.
