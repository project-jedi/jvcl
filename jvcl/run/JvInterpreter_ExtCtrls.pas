{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_ExtCtrls.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_ExtCtrls;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Graphics, Controls, ExtCtrls;

{ TShape }

{ constructor Create(AOwner: TComponent) }

procedure TShape_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TShape.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure StyleChanged(Sender: TObject); }

procedure TShape_StyleChanged(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TShape(Args.Obj).StyleChanged(V2O(Args.Values[0]));
end;

{ property Read Brush: TBrush }

procedure TShape_Read_Brush(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TShape(Args.Obj).Brush);
end;

{ property Write Brush(Value: TBrush) }

procedure TShape_Write_Brush(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TShape(Args.Obj).Brush := V2O(Value) as TBrush;
end;

{ property Read Pen: TPen }

procedure TShape_Read_Pen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TShape(Args.Obj).Pen);
end;

{ property Write Pen(Value: TPen) }

procedure TShape_Write_Pen(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TShape(Args.Obj).Pen := V2O(Value) as TPen;
end;

{ property Read Shape: TShapeType }

procedure TShape_Read_Shape(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TShape(Args.Obj).Shape;
end;

{ property Write Shape(Value: TShapeType) }

procedure TShape_Write_Shape(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TShape(Args.Obj).Shape := Value;
end;

{ TPaintBox }

{ constructor Create(AOwner: TComponent) }

procedure TPaintBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPaintBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TImage }

{ constructor Create(AOwner: TComponent) }

procedure TImage_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TImage.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Canvas: TCanvas }

procedure TImage_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TImage(Args.Obj).Canvas);
end;

{ property Read AutoSize: Boolean }

procedure TImage_Read_AutoSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImage(Args.Obj).AutoSize;
end;

{ property Write AutoSize(Value: Boolean) }

procedure TImage_Write_AutoSize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImage(Args.Obj).AutoSize := Value;
end;

{ property Read Center: Boolean }

procedure TImage_Read_Center(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImage(Args.Obj).Center;
end;

{ property Write Center(Value: Boolean) }

procedure TImage_Write_Center(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImage(Args.Obj).Center := Value;
end;

{ property Read IncrementalDisplay: Boolean }

procedure TImage_Read_IncrementalDisplay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImage(Args.Obj).IncrementalDisplay;
end;

{ property Write IncrementalDisplay(Value: Boolean) }

procedure TImage_Write_IncrementalDisplay(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImage(Args.Obj).IncrementalDisplay := Value;
end;

{ property Read Picture: TPicture }

procedure TImage_Read_Picture(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TImage(Args.Obj).Picture);
end;

{ property Write Picture(Value: TPicture) }

procedure TImage_Write_Picture(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImage(Args.Obj).Picture := V2O(Value) as TPicture;
end;

{ property Read Stretch: Boolean }

procedure TImage_Read_Stretch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImage(Args.Obj).Stretch;
end;

{ property Write Stretch(Value: Boolean) }

procedure TImage_Write_Stretch(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImage(Args.Obj).Stretch := Value;
end;

{ property Read Transparent: Boolean }

procedure TImage_Read_Transparent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TImage(Args.Obj).Transparent;
end;

{ property Write Transparent(Value: Boolean) }

procedure TImage_Write_Transparent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TImage(Args.Obj).Transparent := Value;
end;

{ TBevel }

{ constructor Create(AOwner: TComponent) }

procedure TBevel_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBevel.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Shape: TBevelShape }

procedure TBevel_Read_Shape(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBevel(Args.Obj).Shape;
end;

{ property Write Shape(Value: TBevelShape) }

procedure TBevel_Write_Shape(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBevel(Args.Obj).Shape := Value;
end;

{ property Read Style: TBevelStyle }

procedure TBevel_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBevel(Args.Obj).Style;
end;

{ property Write Style(Value: TBevelStyle) }

procedure TBevel_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBevel(Args.Obj).Style := Value;
end;

{ TTimer }

{ constructor Create(AOwner: TComponent) }

procedure TTimer_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TTimer.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Enabled: Boolean }

procedure TTimer_Read_Enabled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TTimer(Args.Obj).Enabled;
end;

{ property Write Enabled(Value: Boolean) }

procedure TTimer_Write_Enabled(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTimer(Args.Obj).Enabled := Value;
end;

{ property Read Interval: Cardinal }

procedure TTimer_Read_Interval(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TTimer(Args.Obj).Interval);
end;

{ property Write Interval(Value: Cardinal) }

procedure TTimer_Write_Interval(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TTimer(Args.Obj).Interval := Value;
end;

{ TCustomPanel }

{ TPanel }

{ constructor Create(AOwner: TComponent) }

procedure TPanel_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPanel.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TPage }

{ constructor Create(AOwner: TComponent) }

procedure TPage_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TPage.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TNotebook }

{ constructor Create(AOwner: TComponent) }

procedure TNotebook_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TNotebook.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ActivePage: string }

procedure TNotebook_Read_ActivePage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TNotebook(Args.Obj).ActivePage;
end;

{ property Write ActivePage(Value: string) }

procedure TNotebook_Write_ActivePage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TNotebook(Args.Obj).ActivePage := Value;
end;

{ property Read PageIndex: Integer }

procedure TNotebook_Read_PageIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TNotebook(Args.Obj).PageIndex;
end;

{ property Write PageIndex(Value: Integer) }

procedure TNotebook_Write_PageIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TNotebook(Args.Obj).PageIndex := Value;
end;

{ property Read Pages: TStrings }

procedure TNotebook_Read_Pages(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TNotebook(Args.Obj).Pages);
end;

{ property Write Pages(Value: TStrings) }

procedure TNotebook_Write_Pages(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TNotebook(Args.Obj).Pages := V2O(Value) as TStrings;
end;

{ THeader }

{ constructor Create(AOwner: TComponent) }

procedure THeader_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeader.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read SectionWidth[Integer]: Integer }

procedure THeader_Read_SectionWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THeader(Args.Obj).SectionWidth[Args.Values[0]];
end;

{ property Write SectionWidth[Integer]: Integer }

procedure THeader_Write_SectionWidth(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THeader(Args.Obj).SectionWidth[Args.Values[0]] := Value;
end;

{ property Read AllowResize: Boolean }

procedure THeader_Read_AllowResize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THeader(Args.Obj).AllowResize;
end;

{ property Write AllowResize(Value: Boolean) }

procedure THeader_Write_AllowResize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THeader(Args.Obj).AllowResize := Value;
end;

{ property Read BorderStyle: TBorderStyle }

procedure THeader_Read_BorderStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := THeader(Args.Obj).BorderStyle;
end;

{ property Write BorderStyle(Value: TBorderStyle) }

procedure THeader_Write_BorderStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THeader(Args.Obj).BorderStyle := Value;
end;

{ property Read Sections: TStrings }

procedure THeader_Read_Sections(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(THeader(Args.Obj).Sections);
end;

{ property Write Sections(Value: TStrings) }

procedure THeader_Write_Sections(const Value: Variant; Args: TJvInterpreterArgs);
begin
  THeader(Args.Obj).Sections := V2O(Value) as TStrings;
end;

{ TCustomRadioGroup }

{ TRadioGroup }

{ constructor Create(AOwner: TComponent) }

procedure TRadioGroup_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TRadioGroup.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TSplitter }

{ constructor Create(AOwner: TComponent) }

procedure TSplitter_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TSplitter.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Beveled: Boolean }

procedure TSplitter_Read_Beveled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSplitter(Args.Obj).Beveled;
end;

{ property Write Beveled(Value: Boolean) }

procedure TSplitter_Write_Beveled(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSplitter(Args.Obj).Beveled := Value;
end;

{ property Read MinSize: NaturalNumber }

procedure TSplitter_Read_MinSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TSplitter(Args.Obj).MinSize;
end;

{ property Write MinSize(Value: NaturalNumber) }

procedure TSplitter_Write_MinSize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TSplitter(Args.Obj).MinSize := Value;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cExtCtrls = 'ExtCtrls';
begin
  with JvInterpreterAdapter do
  begin
    { TShapeType }
    AddConst(cExtCtrls, 'stRectangle', Ord(stRectangle));
    AddConst(cExtCtrls, 'stSquare', Ord(stSquare));
    AddConst(cExtCtrls, 'stRoundRect', Ord(stRoundRect));
    AddConst(cExtCtrls, 'stRoundSquare', Ord(stRoundSquare));
    AddConst(cExtCtrls, 'stEllipse', Ord(stEllipse));
    AddConst(cExtCtrls, 'stCircle', Ord(stCircle));
    { TShape }
    AddClass(cExtCtrls, TShape, 'TShape');
    AddGet(TShape, 'Create', TShape_Create, 1, [varEmpty], varEmpty);
    AddGet(TShape, 'StyleChanged', TShape_StyleChanged, 1, [varEmpty], varEmpty);
    AddGet(TShape, 'Brush', TShape_Read_Brush, 0, [varEmpty], varEmpty);
    AddSet(TShape, 'Brush', TShape_Write_Brush, 0, [varEmpty]);
    AddGet(TShape, 'Pen', TShape_Read_Pen, 0, [varEmpty], varEmpty);
    AddSet(TShape, 'Pen', TShape_Write_Pen, 0, [varEmpty]);
    AddGet(TShape, 'Shape', TShape_Read_Shape, 0, [varEmpty], varEmpty);
    AddSet(TShape, 'Shape', TShape_Write_Shape, 0, [varEmpty]);
    { TPaintBox }
    AddClass(cExtCtrls, TPaintBox, 'TPaintBox');
    AddGet(TPaintBox, 'Create', TPaintBox_Create, 1, [varEmpty], varEmpty);
    { TImage }
    AddClass(cExtCtrls, TImage, 'TImage');
    AddGet(TImage, 'Create', TImage_Create, 1, [varEmpty], varEmpty);
    AddGet(TImage, 'Canvas', TImage_Read_Canvas, 0, [varEmpty], varEmpty);
    AddGet(TImage, 'AutoSize', TImage_Read_AutoSize, 0, [varEmpty], varEmpty);
    AddSet(TImage, 'AutoSize', TImage_Write_AutoSize, 0, [varEmpty]);
    AddGet(TImage, 'Center', TImage_Read_Center, 0, [varEmpty], varEmpty);
    AddSet(TImage, 'Center', TImage_Write_Center, 0, [varEmpty]);
    AddGet(TImage, 'IncrementalDisplay', TImage_Read_IncrementalDisplay, 0, [varEmpty], varEmpty);
    AddSet(TImage, 'IncrementalDisplay', TImage_Write_IncrementalDisplay, 0, [varEmpty]);
    AddGet(TImage, 'Picture', TImage_Read_Picture, 0, [varEmpty], varEmpty);
    AddSet(TImage, 'Picture', TImage_Write_Picture, 0, [varEmpty]);
    AddGet(TImage, 'Stretch', TImage_Read_Stretch, 0, [varEmpty], varEmpty);
    AddSet(TImage, 'Stretch', TImage_Write_Stretch, 0, [varEmpty]);
    AddGet(TImage, 'Transparent', TImage_Read_Transparent, 0, [varEmpty], varEmpty);
    AddSet(TImage, 'Transparent', TImage_Write_Transparent, 0, [varEmpty]);
    { TBevelStyle }
    AddConst(cExtCtrls, 'bsLowered', Ord(bsLowered));
    AddConst(cExtCtrls, 'bsRaised', Ord(bsRaised));
    { TBevelShape }
    AddConst(cExtCtrls, 'bsBox', Ord(bsBox));
    AddConst(cExtCtrls, 'bsFrame', Ord(bsFrame));
    AddConst(cExtCtrls, 'bsTopLine', Ord(bsTopLine));
    AddConst(cExtCtrls, 'bsBottomLine', Ord(bsBottomLine));
    AddConst(cExtCtrls, 'bsLeftLine', Ord(bsLeftLine));
    AddConst(cExtCtrls, 'bsRightLine', Ord(bsRightLine));
    { TBevel }
    AddClass(cExtCtrls, TBevel, 'TBevel');
    AddGet(TBevel, 'Create', TBevel_Create, 1, [varEmpty], varEmpty);
    AddGet(TBevel, 'Shape', TBevel_Read_Shape, 0, [varEmpty], varEmpty);
    AddSet(TBevel, 'Shape', TBevel_Write_Shape, 0, [varEmpty]);
    AddGet(TBevel, 'Style', TBevel_Read_Style, 0, [varEmpty], varEmpty);
    AddSet(TBevel, 'Style', TBevel_Write_Style, 0, [varEmpty]);
   { TTimer }
    AddClass(cExtCtrls, TTimer, 'TTimer');
    AddGet(TTimer, 'Create', TTimer_Create, 1, [varEmpty], varEmpty);
    AddGet(TTimer, 'Enabled', TTimer_Read_Enabled, 0, [varEmpty], varEmpty);
    AddSet(TTimer, 'Enabled', TTimer_Write_Enabled, 0, [varEmpty]);
    AddGet(TTimer, 'Interval', TTimer_Read_Interval, 0, [varEmpty], varEmpty);
    AddSet(TTimer, 'Interval', TTimer_Write_Interval, 0, [varEmpty]);
    { TPanelBevel }
    AddConst(cExtCtrls, 'bvNone', Ord(bvNone));
    AddConst(cExtCtrls, 'bvLowered', Ord(bvLowered));
    AddConst(cExtCtrls, 'bvRaised', Ord(bvRaised));
    { TCustomPanel }
    AddClass(cExtCtrls, TCustomPanel, 'TCustomPanel');
    { TPanel }
    AddClass(cExtCtrls, TPanel, 'TPanel');
    AddGet(TPanel, 'Create', TPanel_Create, 1, [varEmpty], varEmpty);
    { TPage }
    AddClass(cExtCtrls, TPage, 'TPage');
    AddGet(TPage, 'Create', TPage_Create, 1, [varEmpty], varEmpty);
    { TNotebook }
    AddClass(cExtCtrls, TNotebook, 'TNotebook');
    AddGet(TNotebook, 'Create', TNotebook_Create, 1, [varEmpty], varEmpty);
    AddGet(TNotebook, 'ActivePage', TNotebook_Read_ActivePage, 0, [varEmpty], varEmpty);
    AddSet(TNotebook, 'ActivePage', TNotebook_Write_ActivePage, 0, [varEmpty]);
    AddGet(TNotebook, 'PageIndex', TNotebook_Read_PageIndex, 0, [varEmpty], varEmpty);
    AddSet(TNotebook, 'PageIndex', TNotebook_Write_PageIndex, 0, [varEmpty]);
    AddIGet(TNotebook, 'Pages', TNotebook_Read_Pages, 0, [varEmpty], varEmpty);
    AddISet(TNotebook, 'Pages', TNotebook_Write_Pages, 0, [varEmpty]);
    { THeader }
    AddClass(cExtCtrls, THeader, 'THeader');
    AddGet(THeader, 'Create', THeader_Create, 1, [varEmpty], varEmpty);
    AddGet(THeader, 'SectionWidth', THeader_Read_SectionWidth, 1, [varEmpty], varEmpty);
    AddSet(THeader, 'SectionWidth', THeader_Write_SectionWidth, 1, [varNull]);
    AddGet(THeader, 'AllowResize', THeader_Read_AllowResize, 0, [varEmpty], varEmpty);
    AddSet(THeader, 'AllowResize', THeader_Write_AllowResize, 0, [varEmpty]);
    AddGet(THeader, 'BorderStyle', THeader_Read_BorderStyle, 0, [varEmpty], varEmpty);
    AddSet(THeader, 'BorderStyle', THeader_Write_BorderStyle, 0, [varEmpty]);
    AddIGet(THeader, 'Sections', THeader_Read_Sections, 0, [varEmpty], varEmpty);
    AddISet(THeader, 'Sections', THeader_Write_Sections, 0, [varEmpty]);
    { TCustomRadioGroup }
    AddClass(cExtCtrls, TCustomRadioGroup, 'TCustomRadioGroup');
    { TRadioGroup }
    AddClass(cExtCtrls, TRadioGroup, 'TRadioGroup');
    AddGet(TRadioGroup, 'Create', TRadioGroup_Create, 1, [varEmpty], varEmpty);
    { TSplitter }
    AddClass(cExtCtrls, TSplitter, 'TSplitter');
    AddGet(TSplitter, 'Create', TSplitter_Create, 1, [varEmpty], varEmpty);
    AddGet(TSplitter, 'Beveled', TSplitter_Read_Beveled, 0, [varEmpty], varEmpty);
    AddSet(TSplitter, 'Beveled', TSplitter_Write_Beveled, 0, [varEmpty]);
    AddGet(TSplitter, 'MinSize', TSplitter_Read_MinSize, 0, [varEmpty], varEmpty);
    AddSet(TSplitter, 'MinSize', TSplitter_Write_MinSize, 0, [varEmpty]);
  end;
  RegisterClasses([TShape, TPaintBox, TImage, TBevel, TTimer, TPanel, TPage,
    TNotebook, THeader, TRadioGroup, TSplitter]);
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

