{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_Quickrpt.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvInterpreter_Quickrpt;

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  SysUtils, Classes, Controls, Forms, Graphics, Db,
  {$IFDEF JVCL_UseQuickReport}
  QrPrntr, Quickrpt, QrCtrls,
  {$ENDIF}
  JvInterpreterFm, JvResources, JvTypes;

const
  cQuickRep1 = 'QuickRep1';

procedure JvInterpreterRunReportPreview(const FileName: string);
var
  Form: TForm;
  QuickRep1: TQuickRep;
  I: Integer;
begin
  Form := JvInterpreterMakeForm(FileName);
  try
    QuickRep1 := Form.FindComponent(cQuickRep1) as TQuickRep;
    if QuickRep1 = nil then
      for I := 0 to Form.ComponentCount - 1 do
        if Form.Components[I] is TQuickRep then
        begin
          QuickRep1 := Form.Components[I] as TQuickRep;
          Break;
        end;
    if QuickRep1 = nil then
      raise EJVCLException.CreateRes(@RsENoQuickReportFound);
    QuickRep1.Preview;
  finally
    Form.Free;
  end;
end;

procedure JvInterpreterRunReportPreview2(const FileName: string; JvInterpreterProgram: TJvInterpreterFm);
var
  Form: TForm;
  QuickRep1: TQuickRep;
  I: Integer;
begin
  Form := JvInterpreterProgram.MakeForm(FileName);
  try
    QuickRep1 := Form.FindComponent(cQuickRep1) as TQuickRep;
    if QuickRep1 = nil then
      for I := 0 to Form.ComponentCount - 1 do
        if Form.Components[I] is TQuickRep then
        begin
          QuickRep1 := Form.Components[I] as TQuickRep;
          Break;
        end;
    if QuickRep1 = nil then
      raise EJVCLException.CreateRes(@RsENoQuickReportFound);
    QuickRep1.Preview;
  finally
    Form.Free;
  end;
end;

{ TQRController }

{ constructor Create(AOwner: TComponent) }

procedure TQRController_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure AddNotifyClient(Value: TQRPrintable); }

procedure TQRController_AddNotifyClient(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).AddNotifyClient(V2O(Args.Values[0]) as TQRPrintable);
end;

{ property Read DataSet: TDataSet }

procedure TQRController_Read_DataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).DataSet);
end;

{ property Write DataSet(Value: TDataSet) }

procedure TQRController_Write_DataSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).DataSet := V2O(Value) as TDataSet;
end;

{ property Read DetailNumber: Integer }

procedure TQRController_Read_DetailNumber(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRController(Args.Obj).DetailNumber;
end;

{ property Read Detail: TQRCustomBand }

procedure TQRController_Read_Detail(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).Detail);
end;

{ property Write Detail(Value: TQRCustomBand) }

procedure TQRController_Write_Detail(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).Detail := V2O(Value) as TQRCustomBand;
end;

{ property Read Footer: TQRCustomBand }

procedure TQRController_Read_Footer(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).Footer);
end;

{ property Write Footer(Value: TQRCustomBand) }

procedure TQRController_Write_Footer(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).Footer := V2O(Value) as TQRCustomBand;
end;

{ property Read Header: TQRCustomBand }

procedure TQRController_Read_Header(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).Header);
end;

{ property Write Header(Value: TQRCustomBand) }

procedure TQRController_Write_Header(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).Header := V2O(Value) as TQRCustomBand;
end;

{ property Read Master: TComponent }

procedure TQRController_Read_Master(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).Master);
end;

{ property Write Master(Value: TComponent) }

procedure TQRController_Write_Master(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).Master := V2O(Value) as TComponent;
end;

{ property Read ParentReport: TQuickRep }

procedure TQRController_Read_ParentReport(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).ParentReport);
end;

{ property Write ParentReport(Value: TQuickRep) }

procedure TQRController_Write_ParentReport(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).ParentReport := V2O(Value) as TQuickRep;
end;

{ property Read PrintBefore: Boolean }

procedure TQRController_Read_PrintBefore(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRController(Args.Obj).PrintBefore;
end;

{ property Write PrintBefore(Value: Boolean) }

procedure TQRController_Write_PrintBefore(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).PrintBefore := Value;
end;

{ property Read PrintIfEmpty: Boolean }

procedure TQRController_Read_PrintIfEmpty(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRController(Args.Obj).PrintIfEmpty;
end;

{ property Write PrintIfEmpty(Value: Boolean) }

procedure TQRController_Write_PrintIfEmpty(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).PrintIfEmpty := Value;
end;

{ property Read SelfCheck: TComponent }

procedure TQRController_Read_SelfCheck(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRController(Args.Obj).SelfCheck);
end;

{ property Write SelfCheck(Value: TComponent) }

procedure TQRController_Write_SelfCheck(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRController(Args.Obj).SelfCheck := V2O(Value) as TComponent;
end;

{ TQRFrame }

{ constructor Create }

procedure TQRFrame_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRFrame.Create);
end;

{ function AnyFrame: Boolean; }

procedure TQRFrame_AnyFrame(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).AnyFrame;
end;

{ property Read Parent: TControl }

procedure TQRFrame_Read_Parent(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRFrame(Args.Obj).Parent);
end;

{ property Write Parent(Value: TControl) }

procedure TQRFrame_Write_Parent(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).Parent := V2O(Value) as TControl;
end;

{ property Read Color: TColor }

procedure TQRFrame_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TQRFrame_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).Color := Value;
end;

{ property Read DrawTop: Boolean }

procedure TQRFrame_Read_DrawTop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).DrawTop;
end;

{ property Write DrawTop(Value: Boolean) }

procedure TQRFrame_Write_DrawTop(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).DrawTop := Value;
end;

{ property Read DrawBottom: Boolean }

procedure TQRFrame_Read_DrawBottom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).DrawBottom;
end;

{ property Write DrawBottom(Value: Boolean) }

procedure TQRFrame_Write_DrawBottom(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).DrawBottom := Value;
end;

{ property Read DrawLeft: Boolean }

procedure TQRFrame_Read_DrawLeft(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).DrawLeft;
end;

{ property Write DrawLeft(Value: Boolean) }

procedure TQRFrame_Write_DrawLeft(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).DrawLeft := Value;
end;

{ property Read DrawRight: Boolean }

procedure TQRFrame_Read_DrawRight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).DrawRight;
end;

{ property Write DrawRight(Value: Boolean) }

procedure TQRFrame_Write_DrawRight(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).DrawRight := Value;
end;

{ property Read Style: TPenStyle }

procedure TQRFrame_Read_Style(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).Style;
end;

{ property Write Style(Value: TPenStyle) }

procedure TQRFrame_Write_Style(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).Style := Value;
end;

{ property Read Width: Integer }

procedure TQRFrame_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRFrame(Args.Obj).Width;
end;

{ property Write Width(Value: Integer) }

procedure TQRFrame_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRFrame(Args.Obj).Width := Value;
end;

{ TQRUnitBase }

{ constructor Create }

procedure TQRUnitBase_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRUnitBase.Create);
end;

{ property Read ParentReport: TQuickRep }

procedure TQRUnitBase_Read_ParentReport(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRUnitBase(Args.Obj).ParentReport);
end;

{ property Write ParentReport(Value: TQuickRep) }

procedure TQRUnitBase_Write_ParentReport(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRUnitBase(Args.Obj).ParentReport := V2O(Value) as TQuickRep;
end;

{ property Read ParentUpdating: Boolean }

procedure TQRUnitBase_Read_ParentUpdating(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRUnitBase(Args.Obj).ParentUpdating;
end;

{ property Write ParentUpdating(Value: Boolean) }

procedure TQRUnitBase_Write_ParentUpdating(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRUnitBase(Args.Obj).ParentUpdating := Value;
end;

{ property Read Resolution: Integer }

procedure TQRUnitBase_Read_Resolution(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRUnitBase(Args.Obj).Resolution;
end;

{ property Read Units: TQRUnit }

procedure TQRUnitBase_Read_Units(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRUnitBase(Args.Obj).Units;
end;

{ property Write Units(Value: TQRUnit) }

procedure TQRUnitBase_Write_Units(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRUnitBase(Args.Obj).Units := Value;
end;

{ property Read Zoom: Integer }

procedure TQRUnitBase_Read_Zoom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRUnitBase(Args.Obj).Zoom;
end;

{ property Write Zoom(Value: Integer) }

procedure TQRUnitBase_Write_Zoom(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRUnitBase(Args.Obj).Zoom := Value;
end;

{ TQRBandSize }

{ constructor Create(AParent: TQRCustomBand) }

procedure TQRBandSize_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRBandSize.Create(V2O(Args.Values[0]) as TQRCustomBand));
end;

{ property Read Length: Extended }

procedure TQRBandSize_Read_Length(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRBandSize(Args.Obj).Length;
end;

{ property Write Length(Value: Extended) }

procedure TQRBandSize_Write_Length(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRBandSize(Args.Obj).Length := Value;
end;

{ property Read Height: Extended }

procedure TQRBandSize_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRBandSize(Args.Obj).Height;
end;

{ property Write Height(Value: Extended) }

procedure TQRBandSize_Write_Height(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRBandSize(Args.Obj).Height := Value;
end;

{ property Read Width: Extended }

procedure TQRBandSize_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRBandSize(Args.Obj).Width;
end;

{ property Write Width(Value: Extended) }

procedure TQRBandSize_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRBandSize(Args.Obj).Width := Value;
end;

{ TQRPage }

{ constructor Create(AParent: TQuickRep) }

procedure TQRPage_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRPage.Create(V2O(Args.Values[0]) as TQuickRep));
end;

{ property Read BottomMargin: Extended }

procedure TQRPage_Read_BottomMargin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).BottomMargin;
end;

{ property Write BottomMargin(Value: Extended) }

procedure TQRPage_Write_BottomMargin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).BottomMargin := Value;
end;

{ property Read ColumnSpace: Extended }

procedure TQRPage_Read_ColumnSpace(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).ColumnSpace;
end;

{ property Write ColumnSpace(Value: Extended) }

procedure TQRPage_Write_ColumnSpace(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).ColumnSpace := Value;
end;

{ property Read Columns: Integer }

procedure TQRPage_Read_Columns(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).Columns;
end;

{ property Write Columns(Value: Integer) }

procedure TQRPage_Write_Columns(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).Columns := Value;
end;

{ property Read LeftMargin: Extended }

procedure TQRPage_Read_LeftMargin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).LeftMargin;
end;

{ property Write LeftMargin(Value: Extended) }

procedure TQRPage_Write_LeftMargin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).LeftMargin := Value;
end;

{ property Read Length: Extended }

procedure TQRPage_Read_Length(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).Length;
end;

{ property Write Length(Value: Extended) }

procedure TQRPage_Write_Length(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).Length := Value;
end;

{ property Read Orientation: TPrinterOrientation }

procedure TQRPage_Read_Orientation(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).Orientation;
end;

{ property Write Orientation(Value: TPrinterOrientation) }

procedure TQRPage_Write_Orientation(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).Orientation := Value;
end;

{ property Read PaperSize: TQRPaperSize }

procedure TQRPage_Read_PaperSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).PaperSize;
end;

{ property Write PaperSize(Value: TQRPaperSize) }

procedure TQRPage_Write_PaperSize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).PaperSize := Value;
end;

{ property Read RightMargin: Extended }

procedure TQRPage_Read_RightMargin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).RightMargin;
end;

{ property Write RightMargin(Value: Extended) }

procedure TQRPage_Write_RightMargin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).RightMargin := Value;
end;

{ property Read Ruler: Boolean }

procedure TQRPage_Read_Ruler(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).Ruler;
end;

{ property Write Ruler(Value: Boolean) }

procedure TQRPage_Write_Ruler(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).Ruler := Value;
end;

{ property Read TopMargin: Extended }

procedure TQRPage_Read_TopMargin(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).TopMargin;
end;

{ property Write TopMargin(Value: Extended) }

procedure TQRPage_Write_TopMargin(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).TopMargin := Value;
end;

{ property Read Width: Extended }

procedure TQRPage_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPage(Args.Obj).Width;
end;

{ property Write Width(Value: Extended) }

procedure TQRPage_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPage(Args.Obj).Width := Value;
end;

{ TQRBasePanel }

{ constructor Create(AOwner: TComponent) }

procedure TQRBasePanel_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRBasePanel.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Zoom: Integer }

procedure TQRBasePanel_Read_Zoom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRBasePanel(Args.Obj).Zoom;
end;

{ property Write Zoom(Value: Integer) }

procedure TQRBasePanel_Write_Zoom(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRBasePanel(Args.Obj).Zoom := Value;
end;

{ property Read FontSize: Integer }

procedure TQRBasePanel_Read_FontSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRBasePanel(Args.Obj).FontSize;
end;

{ property Write FontSize(Value: Integer) }

procedure TQRBasePanel_Write_FontSize(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRBasePanel(Args.Obj).FontSize := Value;
end;

{ property Read Frame: TQRFrame }

procedure TQRBasePanel_Read_Frame(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRBasePanel(Args.Obj).Frame);
end;

{ property Write Frame(Value: TQRFrame) }

procedure TQRBasePanel_Write_Frame(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRBasePanel(Args.Obj).Frame := V2O(Value) as TQRFrame;
end;

{ TQRCustomBand }

{ constructor Create(AOwner: TComponent) }

procedure TQRCustomBand_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCustomBand.Create(V2O(Args.Values[0]) as TComponent));
end;

{ function AddPrintable(PrintableClass: TQRNewComponentClass): TQRPrintable; }

procedure TQRCustomBand_AddPrintable(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCustomBand(Args.Obj).AddPrintable(TQRNewComponentClass(V2C(Args.Values[0]))));
end;

{ function CanExpand(Value: Extended): Boolean; }

procedure TQRCustomBand_CanExpand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).CanExpand(Args.Values[0]);
end;

{ function ExpandBand(Value: Extended; var NewTop, OfsX: Extended): Boolean; }

procedure TQRCustomBand_ExpandBand(var Value: Variant; Args: TJvInterpreterArgs);
var
  NewTop: Extended;
  param2: Boolean;
begin
  NewTop := Args.Values[1];
  param2 := Args.Values[2];
  Value := TQRCustomBand(Args.Obj).ExpandBand(Args.Values[0], NewTop, param2);
  Args.Values[1] := NewTop;
  Args.Values[2] := param2;
end;

{ property Read BandType: TQRBandType }

procedure TQRCustomBand_Read_BandType(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).BandType;
end;

{ property Write BandType(Value: TQRBandType) }

procedure TQRCustomBand_Write_BandType(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).BandType := Value;
end;

{ property Read ChildBand: TQRChildBand }

procedure TQRCustomBand_Read_ChildBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCustomBand(Args.Obj).ChildBand);
end;

{ property Read ParentReport: TQuickRep }

procedure TQRCustomBand_Read_ParentReport(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCustomBand(Args.Obj).ParentReport);
end;

{ property Write ParentReport(Value: TQuickRep) }

procedure TQRCustomBand_Write_ParentReport(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).ParentReport := V2O(Value) as TQuickRep;
end;

{ property Read LinkBand: TQRCustomBand }

procedure TQRCustomBand_Read_LinkBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCustomBand(Args.Obj).LinkBand);
end;

{ property Write LinkBand(Value: TQRCustomBand) }

procedure TQRCustomBand_Write_LinkBand(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).LinkBand := V2O(Value) as TQRCustomBand;
end;

{ property Read AlignToBottom: Boolean }

procedure TQRCustomBand_Read_AlignToBottom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).AlignToBottom;
end;

{ property Write AlignToBottom(Value: Boolean) }

procedure TQRCustomBand_Write_AlignToBottom(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).AlignToBottom := Value;
end;

{ property Read Enabled: Boolean }

procedure TQRCustomBand_Read_Enabled(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).Enabled;
end;

{ property Write Enabled(Value: Boolean) }

procedure TQRCustomBand_Write_Enabled(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).Enabled := Value;
end;

{ property Read ForceNewColumn: Boolean }

procedure TQRCustomBand_Read_ForceNewColumn(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).ForceNewColumn;
end;

{ property Write ForceNewColumn(Value: Boolean) }

procedure TQRCustomBand_Write_ForceNewColumn(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).ForceNewColumn := Value;
end;

{ property Read ForceNewPage: Boolean }

procedure TQRCustomBand_Read_ForceNewPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).ForceNewPage;
end;

{ property Write ForceNewPage(Value: Boolean) }

procedure TQRCustomBand_Write_ForceNewPage(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).ForceNewPage := Value;
end;

{ property Read HasChild: Boolean }

procedure TQRCustomBand_Read_HasChild(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCustomBand(Args.Obj).HasChild;
end;

{ property Write HasChild(Value: Boolean) }

procedure TQRCustomBand_Write_HasChild(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).HasChild := Value;
end;

{ property Read Size: TQRBandSize }

procedure TQRCustomBand_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCustomBand(Args.Obj).Size);
end;

{ property Write Size(Value: TQRBandSize) }

procedure TQRCustomBand_Write_Size(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCustomBand(Args.Obj).Size := V2O(Value) as TQRBandSize;
end;

{ TQRBand }

{ TQRChildBand }

{ constructor Create(AOwner: TComponent) }

procedure TQRChildBand_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRChildBand.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ParentBand: TQRCustomBand }

procedure TQRChildBand_Read_ParentBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRChildBand(Args.Obj).ParentBand);
end;

{ property Write ParentBand(Value: TQRCustomBand) }

procedure TQRChildBand_Write_ParentBand(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRChildBand(Args.Obj).ParentBand := V2O(Value) as TQRCustomBand;
end;

{ TQRControllerBand }

{ constructor Create(AOwner: TComponent) }

procedure TQRControllerBand_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRControllerBand.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read PrintIfEmpty: Boolean }

procedure TQRControllerBand_Read_PrintIfEmpty(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRControllerBand(Args.Obj).PrintIfEmpty;
end;

{ property Write PrintIfEmpty(Value: Boolean) }

procedure TQRControllerBand_Write_PrintIfEmpty(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRControllerBand(Args.Obj).PrintIfEmpty := Value;
end;

{ property Read Master: TComponent }

procedure TQRControllerBand_Read_Master(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRControllerBand(Args.Obj).Master);
end;

{ property Write Master(Value: TComponent) }

procedure TQRControllerBand_Write_Master(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRControllerBand(Args.Obj).Master := V2O(Value) as TComponent;
end;

{ TQRSubDetailGroupBands }

{ constructor Create(AOwner: TQRSubDetail) }

procedure TQRSubDetailGroupBands_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetailGroupBands.Create(V2O(Args.Values[0]) as TQRSubDetail));
end;

{ property Read FooterBand: TQRCustomBand }

procedure TQRSubDetailGroupBands_Read_FooterBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetailGroupBands(Args.Obj).FooterBand);
end;

{ property Read HeaderBand: TQRCustomBand }

procedure TQRSubDetailGroupBands_Read_HeaderBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetailGroupBands(Args.Obj).HeaderBand);
end;

{ property Read HasFooter: Boolean }

procedure TQRSubDetailGroupBands_Read_HasFooter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRSubDetailGroupBands(Args.Obj).HasFooter;
end;

{ property Write HasFooter(Value: Boolean) }

procedure TQRSubDetailGroupBands_Write_HasFooter(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetailGroupBands(Args.Obj).HasFooter := Value;
end;

{ property Read HasHeader: Boolean }

procedure TQRSubDetailGroupBands_Read_HasHeader(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRSubDetailGroupBands(Args.Obj).HasHeader;
end;

{ property Write HasHeader(Value: Boolean) }

procedure TQRSubDetailGroupBands_Write_HasHeader(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetailGroupBands(Args.Obj).HasHeader := Value;
end;

{ TQRSubDetail }

{ constructor Create(AOwner: TComponent) }

procedure TQRSubDetail_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetail.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure AddNotifyClient(Value: TQRPrintable); }

procedure TQRSubDetail_AddNotifyClient(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetail(Args.Obj).AddNotifyClient(V2O(Args.Values[0]) as TQRPrintable);
end;

{ property Read Bands: TQRSubDetailGroupBands }

procedure TQRSubDetail_Read_Bands(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetail(Args.Obj).Bands);
end;

{ property Write Bands(Value: TQRSubDetailGroupBands) }

procedure TQRSubDetail_Write_Bands(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetail(Args.Obj).Bands := V2O(Value) as TQRSubDetailGroupBands;
end;

{ property Read DataSet: TDataSet }

procedure TQRSubDetail_Read_DataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetail(Args.Obj).DataSet);
end;

{ property Write DataSet(Value: TDataSet) }

procedure TQRSubDetail_Write_DataSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetail(Args.Obj).DataSet := V2O(Value) as TDataSet;
end;

{ property Read FooterBand: TQRCustomBand }

procedure TQRSubDetail_Read_FooterBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetail(Args.Obj).FooterBand);
end;

{ property Write FooterBand(Value: TQRCustomBand) }

procedure TQRSubDetail_Write_FooterBand(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetail(Args.Obj).FooterBand := V2O(Value) as TQRCustomBand;
end;

{ property Read HeaderBand: TQRCustomBand }

procedure TQRSubDetail_Read_HeaderBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRSubDetail(Args.Obj).HeaderBand);
end;

{ property Write HeaderBand(Value: TQRCustomBand) }

procedure TQRSubDetail_Write_HeaderBand(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetail(Args.Obj).HeaderBand := V2O(Value) as TQRCustomBand;
end;

{ property Read PrintBefore: Boolean }

procedure TQRSubDetail_Read_PrintBefore(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRSubDetail(Args.Obj).PrintBefore;
end;

{ property Write PrintBefore(Value: Boolean) }

procedure TQRSubDetail_Write_PrintBefore(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRSubDetail(Args.Obj).PrintBefore := Value;
end;

{ TQuickRepBands }

{ constructor Create(AOwner: TQuickRep) }

procedure TQuickRepBands_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands.Create(V2O(Args.Values[0]) as TQuickRep));
end;

{ property Read TitleBand: TQRCustomBand }

procedure TQuickRepBands_Read_TitleBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).TitleBand);
end;

{ property Read PageHeaderBand: TQRCustomBand }

procedure TQuickRepBands_Read_PageHeaderBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).PageHeaderBand);
end;

{ property Read ColumnHeaderBand: TQRCustomBand }

procedure TQuickRepBands_Read_ColumnHeaderBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).ColumnHeaderBand);
end;

{ property Read DetailBand: TQRCustomBand }

procedure TQuickRepBands_Read_DetailBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).DetailBand);
end;

{ property Read ColumnFooterBand: TQRCustomBand }

procedure TQuickRepBands_Read_ColumnFooterBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).ColumnFooterBand);
end;

{ property Read PageFooterBand: TQRCustomBand }

procedure TQuickRepBands_Read_PageFooterBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).PageFooterBand);
end;

{ property Read SummaryBand: TQRCustomBand }

procedure TQuickRepBands_Read_SummaryBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRepBands(Args.Obj).SummaryBand);
end;

{ property Read HasTitle: Boolean }

procedure TQuickRepBands_Read_HasTitle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRepBands(Args.Obj).HasTitle;
end;

{ property Write HasTitle(Value: Boolean) }

procedure TQuickRepBands_Write_HasTitle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRepBands(Args.Obj).HasTitle := Value;
end;

{ property Read HasPageHeader: Boolean }

procedure TQuickRepBands_Read_HasPageHeader(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRepBands(Args.Obj).HasPageHeader;
end;

{ property Write HasPageHeader(Value: Boolean) }

procedure TQuickRepBands_Write_HasPageHeader(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRepBands(Args.Obj).HasPageHeader := Value;
end;

{ property Read HasColumnHeader: Boolean }

procedure TQuickRepBands_Read_HasColumnHeader(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRepBands(Args.Obj).HasColumnHeader;
end;

{ property Write HasColumnHeader(Value: Boolean) }

procedure TQuickRepBands_Write_HasColumnHeader(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRepBands(Args.Obj).HasColumnHeader := Value;
end;

{ property Read HasDetail: Boolean }

procedure TQuickRepBands_Read_HasDetail(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRepBands(Args.Obj).HasDetail;
end;

{ property Write HasDetail(Value: Boolean) }

procedure TQuickRepBands_Write_HasDetail(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRepBands(Args.Obj).HasDetail := Value;
end;

{ property Read HasPageFooter: Boolean }

procedure TQuickRepBands_Read_HasPageFooter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRepBands(Args.Obj).HasPageFooter;
end;

{ property Write HasPageFooter(Value: Boolean) }

procedure TQuickRepBands_Write_HasPageFooter(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRepBands(Args.Obj).HasPageFooter := Value;
end;

{ property Read HasSummary: Boolean }

procedure TQuickRepBands_Read_HasSummary(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRepBands(Args.Obj).HasSummary;
end;

{ property Write HasSummary(Value: Boolean) }

procedure TQuickRepBands_Write_HasSummary(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRepBands(Args.Obj).HasSummary := Value;
end;

{ TQuickRepPrinterSettings }

{ TQuickRep }

{ constructor Create(AOwner: TComponent) }

procedure TQuickRep_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep.Create(V2O(Args.Values[0]) as TComponent));
end;

{ constructor CreateNew(AOwner: TComponent) }

procedure TQuickRep_CreateNew(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep.CreateNew(V2O(Args.Values[0]) as TComponent));
end;

{ function CreateBand(BandType: TQRBandType): TQRBand; }

procedure TQuickRep_CreateBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).CreateBand(Args.Values[0]));
end;

{ function TextHeight(aFont: TFont; aText: string): Integer; }

procedure TQuickRep_TextHeight(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).TextHeight(V2O(Args.Values[0]) as TFont, Args.Values[1]);
end;

{ function TextWidth(aFont: TFont; aText: string): Integer; }

procedure TQuickRep_TextWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).TextWidth(V2O(Args.Values[0]) as TFont, Args.Values[1]);
end;

{ procedure AddBand(aBand: TQRCustomBand); }

procedure TQuickRep_AddBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).AddBand(V2O(Args.Values[0]) as TQRCustomBand);
end;

{ procedure AddNotifyClient(Value: TQRPrintable); }

procedure TQuickRep_AddNotifyClient(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).AddNotifyClient(V2O(Args.Values[0]) as TQRPrintable);
end;

(*
{ procedure ExportToFilter(AFilter: TQRExportFilter); }
procedure TQuickRep_ExportToFilter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).ExportToFilter(Args.Values[0]);
end;
*)

{ procedure EndPage; }

procedure TQuickRep_EndPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).EndPage;
end;

{ procedure NewColumn; }

procedure TQuickRep_NewColumn(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).NewColumn;
end;

{ procedure NewPage; }

procedure TQuickRep_NewPage(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).NewPage;
end;

{ procedure Paint; }

procedure TQuickRep_Paint(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Paint;
end;

{ procedure Print; }

procedure TQuickRep_Print(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Print;
end;

{ procedure PrintBackground; }

procedure TQuickRep_PrintBackground(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).PrintBackground;
end;

{ procedure PrinterSetup; }

procedure TQuickRep_PrinterSetup(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).PrinterSetup;
end;

{ procedure Prepare; }

procedure TQuickRep_Prepare(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Prepare;
end;

{ procedure Preview; }

procedure TQuickRep_Preview(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Preview;
end;

{ procedure ResetPageFooterSize; }

procedure TQuickRep_ResetPageFooterSize(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).ResetPageFooterSize;
end;

{ procedure RemoveBand(aBand: TQRCustomBand); }

procedure TQuickRep_RemoveBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).RemoveBand(V2O(Args.Values[0]) as TQRCustomBand);
end;

{ procedure SetBandValues; }

procedure TQuickRep_SetBandValues(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).SetBandValues;
end;

{ procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); }

procedure TQuickRep_SetBounds(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).SetBounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ property Read AllDataSets: TList }

procedure TQuickRep_Read_AllDataSets(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).AllDataSets);
end;

{ property Write AllDataSets(Value: TList) }

procedure TQuickRep_Write_AllDataSets(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).AllDataSets := V2O(Value) as TList;
end;

{ property Read Available: Boolean }

procedure TQuickRep_Read_Available(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).Available;
end;

{ property Read BandList: TList }

procedure TQuickRep_Read_BandList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).BandList);
end;

{ property Read ColumnTopPosition: Integer }

procedure TQuickRep_Read_ColumnTopPosition(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).ColumnTopPosition;
end;

{ property Write ColumnTopPosition(Value: Integer) }

procedure TQuickRep_Write_ColumnTopPosition(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).ColumnTopPosition := Value;
end;

{ property Read CurrentColumn: Integer }

procedure TQuickRep_Read_CurrentColumn(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).CurrentColumn;
end;

{ property Read CurrentX: Integer }

procedure TQuickRep_Read_CurrentX(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).CurrentX;
end;

{ property Write CurrentX(Value: Integer) }

procedure TQuickRep_Write_CurrentX(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).CurrentX := Value;
end;

{ property Read CurrentY: Integer }

procedure TQuickRep_Read_CurrentY(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).CurrentY;
end;

{ property Write CurrentY(Value: Integer) }

procedure TQuickRep_Write_CurrentY(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).CurrentY := Value;
end;

(*
{ property Read ExportFilter: TQRExportFilter }
procedure TQuickRep_Read_ExportFilter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).ExportFilter;
end;

{ property Write ExportFilter(Value: TQRExportFilter) }
procedure TQuickRep_Write_ExportFilter(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).ExportFilter := Value;
end;
*)

{ property Read Exporting: Boolean }

procedure TQuickRep_Read_Exporting(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).Exporting;
end;

{ property Read FinalPass: Boolean }

procedure TQuickRep_Read_FinalPass(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).FinalPass;
end;

{ property Read HideBands: Boolean }

procedure TQuickRep_Read_HideBands(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).HideBands;
end;

{ property Write HideBands(Value: Boolean) }

procedure TQuickRep_Write_HideBands(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).HideBands := Value;
end;

{ property Read PageNumber: Integer }

procedure TQuickRep_Read_PageNumber(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).PageNumber;
end;

{ property Read Printer: TQRPrinter }

procedure TQuickRep_Read_Printer(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).Printer);
end;

{ property Read QRPrinter: TQRPrinter }

procedure TQuickRep_Read_QRPrinter(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).QRPrinter);
end;

{ property Write QRPrinter(Value: TQRPrinter) }

procedure TQuickRep_Write_QRPrinter(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).QRPrinter := V2O(Value) as TQRPrinter;
end;

{ property Read RotateBands: Integer }

procedure TQuickRep_Read_RotateBands(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).RotateBands;
end;

{ property Write RotateBands(Value: Integer) }

procedure TQuickRep_Write_RotateBands(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).RotateBands := Value;
end;

{ property Read State: TQRState }

procedure TQuickRep_Read_State(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).State;
end;

{ property Write State(Value: TQRState) }

procedure TQuickRep_Write_State(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).State := Value;
end;

{ property Read Bands: TQuickRepBands }

procedure TQuickRep_Read_Bands(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).Bands);
end;

{ property Write Bands(Value: TQuickRepBands) }

procedure TQuickRep_Write_Bands(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Bands := V2O(Value) as TQuickRepBands;
end;

{ property Read DataSet: TDataSet }

procedure TQuickRep_Read_DataSet(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).DataSet);
end;

{ property Write DataSet(Value: TDataSet) }

procedure TQuickRep_Write_DataSet(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).DataSet := V2O(Value) as TDataSet;
end;

{ property Read Description: TStrings }

procedure TQuickRep_Read_Description(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).Description);
end;

{ property Write Description(Value: TStrings) }

procedure TQuickRep_Write_Description(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Description := V2O(Value) as TStrings;
end;

{ property Read Options: TQuickReportOptions }

procedure TQuickRep_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TQuickRep(Args.Obj).Options));
end;

{ property Write Options(Value: TQuickReportOptions) }

procedure TQuickRep_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Options := TQuickReportOptions(Byte(V2S(Value)));
end;

{ property Read Page: TQRPage }

procedure TQuickRep_Read_Page(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).Page);
end;

{ property Write Page(Value: TQRPage) }

procedure TQuickRep_Write_Page(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Page := V2O(Value) as TQRPage;
end;

{ property Read PrintIfEmpty: Boolean }

procedure TQuickRep_Read_PrintIfEmpty(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).PrintIfEmpty;
end;

{ property Write PrintIfEmpty(Value: Boolean) }

procedure TQuickRep_Write_PrintIfEmpty(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).PrintIfEmpty := Value;
end;

{ property Read PrinterSettings: TQuickRepPrinterSettings }

procedure TQuickRep_Read_PrinterSettings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQuickRep(Args.Obj).PrinterSettings);
end;

{ property Write PrinterSettings(Value: TQuickRepPrinterSettings) }

procedure TQuickRep_Write_PrinterSettings(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).PrinterSettings := V2O(Value) as TQuickRepPrinterSettings;
end;

{ property Read ReportTitle: string }

procedure TQuickRep_Read_ReportTitle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).ReportTitle;
end;

{ property Write ReportTitle(Value: string) }

procedure TQuickRep_Write_ReportTitle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).ReportTitle := Value;
end;

{ property Read ShowProgress: Boolean }

procedure TQuickRep_Read_ShowProgress(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).ShowProgress;
end;

{ property Write ShowProgress(Value: Boolean) }

procedure TQuickRep_Write_ShowProgress(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).ShowProgress := Value;
end;

{ property Read SnapToGrid: Boolean }

procedure TQuickRep_Read_SnapToGrid(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).SnapToGrid;
end;

{ property Write SnapToGrid(Value: Boolean) }

procedure TQuickRep_Write_SnapToGrid(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).SnapToGrid := Value;
end;

{ property Read Units: TQRUnit }

procedure TQuickRep_Read_Units(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQuickRep(Args.Obj).Units;
end;

{ property Write Units(Value: TQRUnit) }

procedure TQuickRep_Write_Units(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQuickRep(Args.Obj).Units := Value;
end;

{ TQRGroup }

{ constructor Create(AOwner: TComponent) }

procedure TQRGroup_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRGroup.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Expression: string }

procedure TQRGroup_Read_Expression(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRGroup(Args.Obj).Expression;
end;

{ property Write Expression(Value: string) }

procedure TQRGroup_Write_Expression(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRGroup(Args.Obj).Expression := Value;
end;

{ property Read FooterBand: TQRBand }

procedure TQRGroup_Read_FooterBand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRGroup(Args.Obj).FooterBand);
end;

{ property Write FooterBand(Value: TQRBand) }

procedure TQRGroup_Write_FooterBand(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRGroup(Args.Obj).FooterBand := V2O(Value) as TQRBand;
end;

{ property Read Master: TComponent }

procedure TQRGroup_Read_Master(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRGroup(Args.Obj).Master);
end;

{ property Write Master(Value: TComponent) }

procedure TQRGroup_Write_Master(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRGroup(Args.Obj).Master := V2O(Value) as TComponent;
end;

{ TQRPrintableSize }

{ constructor Create(AParent: TQRPrintable) }

procedure TQRPrintableSize_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRPrintableSize.Create(V2O(Args.Values[0]) as TQRPrintable));
end;

{ property Read Height: Extended }

procedure TQRPrintableSize_Read_Height(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPrintableSize(Args.Obj).Height;
end;

{ property Write Height(Value: Extended) }

procedure TQRPrintableSize_Write_Height(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintableSize(Args.Obj).Height := Value;
end;

{ property Read Left: Extended }

procedure TQRPrintableSize_Read_Left(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPrintableSize(Args.Obj).Left;
end;

{ property Write Left(Value: Extended) }

procedure TQRPrintableSize_Write_Left(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintableSize(Args.Obj).Left := Value;
end;

{ property Read Top: Extended }

procedure TQRPrintableSize_Read_Top(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPrintableSize(Args.Obj).Top;
end;

{ property Write Top(Value: Extended) }

procedure TQRPrintableSize_Write_Top(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintableSize(Args.Obj).Top := Value;
end;

{ property Read Width: Extended }

procedure TQRPrintableSize_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPrintableSize(Args.Obj).Width;
end;

{ property Write Width(Value: Extended) }

procedure TQRPrintableSize_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintableSize(Args.Obj).Width := Value;
end;

{ TQRPrintable }

{ constructor Create(AOwner: TComponent) }

procedure TQRPrintable_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRPrintable.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read ParentReport: TQuickRep }

procedure TQRPrintable_Read_ParentReport(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRPrintable(Args.Obj).ParentReport);
end;

{ property Write ParentReport(Value: TQuickRep) }

procedure TQRPrintable_Write_ParentReport(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintable(Args.Obj).ParentReport := V2O(Value) as TQuickRep;
end;

{ property Read Zoom: Integer }

procedure TQRPrintable_Read_Zoom(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRPrintable(Args.Obj).Zoom;
end;

{ property Write Zoom(Value: Integer) }

procedure TQRPrintable_Write_Zoom(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintable(Args.Obj).Zoom := Value;
end;

{ property Read Frame: TQRFrame }

procedure TQRPrintable_Read_Frame(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRPrintable(Args.Obj).Frame);
end;

{ property Write Frame(Value: TQRFrame) }

procedure TQRPrintable_Write_Frame(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintable(Args.Obj).Frame := V2O(Value) as TQRFrame;
end;

{ property Read Size: TQRPrintableSize }

procedure TQRPrintable_Read_Size(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRPrintable(Args.Obj).Size);
end;

{ property Write Size(Value: TQRPrintableSize) }

procedure TQRPrintable_Write_Size(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRPrintable(Args.Obj).Size := V2O(Value) as TQRPrintableSize;
end;

{ TQRCompositeReport }

{ constructor Create(AOwner: TComponent) }

procedure TQRCompositeReport_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCompositeReport.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Prepare; }

procedure TQRCompositeReport_Prepare(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).Prepare;
end;

{ procedure Preview; }

procedure TQRCompositeReport_Preview(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).Preview;
end;

{ procedure Print; }

procedure TQRCompositeReport_Print(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).Print;
end;

{ property Read Reports: TList }

procedure TQRCompositeReport_Read_Reports(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCompositeReport(Args.Obj).Reports);
end;

{ property Write Reports(Value: TList) }

procedure TQRCompositeReport_Write_Reports(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).Reports := V2O(Value) as TList;
end;

{ property Read Options: TQuickReportOptions }

procedure TQRCompositeReport_Read_Options(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Byte(TQRCompositeReport(Args.Obj).Options));
end;

{ property Write Options(Value: TQuickReportOptions) }

procedure TQRCompositeReport_Write_Options(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).Options := TQuickReportOptions(Byte(Value));
end;

{ property Read PrinterSettings: TQuickRepPrinterSettings }

procedure TQRCompositeReport_Read_PrinterSettings(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TQRCompositeReport(Args.Obj).PrinterSettings);
end;

{ property Write PrinterSettings(Value: TQuickRepPrinterSettings) }

procedure TQRCompositeReport_Write_PrinterSettings(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).PrinterSettings := V2O(Value) as TQRCompositePrinterSettings;
end;

{ property Read ReportTitle: string }

procedure TQRCompositeReport_Read_ReportTitle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TQRCompositeReport(Args.Obj).ReportTitle;
end;

{ property Write ReportTitle(Value: string) }

procedure TQRCompositeReport_Write_ReportTitle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TQRCompositeReport(Args.Obj).ReportTitle := Value;
end;

type
  { TQROnNeedDataEvent = procedure (Sender : TObject; var MoreData : Boolean) of object;
  TQRNotifyOperationEvent = procedure (Sender : TObject; Operation : TQRNotifyOperation) of object;
  TQRBandBeforePrintEvent = procedure (Sender : TQRCustomBand; var PrintBand : Boolean) of object;
  TQRBandAfterPrintEvent = procedure (Sender : TQRCustomBand; BandPrinted : Boolean) of object;
  TQRNotifyEvent = procedure (Sender : TQuickRep) of object;
  TQRReportBeforePrintEvent = procedure (Sender : TQuickRep; var PrintReport : Boolean) of object;
  TQRFilterEvent = procedure (var PrintRecord : Boolean) of object; }

  TJvInterpreterQuickrptEvent = class(TJvInterpreterEvent)
  private
    procedure QROnNeedDataEvent(Sender: TObject; var MoreData: Boolean);
    procedure QRNotifyOperationEvent(Sender: TObject; Operation: TQRNotifyOperation);
    procedure QRBandBeforePrintEvent(Sender: TQRCustomBand; var PrintBand: Boolean);
    procedure QRBandAfterPrintEvent(Sender: TQRCustomBand; BandPrinted: Boolean);
    procedure QRNotifyEvent(Sender: TQuickRep);
    procedure QRReportBeforePrintEvent(Sender: TQuickRep; var PrintReport: Boolean);
    procedure QRFilterEvent(var PrintRecord: Boolean);
  end;

procedure TJvInterpreterQuickrptEvent.QROnNeedDataEvent(Sender: TObject; var MoreData: Boolean);
begin
  CallFunction(nil, [O2V(Sender), MoreData]);
  MoreData := Args.Values[1];
end;

procedure TJvInterpreterQuickrptEvent.QRNotifyOperationEvent(Sender: TObject; Operation: TQRNotifyOperation);
begin
  CallFunction(nil, [O2V(Sender), Operation]);
end;

procedure TJvInterpreterQuickrptEvent.QRBandBeforePrintEvent(Sender: TQRCustomBand; var PrintBand: Boolean);
begin
  CallFunction(nil, [O2V(Sender), PrintBand]);
  PrintBand := Args.Values[1];
end;

procedure TJvInterpreterQuickrptEvent.QRBandAfterPrintEvent(Sender: TQRCustomBand; BandPrinted: Boolean);
begin
  CallFunction(nil, [O2V(Sender), BandPrinted]);
end;

procedure TJvInterpreterQuickrptEvent.QRNotifyEvent(Sender: TQuickRep);
begin
  CallFunction(nil, [O2V(Sender)]);
end;

procedure TJvInterpreterQuickrptEvent.QRReportBeforePrintEvent(Sender: TQuickRep; var PrintReport: Boolean);
begin
  CallFunction(nil, [O2V(Sender), PrintReport]);
  PrintReport := Args.Values[1];
end;

procedure TJvInterpreterQuickrptEvent.QRFilterEvent(var PrintRecord: Boolean);
begin
  CallFunction(nil, [PrintRecord]);
  PrintRecord := Args.Values[0];
end;

{ Delphi 3, 4 and CBuilder 3 }

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cQuickrpt = 'Quickrpt';
begin
  with JvInterpreterAdapter do
  begin
    { TQRNotifyOperation }
    AddConst(cQuickrpt, 'qrMasterDataAdvance', Ord(qrMasterDataAdvance));
    AddConst(cQuickrpt, 'qrBandPrinted', Ord(qrBandPrinted));
    AddConst(cQuickrpt, 'qrBandSizeChange', Ord(qrBandSizeChange));
    { TQRController }
    AddClass(cQuickrpt, TQRController, 'TQRController');
    AddGet(TQRController, 'Create', TQRController_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRController, 'AddNotifyClient', TQRController_AddNotifyClient, 1, [varEmpty], varEmpty);
    AddGet(TQRController, 'DataSet', TQRController_Read_DataSet, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'DataSet', TQRController_Write_DataSet, 0, [varEmpty]);
    AddGet(TQRController, 'DetailNumber', TQRController_Read_DetailNumber, 0, [varEmpty], varEmpty);
    AddGet(TQRController, 'Detail', TQRController_Read_Detail, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'Detail', TQRController_Write_Detail, 0, [varEmpty]);
    AddGet(TQRController, 'Footer', TQRController_Read_Footer, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'Footer', TQRController_Write_Footer, 0, [varEmpty]);
    AddGet(TQRController, 'Header', TQRController_Read_Header, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'Header', TQRController_Write_Header, 0, [varEmpty]);
    AddGet(TQRController, 'Master', TQRController_Read_Master, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'Master', TQRController_Write_Master, 0, [varEmpty]);
    AddGet(TQRController, 'ParentReport', TQRController_Read_ParentReport, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'ParentReport', TQRController_Write_ParentReport, 0, [varEmpty]);
    AddGet(TQRController, 'PrintBefore', TQRController_Read_PrintBefore, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'PrintBefore', TQRController_Write_PrintBefore, 0, [varEmpty]);
    AddGet(TQRController, 'PrintIfEmpty', TQRController_Read_PrintIfEmpty, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'PrintIfEmpty', TQRController_Write_PrintIfEmpty, 0, [varEmpty]);
    AddGet(TQRController, 'SelfCheck', TQRController_Read_SelfCheck, 0, [varEmpty], varEmpty);
    AddSet(TQRController, 'SelfCheck', TQRController_Write_SelfCheck, 0, [varEmpty]);
    { TQRFrame }
    AddClass(cQuickrpt, TQRFrame, 'TQRFrame');
    AddGet(TQRFrame, 'Create', TQRFrame_Create, 0, [varEmpty], varEmpty);
    AddGet(TQRFrame, 'AnyFrame', TQRFrame_AnyFrame, 0, [varEmpty], varEmpty);
    AddGet(TQRFrame, 'Parent', TQRFrame_Read_Parent, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'Parent', TQRFrame_Write_Parent, 0, [varEmpty]);
    AddGet(TQRFrame, 'Color', TQRFrame_Read_Color, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'Color', TQRFrame_Write_Color, 0, [varEmpty]);
    AddGet(TQRFrame, 'DrawTop', TQRFrame_Read_DrawTop, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'DrawTop', TQRFrame_Write_DrawTop, 0, [varEmpty]);
    AddGet(TQRFrame, 'DrawBottom', TQRFrame_Read_DrawBottom, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'DrawBottom', TQRFrame_Write_DrawBottom, 0, [varEmpty]);
    AddGet(TQRFrame, 'DrawLeft', TQRFrame_Read_DrawLeft, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'DrawLeft', TQRFrame_Write_DrawLeft, 0, [varEmpty]);
    AddGet(TQRFrame, 'DrawRight', TQRFrame_Read_DrawRight, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'DrawRight', TQRFrame_Write_DrawRight, 0, [varEmpty]);
    AddGet(TQRFrame, 'Style', TQRFrame_Read_Style, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'Style', TQRFrame_Write_Style, 0, [varEmpty]);
    AddGet(TQRFrame, 'Width', TQRFrame_Read_Width, 0, [varEmpty], varEmpty);
    AddSet(TQRFrame, 'Width', TQRFrame_Write_Width, 0, [varEmpty]);
    { TQRUnit }
    AddConst(cQuickrpt, 'MM', Ord(MM));
    AddConst(cQuickrpt, 'Inches', Ord(Inches));
    AddConst(cQuickrpt, 'Pixels', Ord(Pixels));
    AddConst(cQuickrpt, 'Characters', Ord(Characters));
    AddConst(cQuickrpt, 'Native', Ord(Native));
    { TQRUnitBase }
    AddClass(cQuickrpt, TQRUnitBase, 'TQRUnitBase');
    AddGet(TQRUnitBase, 'Create', TQRUnitBase_Create, 0, [varEmpty], varEmpty);
    AddGet(TQRUnitBase, 'ParentReport', TQRUnitBase_Read_ParentReport, 0, [varEmpty], varEmpty);
    AddSet(TQRUnitBase, 'ParentReport', TQRUnitBase_Write_ParentReport, 0, [varEmpty]);
    AddGet(TQRUnitBase, 'ParentUpdating', TQRUnitBase_Read_ParentUpdating, 0, [varEmpty], varEmpty);
    AddSet(TQRUnitBase, 'ParentUpdating', TQRUnitBase_Write_ParentUpdating, 0, [varEmpty]);
    AddGet(TQRUnitBase, 'Resolution', TQRUnitBase_Read_Resolution, 0, [varEmpty], varEmpty);
    AddGet(TQRUnitBase, 'Units', TQRUnitBase_Read_Units, 0, [varEmpty], varEmpty);
    AddSet(TQRUnitBase, 'Units', TQRUnitBase_Write_Units, 0, [varEmpty]);
    AddGet(TQRUnitBase, 'Zoom', TQRUnitBase_Read_Zoom, 0, [varEmpty], varEmpty);
    AddSet(TQRUnitBase, 'Zoom', TQRUnitBase_Write_Zoom, 0, [varEmpty]);
    { TQRBandSize }
    AddClass(cQuickrpt, TQRBandSize, 'TQRBandSize');
    AddGet(TQRBandSize, 'Create', TQRBandSize_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRBandSize, 'Length', TQRBandSize_Read_Length, 0, [varEmpty], varEmpty);
    AddSet(TQRBandSize, 'Length', TQRBandSize_Write_Length, 0, [varEmpty]);
    AddGet(TQRBandSize, 'Height', TQRBandSize_Read_Height, 0, [varEmpty], varEmpty);
    AddSet(TQRBandSize, 'Height', TQRBandSize_Write_Height, 0, [varEmpty]);
    AddGet(TQRBandSize, 'Width', TQRBandSize_Read_Width, 0, [varEmpty], varEmpty);
    AddSet(TQRBandSize, 'Width', TQRBandSize_Write_Width, 0, [varEmpty]);
    { TQRPage }
    AddClass(cQuickrpt, TQRPage, 'TQRPage');
    AddGet(TQRPage, 'Create', TQRPage_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRPage, 'BottomMargin', TQRPage_Read_BottomMargin, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'BottomMargin', TQRPage_Write_BottomMargin, 0, [varEmpty]);
    AddGet(TQRPage, 'ColumnSpace', TQRPage_Read_ColumnSpace, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'ColumnSpace', TQRPage_Write_ColumnSpace, 0, [varEmpty]);
    AddGet(TQRPage, 'Columns', TQRPage_Read_Columns, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'Columns', TQRPage_Write_Columns, 0, [varEmpty]);
    AddGet(TQRPage, 'LeftMargin', TQRPage_Read_LeftMargin, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'LeftMargin', TQRPage_Write_LeftMargin, 0, [varEmpty]);
    AddGet(TQRPage, 'Length', TQRPage_Read_Length, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'Length', TQRPage_Write_Length, 0, [varEmpty]);
    AddGet(TQRPage, 'Orientation', TQRPage_Read_Orientation, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'Orientation', TQRPage_Write_Orientation, 0, [varEmpty]);
    AddGet(TQRPage, 'PaperSize', TQRPage_Read_PaperSize, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'PaperSize', TQRPage_Write_PaperSize, 0, [varEmpty]);
    AddGet(TQRPage, 'RightMargin', TQRPage_Read_RightMargin, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'RightMargin', TQRPage_Write_RightMargin, 0, [varEmpty]);
    AddGet(TQRPage, 'Ruler', TQRPage_Read_Ruler, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'Ruler', TQRPage_Write_Ruler, 0, [varEmpty]);
    AddGet(TQRPage, 'TopMargin', TQRPage_Read_TopMargin, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'TopMargin', TQRPage_Write_TopMargin, 0, [varEmpty]);
    AddGet(TQRPage, 'Width', TQRPage_Read_Width, 0, [varEmpty], varEmpty);
    AddSet(TQRPage, 'Width', TQRPage_Write_Width, 0, [varEmpty]);
    { TQRBasePanel }
    AddClass(cQuickrpt, TQRBasePanel, 'TQRBasePanel');
    AddGet(TQRBasePanel, 'Create', TQRBasePanel_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRBasePanel, 'Zoom', TQRBasePanel_Read_Zoom, 0, [varEmpty], varEmpty);
    AddSet(TQRBasePanel, 'Zoom', TQRBasePanel_Write_Zoom, 0, [varEmpty]);
    AddGet(TQRBasePanel, 'FontSize', TQRBasePanel_Read_FontSize, 0, [varEmpty], varEmpty);
    AddSet(TQRBasePanel, 'FontSize', TQRBasePanel_Write_FontSize, 0, [varEmpty]);
    AddGet(TQRBasePanel, 'Frame', TQRBasePanel_Read_Frame, 0, [varEmpty], varEmpty);
    AddSet(TQRBasePanel, 'Frame', TQRBasePanel_Write_Frame, 0, [varEmpty]);
    { TQRCustomBand }
    AddClass(cQuickrpt, TQRCustomBand, 'TQRCustomBand');
    AddGet(TQRCustomBand, 'Create', TQRCustomBand_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRCustomBand, 'AddPrintable', TQRCustomBand_AddPrintable, 1, [varEmpty], varEmpty);
    AddGet(TQRCustomBand, 'CanExpand', TQRCustomBand_CanExpand, 1, [varEmpty], varEmpty);
    AddGet(TQRCustomBand, 'ExpandBand', TQRCustomBand_ExpandBand, 3, [varEmpty, varByRef, varByRef], varEmpty);
    AddGet(TQRCustomBand, 'BandType', TQRCustomBand_Read_BandType, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'BandType', TQRCustomBand_Write_BandType, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'ChildBand', TQRCustomBand_Read_ChildBand, 0, [varEmpty], varEmpty);
    AddGet(TQRCustomBand, 'ParentReport', TQRCustomBand_Read_ParentReport, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'ParentReport', TQRCustomBand_Write_ParentReport, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'LinkBand', TQRCustomBand_Read_LinkBand, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'LinkBand', TQRCustomBand_Write_LinkBand, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'AlignToBottom', TQRCustomBand_Read_AlignToBottom, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'AlignToBottom', TQRCustomBand_Write_AlignToBottom, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'Enabled', TQRCustomBand_Read_Enabled, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'Enabled', TQRCustomBand_Write_Enabled, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'ForceNewColumn', TQRCustomBand_Read_ForceNewColumn, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'ForceNewColumn', TQRCustomBand_Write_ForceNewColumn, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'ForceNewPage', TQRCustomBand_Read_ForceNewPage, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'ForceNewPage', TQRCustomBand_Write_ForceNewPage, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'HasChild', TQRCustomBand_Read_HasChild, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'HasChild', TQRCustomBand_Write_HasChild, 0, [varEmpty]);
    AddGet(TQRCustomBand, 'Size', TQRCustomBand_Read_Size, 0, [varEmpty], varEmpty);
    AddSet(TQRCustomBand, 'Size', TQRCustomBand_Write_Size, 0, [varEmpty]);
    { TQRBand }
    AddClass(cQuickrpt, TQRBand, 'TQRBand');
    { TQRChildBand }
    AddClass(cQuickrpt, TQRChildBand, 'TQRChildBand');
    AddGet(TQRChildBand, 'Create', TQRChildBand_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRChildBand, 'ParentBand', TQRChildBand_Read_ParentBand, 0, [varEmpty], varEmpty);
    AddSet(TQRChildBand, 'ParentBand', TQRChildBand_Write_ParentBand, 0, [varEmpty]);
    { TQRControllerBand }
    AddClass(cQuickrpt, TQRControllerBand, 'TQRControllerBand');
    AddGet(TQRControllerBand, 'Create', TQRControllerBand_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRControllerBand, 'PrintIfEmpty', TQRControllerBand_Read_PrintIfEmpty, 0, [varEmpty], varEmpty);
    AddSet(TQRControllerBand, 'PrintIfEmpty', TQRControllerBand_Write_PrintIfEmpty, 0, [varEmpty]);
    AddGet(TQRControllerBand, 'Master', TQRControllerBand_Read_Master, 0, [varEmpty], varEmpty);
    AddSet(TQRControllerBand, 'Master', TQRControllerBand_Write_Master, 0, [varEmpty]);
    { TQRSubDetailGroupBands }
    AddClass(cQuickrpt, TQRSubDetailGroupBands, 'TQRSubDetailGroupBands');
    AddGet(TQRSubDetailGroupBands, 'Create', TQRSubDetailGroupBands_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRSubDetailGroupBands, 'FooterBand', TQRSubDetailGroupBands_Read_FooterBand, 0, [varEmpty], varEmpty);
    AddGet(TQRSubDetailGroupBands, 'HeaderBand', TQRSubDetailGroupBands_Read_HeaderBand, 0, [varEmpty], varEmpty);
    AddGet(TQRSubDetailGroupBands, 'HasFooter', TQRSubDetailGroupBands_Read_HasFooter, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetailGroupBands, 'HasFooter', TQRSubDetailGroupBands_Write_HasFooter, 0, [varEmpty]);
    AddGet(TQRSubDetailGroupBands, 'HasHeader', TQRSubDetailGroupBands_Read_HasHeader, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetailGroupBands, 'HasHeader', TQRSubDetailGroupBands_Write_HasHeader, 0, [varEmpty]);
    { TQRSubDetail }
    AddClass(cQuickrpt, TQRSubDetail, 'TQRSubDetail');
    AddGet(TQRSubDetail, 'Create', TQRSubDetail_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRSubDetail, 'AddNotifyClient', TQRSubDetail_AddNotifyClient, 1, [varEmpty], varEmpty);
    AddGet(TQRSubDetail, 'Bands', TQRSubDetail_Read_Bands, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetail, 'Bands', TQRSubDetail_Write_Bands, 0, [varEmpty]);
    AddGet(TQRSubDetail, 'DataSet', TQRSubDetail_Read_DataSet, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetail, 'DataSet', TQRSubDetail_Write_DataSet, 0, [varEmpty]);
    AddGet(TQRSubDetail, 'FooterBand', TQRSubDetail_Read_FooterBand, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetail, 'FooterBand', TQRSubDetail_Write_FooterBand, 0, [varEmpty]);
    AddGet(TQRSubDetail, 'HeaderBand', TQRSubDetail_Read_HeaderBand, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetail, 'HeaderBand', TQRSubDetail_Write_HeaderBand, 0, [varEmpty]);
    AddGet(TQRSubDetail, 'PrintBefore', TQRSubDetail_Read_PrintBefore, 0, [varEmpty], varEmpty);
    AddSet(TQRSubDetail, 'PrintBefore', TQRSubDetail_Write_PrintBefore, 0, [varEmpty]);
    { TQuickRepBands }
    AddClass(cQuickrpt, TQuickRepBands, 'TQuickRepBands');
    AddGet(TQuickRepBands, 'Create', TQuickRepBands_Create, 1, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'TitleBand', TQuickRepBands_Read_TitleBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'PageHeaderBand', TQuickRepBands_Read_PageHeaderBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'ColumnHeaderBand', TQuickRepBands_Read_ColumnHeaderBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'DetailBand', TQuickRepBands_Read_DetailBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'ColumnFooterBand', TQuickRepBands_Read_ColumnFooterBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'PageFooterBand', TQuickRepBands_Read_PageFooterBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'SummaryBand', TQuickRepBands_Read_SummaryBand, 0, [varEmpty], varEmpty);
    AddGet(TQuickRepBands, 'HasTitle', TQuickRepBands_Read_HasTitle, 0, [varEmpty], varEmpty);
    AddSet(TQuickRepBands, 'HasTitle', TQuickRepBands_Write_HasTitle, 0, [varEmpty]);
    AddGet(TQuickRepBands, 'HasPageHeader', TQuickRepBands_Read_HasPageHeader, 0, [varEmpty], varEmpty);
    AddSet(TQuickRepBands, 'HasPageHeader', TQuickRepBands_Write_HasPageHeader, 0, [varEmpty]);
    AddGet(TQuickRepBands, 'HasColumnHeader', TQuickRepBands_Read_HasColumnHeader, 0, [varEmpty], varEmpty);
    AddSet(TQuickRepBands, 'HasColumnHeader', TQuickRepBands_Write_HasColumnHeader, 0, [varEmpty]);
    AddGet(TQuickRepBands, 'HasDetail', TQuickRepBands_Read_HasDetail, 0, [varEmpty], varEmpty);
    AddSet(TQuickRepBands, 'HasDetail', TQuickRepBands_Write_HasDetail, 0, [varEmpty]);
    AddGet(TQuickRepBands, 'HasPageFooter', TQuickRepBands_Read_HasPageFooter, 0, [varEmpty], varEmpty);
    AddSet(TQuickRepBands, 'HasPageFooter', TQuickRepBands_Write_HasPageFooter, 0, [varEmpty]);
    AddGet(TQuickRepBands, 'HasSummary', TQuickRepBands_Read_HasSummary, 0, [varEmpty], varEmpty);
    AddSet(TQuickRepBands, 'HasSummary', TQuickRepBands_Write_HasSummary, 0, [varEmpty]);
    { TQRState }
    AddConst(cQuickrpt, 'qrAvailable', Ord(qrAvailable));
    AddConst(cQuickrpt, 'qrPrepare', Ord(qrPrepare));
    AddConst(cQuickrpt, 'qrPreview', Ord(qrPreview));
    AddConst(cQuickrpt, 'qrPrint', Ord(qrPrint));
    AddConst(cQuickrpt, 'qrEdit', Ord(qrEdit));
    { TQuickRepPrinterSettings }
    AddClass(cQuickrpt, TQuickRepPrinterSettings, 'TQuickRepPrinterSettings');
    { TQuickReportOption }
    AddConst(cQuickrpt, 'FirstPageHeader', Ord(FirstPageHeader));
    AddConst(cQuickrpt, 'LastPageFooter', Ord(LastPageFooter));
    AddConst(cQuickrpt, 'Compression', Ord(Compression));
    { TQuickRep }
    AddClass(cQuickrpt, TQuickRep, 'TQuickRep');
    AddGet(TQuickRep, 'Create', TQuickRep_Create, 1, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'CreateNew', TQuickRep_CreateNew, 1, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'CreateBand', TQuickRep_CreateBand, 1, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'TextHeight', TQuickRep_TextHeight, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TQuickRep, 'TextWidth', TQuickRep_TextWidth, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TQuickRep, 'AddBand', TQuickRep_AddBand, 1, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'AddNotifyClient', TQuickRep_AddNotifyClient, 1, [varEmpty], varEmpty);
    // AddGet(TQuickRep, 'ExportToFilter', TQuickRep_ExportToFilter, 1, [varEmpty], nil);
    AddGet(TQuickRep, 'EndPage', TQuickRep_EndPage, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'NewColumn', TQuickRep_NewColumn, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'NewPage', TQuickRep_NewPage, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'Paint', TQuickRep_Paint, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'Print', TQuickRep_Print, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'PrintBackground', TQuickRep_PrintBackground, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'PrinterSetup', TQuickRep_PrinterSetup, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'Prepare', TQuickRep_Prepare, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'Preview', TQuickRep_Preview, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'ResetPageFooterSize', TQuickRep_ResetPageFooterSize, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'RemoveBand', TQuickRep_RemoveBand, 1, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'SetBandValues', TQuickRep_SetBandValues, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'SetBounds', TQuickRep_SetBounds, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TQuickRep, 'AllDataSets', TQuickRep_Read_AllDataSets, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'AllDataSets', TQuickRep_Write_AllDataSets, 0, [varEmpty]);
    AddGet(TQuickRep, 'Available', TQuickRep_Read_Available, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'BandList', TQuickRep_Read_BandList, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'ColumnTopPosition', TQuickRep_Read_ColumnTopPosition, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'ColumnTopPosition', TQuickRep_Write_ColumnTopPosition, 0, [varEmpty]);
    AddGet(TQuickRep, 'CurrentColumn', TQuickRep_Read_CurrentColumn, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'CurrentX', TQuickRep_Read_CurrentX, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'CurrentX', TQuickRep_Write_CurrentX, 0, [varEmpty]);
    AddGet(TQuickRep, 'CurrentY', TQuickRep_Read_CurrentY, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'CurrentY', TQuickRep_Write_CurrentY, 0, [varEmpty]);
    { AddGet(TQuickRep, 'ExportFilter', TQuickRep_Read_ExportFilter, 0, [varEmpty], nil);
    AddSet(TQuickRep, 'ExportFilter', TQuickRep_Write_ExportFilter, 0, [varEmpty]); }
    AddGet(TQuickRep, 'Exporting', TQuickRep_Read_Exporting, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'FinalPass', TQuickRep_Read_FinalPass, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'HideBands', TQuickRep_Read_HideBands, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'HideBands', TQuickRep_Write_HideBands, 0, [varEmpty]);
    AddGet(TQuickRep, 'PageNumber', TQuickRep_Read_PageNumber, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'Printer', TQuickRep_Read_Printer, 0, [varEmpty], varEmpty);
    AddGet(TQuickRep, 'QRPrinter', TQuickRep_Read_QRPrinter, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'QRPrinter', TQuickRep_Write_QRPrinter, 0, [varEmpty]);
    AddGet(TQuickRep, 'RotateBands', TQuickRep_Read_RotateBands, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'RotateBands', TQuickRep_Write_RotateBands, 0, [varEmpty]);
    AddGet(TQuickRep, 'State', TQuickRep_Read_State, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'State', TQuickRep_Write_State, 0, [varEmpty]);
    AddGet(TQuickRep, 'Bands', TQuickRep_Read_Bands, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'Bands', TQuickRep_Write_Bands, 0, [varEmpty]);
    AddGet(TQuickRep, 'DataSet', TQuickRep_Read_DataSet, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'DataSet', TQuickRep_Write_DataSet, 0, [varEmpty]);
    AddGet(TQuickRep, 'Description', TQuickRep_Read_Description, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'Description', TQuickRep_Write_Description, 0, [varEmpty]);
    AddGet(TQuickRep, 'Options', TQuickRep_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'Options', TQuickRep_Write_Options, 0, [varEmpty]);
    AddGet(TQuickRep, 'Page', TQuickRep_Read_Page, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'Page', TQuickRep_Write_Page, 0, [varEmpty]);
    AddGet(TQuickRep, 'PrintIfEmpty', TQuickRep_Read_PrintIfEmpty, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'PrintIfEmpty', TQuickRep_Write_PrintIfEmpty, 0, [varEmpty]);
    AddGet(TQuickRep, 'PrinterSettings', TQuickRep_Read_PrinterSettings, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'PrinterSettings', TQuickRep_Write_PrinterSettings, 0, [varEmpty]);
    AddGet(TQuickRep, 'ReportTitle', TQuickRep_Read_ReportTitle, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'ReportTitle', TQuickRep_Write_ReportTitle, 0, [varEmpty]);
    AddGet(TQuickRep, 'ShowProgress', TQuickRep_Read_ShowProgress, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'ShowProgress', TQuickRep_Write_ShowProgress, 0, [varEmpty]);
    AddGet(TQuickRep, 'SnapToGrid', TQuickRep_Read_SnapToGrid, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'SnapToGrid', TQuickRep_Write_SnapToGrid, 0, [varEmpty]);
    AddGet(TQuickRep, 'Units', TQuickRep_Read_Units, 0, [varEmpty], varEmpty);
    AddSet(TQuickRep, 'Units', TQuickRep_Write_Units, 0, [varEmpty]);
    { TQRGroup }
    AddClass(cQuickrpt, TQRGroup, 'TQRGroup');
    AddGet(TQRGroup, 'Create', TQRGroup_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRGroup, 'Expression', TQRGroup_Read_Expression, 0, [varEmpty], varEmpty);
    AddSet(TQRGroup, 'Expression', TQRGroup_Write_Expression, 0, [varEmpty]);
    AddGet(TQRGroup, 'FooterBand', TQRGroup_Read_FooterBand, 0, [varEmpty], varEmpty);
    AddSet(TQRGroup, 'FooterBand', TQRGroup_Write_FooterBand, 0, [varEmpty]);
    AddGet(TQRGroup, 'Master', TQRGroup_Read_Master, 0, [varEmpty], varEmpty);
    AddSet(TQRGroup, 'Master', TQRGroup_Write_Master, 0, [varEmpty]);
    { TQRPrintableSize }
    AddClass(cQuickrpt, TQRPrintableSize, 'TQRPrintableSize');
    AddGet(TQRPrintableSize, 'Create', TQRPrintableSize_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRPrintableSize, 'Height', TQRPrintableSize_Read_Height, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintableSize, 'Height', TQRPrintableSize_Write_Height, 0, [varEmpty]);
    AddGet(TQRPrintableSize, 'Left', TQRPrintableSize_Read_Left, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintableSize, 'Left', TQRPrintableSize_Write_Left, 0, [varEmpty]);
    AddGet(TQRPrintableSize, 'Top', TQRPrintableSize_Read_Top, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintableSize, 'Top', TQRPrintableSize_Write_Top, 0, [varEmpty]);
    AddGet(TQRPrintableSize, 'Width', TQRPrintableSize_Read_Width, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintableSize, 'Width', TQRPrintableSize_Write_Width, 0, [varEmpty]);
    { TQRPrintable }
    AddClass(cQuickrpt, TQRPrintable, 'TQRPrintable');
    AddGet(TQRPrintable, 'Create', TQRPrintable_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRPrintable, 'ParentReport', TQRPrintable_Read_ParentReport, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintable, 'ParentReport', TQRPrintable_Write_ParentReport, 0, [varEmpty]);
    AddGet(TQRPrintable, 'Zoom', TQRPrintable_Read_Zoom, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintable, 'Zoom', TQRPrintable_Write_Zoom, 0, [varEmpty]);
    AddGet(TQRPrintable, 'Frame', TQRPrintable_Read_Frame, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintable, 'Frame', TQRPrintable_Write_Frame, 0, [varEmpty]);
    AddGet(TQRPrintable, 'Size', TQRPrintable_Read_Size, 0, [varEmpty], varEmpty);
    AddSet(TQRPrintable, 'Size', TQRPrintable_Write_Size, 0, [varEmpty]);
    { TQRCompositeReport }
    AddClass(cQuickrpt, TQRCompositeReport, 'TQRCompositeReport');
    AddGet(TQRCompositeReport, 'Create', TQRCompositeReport_Create, 1, [varEmpty], varEmpty);
    AddGet(TQRCompositeReport, 'Prepare', TQRCompositeReport_Prepare, 0, [varEmpty], varEmpty);
    AddGet(TQRCompositeReport, 'Preview', TQRCompositeReport_Preview, 0, [varEmpty], varEmpty);
    AddGet(TQRCompositeReport, 'Print', TQRCompositeReport_Print, 0, [varEmpty], varEmpty);
    AddGet(TQRCompositeReport, 'Reports', TQRCompositeReport_Read_Reports, 0, [varEmpty], varEmpty);
    AddSet(TQRCompositeReport, 'Reports', TQRCompositeReport_Write_Reports, 0, [varEmpty]);
    AddGet(TQRCompositeReport, 'Options', TQRCompositeReport_Read_Options, 0, [varEmpty], varEmpty);
    AddSet(TQRCompositeReport, 'Options', TQRCompositeReport_Write_Options, 0, [varEmpty]);
    AddGet(TQRCompositeReport, 'PrinterSettings', TQRCompositeReport_Read_PrinterSettings, 0, [varEmpty], varEmpty);
    AddSet(TQRCompositeReport, 'PrinterSettings', TQRCompositeReport_Write_PrinterSettings, 0, [varEmpty]);
    AddGet(TQRCompositeReport, 'ReportTitle', TQRCompositeReport_Read_ReportTitle, 0, [varEmpty], varEmpty);
    AddSet(TQRCompositeReport, 'ReportTitle', TQRCompositeReport_Write_ReportTitle, 0, [varEmpty]);
    AddHandler(cQuickrpt, 'TQROnNeedDataEvent', TJvInterpreterQuickrptEvent,
      @TJvInterpreterQuickrptEvent.QROnNeedDataEvent);
    AddHandler(cQuickrpt, 'TQRNotifyOperationEvent', TJvInterpreterQuickrptEvent,
      @TJvInterpreterQuickrptEvent.QRNotifyOperationEvent);
    AddHandler(cQuickrpt, 'TQRBandBeforePrintEvent', TJvInterpreterQuickrptEvent,
      @TJvInterpreterQuickrptEvent.QRBandBeforePrintEvent);
    AddHandler(cQuickrpt, 'TQRBandAfterPrintEvent', TJvInterpreterQuickrptEvent,
      @TJvInterpreterQuickrptEvent.QRBandAfterPrintEvent);
    AddHandler(cQuickrpt, 'TQRNotifyEvent', TJvInterpreterQuickrptEvent, @TJvInterpreterQuickrptEvent.QRNotifyEvent);
    AddHandler(cQuickrpt, 'TQRReportBeforePrintEvent', TJvInterpreterQuickrptEvent,
      @TJvInterpreterQuickrptEvent.QRReportBeforePrintEvent);
    AddHandler(cQuickrpt, 'TQRFilterEvent', TJvInterpreterQuickrptEvent, @TJvInterpreterQuickrptEvent.QRFilterEvent);
  end;

  RegisterClasses([TQuickRep, TQRSubDetail, TQRBand, TQRChildBand, TQRGroup,
    TQRLabel, TQRDBText, TQRExpr, TQRSysData, TQRMemo, TQRRichText,
      TQRDBRichText, TQRShape, TQRImage, TQRDBImage, TQRCompositeReport,
      TQRPreview]);
end;


initialization
  JvInterpreterRunReportPreviewProc := JvInterpreterRunReportPreview;
  JvInterpreterRunReportPreview2Proc := JvInterpreterRunReportPreview2;

end.

