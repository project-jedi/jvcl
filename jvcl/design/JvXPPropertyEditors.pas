{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPPropertyEditors.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvXPPropertyEditors;

interface

uses
  Classes, SysUtils,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  Contnrs, DsgnIntf,
  {$ENDIF COMPILER6_UP}
  {$IFDEF VisualCLX}
  JvQDsgnEditors,
  {$ENDIF VisualCLX}
  Windows, Forms, ImgList, ActnList, Graphics,
  TypInfo;

type
  {$IFDEF COMPILER6_UP}
  TDesignerSelectionList = IDesignerSelections;
  {$ELSE}
  //TDesignerSelectionList = TComponentList;
  {$ENDIF COMPILER6_UP}

  {$IFDEF VCL}
  TJvXPCustomImageIndexPropertyEditor = class(TIntegerProperty
    {$IFDEF COMPILER6_UP}, ICustomPropertyListDrawing {$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); {$IFNDEF COMPILER6_UP} override; {$ENDIF}
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); {$IFNDEF COMPILER6_UP} override; {$ENDIF}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF COMPILER6_UP} override; {$ENDIF}
  end;
  {$ENDIF VCL} 

  {$IFDEF VisualCLX}
  TJvXPCustomImageIndexPropertyEditor = class(TJvDefaultImageIndexProperty)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
  end;
  {$ENDIF VisualCLX}

  TJvXPItemImageIndexPropertyEditor = class(TJvXPCustomImageIndexPropertyEditor)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; override;
  end;

  TJvXPBarItemEditor = class(TDefaultEditor)
  protected
    {$IFDEF COMPILER6_UP}
    procedure RunPropertyEditor(const Prop: IProperty);
    {$ELSE}
    procedure RunPropertyEditor(Prop: TPropertyEditor);
    {$ENDIF COMPILER6_UP}
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvDsgnConsts,
  {$ENDIF USEJVCL}
  JvXPBar;

{$IFNDEF USEJVCL}
resourcestring
  RsItemEditorEllipsis = 'Item Editor...';
  RsDefaultColorItem = 'Restore Default Colors';
  RsDefaultFontsItem = 'Restore Default Fonts';
{$ENDIF USEJVCL}

type
  TCustomWinXPBar = class(TJvXPCustomWinXPBar)
  public
    property HotTrackColor;
  end;

//=== { TJvXPCustomImageIndexPropertyEditor } ================================

function TJvXPCustomImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
begin
  Result := nil;
end;

{$IFDEF VCL}

function TJvXPCustomImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TJvXPCustomImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) then
    for I := 0 to ImgList.Count -1 do
      Proc(IntToStr(I));
end;


procedure TJvXPCustomImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then
  begin
    ImgList.Draw(ACanvas, X, ARect.Top + 2, StrToInt(Value));
    Inc(X, ImgList.Width);
  end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TJvXPCustomImageIndexPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TJvXPCustomImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;

{$ENDIF VCL}

//=== { TJvXPItemImageIndexPropertyEditor } ==================================

function TJvXPItemImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  Item: TPersistent;
begin
  Result := nil;
  Item := GetComponent(Index);
  if Item is TJvXPBarItem then
    Result := TJvXPBarItem(Item).Images;
end;

//=== { TJvXPBarItemEditor } =================================================

procedure TJvXPBarItemEditor.Edit;
var
  Components: TDesignerSelectionList;
begin
  {$IFDEF COMPILER6_UP}
  Components := CreateSelectionList;
  {$ELSE}
  Components := TDesignerSelectionList.Create;
  {$ENDIF COMPILER6_UP}
  Components.Add(Component);
  GetComponentProperties(Components, [tkClass], Designer, RunPropertyEditor);
end;

procedure TJvXPBarItemEditor.ExecuteVerb(Index: Integer);
const
 cFontColor = $00840000;
 cHotTrackColor = $00FF7C35;
begin
  case Index of
    0: // 'Item Editor...'
      Edit;
    1: // 'Restore Default Colors'
      with TCustomWinXPBar(Component) do
      begin
        Font.Color := cFontColor;
        HeaderFont.Color := cFontColor;
        HotTrackColor := cHotTrackColor;
        if csDesigning in ComponentState then
          {$IFDEF VCL}
          TCustomForm(Owner).Designer.Modified;
          {$ENDIF VCL}
          {$IFDEF VisualCLX}
          TCustomForm(Owner).DesignerHook.Modified;
          {$ENDIF VisualCLX}
      end;
    2: // 'Restore Default Fonts'
      with TCustomWinXPBar(Component) do
      begin
        ParentFont := True;
        if csDesigning in ComponentState then
          {$IFDEF VCL}
          TCustomForm(Owner).Designer.Modified;
          {$ENDIF VCL}
          {$IFDEF VisualCLX}
          TCustomForm(Owner).DesignerHook.Modified;
          {$ENDIF VisualCLX}
      end;
  end;
end;

function TJvXPBarItemEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsItemEditorEllipsis;
    1:
      Result := RsDefaultColorItem;
    2:
      Result := RsDefaultFontsItem;
  end;
end;

function TJvXPBarItemEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{$IFDEF COMPILER6_UP}
procedure TJvXPBarItemEditor.RunPropertyEditor(const Prop: IProperty);
{$ELSE}
procedure TJvXPBarItemEditor.RunPropertyEditor(Prop: TPropertyEditor);
{$ENDIF COMPILER6_UP}
begin
  if UpperCase(Prop.GetName) = 'ITEMS' then
    Prop.Edit;
end;

end.

