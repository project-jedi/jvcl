{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColorProviderDsgnTreeFrame.pas, released on 2003-12-02.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvColorProviderDsgnTreeFrame;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ActnList, ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs, QImgList, QMenus, QActnList, QComCtrls,
  {$ENDIF VisualCLX}
  JvProviderTreeListDsgnFrame;

type
  TfmeJvColorProviderDsgnTree = class(TfmeJvProviderTreeListDsgn)
  public
    procedure UpdateActionStates; override;
  published
    procedure aiAddItemExecute(Sender: TObject); override;
    procedure aiDeleteExecute(Sender: TObject); override;
    procedure aiClearExecute(Sender: TObject); override;
  end;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

uses
  JclStrings,
  JvColorProvider, JvDataProviderIntf, JvColorProviderAddDialogForm, JvTypes;

//=== { TfmeJvColorProviderDsgnTree } =========================================

procedure TfmeJvColorProviderDsgnTree.UpdateActionStates;
var
  Item: IJvDataItem;
  ColItem: IJvColorItem;
  GrpItem: IJvDataItem;
begin
  if lvProvider.SelCount <> 0 then
    Item := GetDataItem(lvProvider.Selected.Index);
  if Supports(Item, IJvColorItem, ColItem) then
    GrpItem := Item.Items.GetParent
  else
    GrpItem := Item;

  aiAddItem.Enabled := GrpItem <> nil;
  aiDelete.Enabled := ColItem <> nil;
  aiClear.Enabled := (GrpItem <> nil) and (ColItem = nil);
  aiRename.Enabled := ColItem <> nil;
end;

procedure TfmeJvColorProviderDsgnTree.aiAddItemExecute(Sender: TObject);
var
  Item: IJvDataItem;
  ColItem: IJvColorItem;
  GrpItem: IJvDataItem;
  GrpType: TColorType;
  NewColor: TColor;
begin
  Provider.Enter;
  try
    if lvProvider.SelCount <> 0 then
      Item := GetDataItem(lvProvider.Selected.Index);
    if Supports(Item, IJvColorItem, ColItem) then
      GrpItem := Item.Items.GetParent
    else
      GrpItem := Item;

    if GrpItem <> nil then
    begin
      GrpType := TColorType(StrToInt(StrRestOf(GrpItem.GetID,
        Length(cColorProviderGroupHeaderID) + 1)));
      if DoAddDsgnColor(GrpType, Provider.ProviderIntf, NewColor) then
        (Provider.ProviderIntf as IJvColorProvider).AddColor(GrpType, NewColor);
    end;
  finally
    Provider.Leave;
  end;
end;

procedure TfmeJvColorProviderDsgnTree.aiDeleteExecute(Sender: TObject);
var
  Item: IJvDataItem;
  ColItem: IJvColorItem;
  GrpItem: IJvDataItem;
begin
  Provider.Enter;
  try
    if lvProvider.SelCount <> 0 then
      Item := GetDataItem(lvProvider.Selected.Index);
    if Supports(Item, IJvColorItem, ColItem) then
    begin
      GrpItem := Item.Items.GetParent;
      case TColorType(StrToInt(StrRestOf(GrpItem.GetID, Length(cColorProviderGroupHeaderID)))) of
        ctStandard:
          (Provider.ProviderIntf as IJvColorProvider).DeleteStdColor(ColItem.Color);
        ctSystem:
          (Provider.ProviderIntf as IJvColorProvider).DeleteSysColor(ColItem.Color);
        ctCustom:
          (Provider.ProviderIntf as IJvColorProvider).DeleteCstColor(ColItem.Color);
      end;
    end;
  finally
    Provider.Leave;
  end;
end;

procedure TfmeJvColorProviderDsgnTree.aiClearExecute(Sender: TObject);
var
  Item: IJvDataItem;
  ColItem: IJvColorItem;
  GrpItem: IJvDataItem;
begin
  Provider.Enter;
  try
    if lvProvider.SelCount <> 0 then
      Item := GetDataItem(lvProvider.Selected.Index);
    if Supports(Item, IJvColorItem, ColItem) then
      GrpItem := Item.Items.GetParent
    else
      GrpItem := Item;

    if GrpItem <> nil then
      case TColorType(StrToInt(StrRestOf(GrpItem.GetID, Length(cColorProviderGroupHeaderID)))) of
        ctStandard:
          (Provider.ProviderIntf as IJvColorProvider).ClearStdColorList;
        ctSystem:
          (Provider.ProviderIntf as IJvColorProvider).ClearSysColorList;
        ctCustom:
          (Provider.ProviderIntf as IJvColorProvider).ClearCstColorList;
      end;
  finally
    Provider.Leave;
  end;
end;

end.
