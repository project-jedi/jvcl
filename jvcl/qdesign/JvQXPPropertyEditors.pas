{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQXPPropertyEditors;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils, 
  DesignIntf, DesignEditors, CLXEditors,  
  JvQDsgnEditors, 
  QWindows, QForms, QImgList, QActnList, QGraphics,
  TypInfo;

type 
  TDesignerSelectionList = IDesignerSelections; 
  
 
  TJvXPCustomImageIndexPropertyEditor = class(TJvDefaultImageIndexProperty)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
  end; 

  TJvXPItemImageIndexPropertyEditor = class(TJvXPCustomImageIndexPropertyEditor)
  public
    function GetImageListAt(Index: Integer): TCustomImageList; override;
  end;

  TJvXPBarItemEditor = class(TDefaultEditor)
  protected 
    procedure RunPropertyEditor(const Prop: IProperty); 
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses 
  JvQDsgnConsts, 
  JvQXPBar;



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
  Components := CreateSelectionList; 
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
          TCustomForm(Owner).DesignerHook.Modified; 
      end;
    2: // 'Restore Default Fonts'
      with TCustomWinXPBar(Component) do
      begin
        ParentFont := True;
        if csDesigning in ComponentState then  
          TCustomForm(Owner).DesignerHook.Modified; 
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


procedure TJvXPBarItemEditor.RunPropertyEditor(const Prop: IProperty);

begin
  if UpperCase(Prop.GetName) = 'ITEMS' then
    Prop.Edit;
end;

end.

