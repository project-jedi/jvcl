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

The Original Code is: JvAnimatedEditor.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQAnimatedEditor;

{$I jvcl.inc}

interface

uses
  Types, QWindows, QForms, QGraphics, QImgList,
  SysUtils, Classes, QDialogs, QControls, 
  DesignIntf, DesignEditors, DesignMenus, CLXEditors, 
  JvQAnimatedImage;

type
  TJvAnimatedEditor = class(TComponentEditor)
  private
    FContinue: Boolean; 
    procedure CheckEdit(const PropertyEditor: IProperty); 
    procedure EditImage(Image: TJvAnimatedImage);
    procedure LoadAniFile(Image: TJvAnimatedImage);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  TypInfo,
  JvQDsgnConsts, JvQAni;

const
  cGlyphName = 'GLYPH';

//=== { TJvAnimatedEditor } ==================================================
  

procedure TJvAnimatedEditor.CheckEdit(const PropertyEditor: IProperty);
begin

    if FContinue and (CompareText(PropertyEditor.GetName, cGlyphName) = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end; 
end;


type
  TDesignerSelectionList = IDesignerSelections;


procedure TJvAnimatedEditor.EditImage(Image: TJvAnimatedImage);
var
  Components: TDesignerSelectionList;
begin 
  Components := TDesignerSelections.Create;  
    FContinue := True;
    Components.Add(Component);
    GetComponentProperties(Components, tkAny, Designer, CheckEdit); 
end;

procedure TJvAnimatedEditor.LoadAniFile(Image: TJvAnimatedImage);
var
  AniCursor: TJvAni;
begin
  AniCursor := LoadJvAniDialog;
  if AniCursor <> nil then
    try
      AniCursor.AssignToBitmap(Image.Glyph, clFuchsia, True,
        Image.Orientation = goVertical);
      Image.Interval := (AniCursor.Header.dwJIFRate * 100) div 6;
      if Image.Interval = 0 then
        Image.Interval := 100;
      Image.TransparentColor := clFuchsia;
      Designer.Modified;
    finally
      AniCursor.Free;
    end;
end;

procedure TJvAnimatedEditor.ExecuteVerb(Index: Integer);
begin
  if Index = GetVerbCount - 1 then
    LoadAniFile(TJvAnimatedImage(Component))
  else
  if Index = GetVerbCount - 2 then
    EditImage(TJvAnimatedImage(Component))
  else
    inherited ExecuteVerb(Index);
end;

function TJvAnimatedEditor.GetVerb(Index: Integer): string;
begin
  if Index = GetVerbCount - 1 then
    Result := RsLoadAniCursor
  else
  if Index = GetVerbCount - 2 then
    Result := RsEditPicture
  else
    Result := inherited GetVerb(Index);
end;

function TJvAnimatedEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 2;
end;

end.
