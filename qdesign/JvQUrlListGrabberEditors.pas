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

The Original Code is: JvUrlListGrabberEditors.Pas, released on 2003-11-02.

The Initial Developer of the Original Code is Olivier Sannier [obones att altern dott org]
Portions created by Olivier Sannier are Copyright (C) 2003 Olivier Sannier.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQUrlListGrabberEditors;

interface

uses
  Types, QWindows, 
  DesignIntf, DesignEditors, DesignMenus, CLXEditors, 
  Classes;

type
  TJvUrlGrabberDefaultPropertiesListEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override; 
    procedure GetProperties(Proc: TGetPropProc); override; 
  end;

  TJvUrlGrabberDefaultPropertiesEditor = class (TClassProperty)
  public
    function GetName: string; override;
  end;

  TJvUrlGrabberIndexProperty = class (TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses
  TypInfo,
  JvQUrlListGrabber,
  SysUtils;

const
  None: string = '(none)';

//=== { TJvUrlGrabberDefaultPropertiesListEditor } ===========================

function TJvUrlGrabberDefaultPropertiesListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paReadOnly];
end;


procedure TJvUrlGrabberDefaultPropertiesListEditor.GetProperties(Proc: TGetPropProc);
var
  UrlListGrabber: TJvUrlListGrabber;
  I: Integer;
  Components: IDesignerSelections;
begin
  inherited GetProperties(Proc);

  UrlListGrabber := TJvUrlListGrabber(GetComponent(0));
  for I := 0 to UrlListGrabber.DefaultGrabbersProperties.Count - 1 do
  begin
    Components := CreateSelectionList;
    Components.Add(UrlListGrabber.DefaultGrabbersProperties.Items[I].EditorTrick);
    GetComponentProperties(Components, tkAny, Designer, Proc);
  end;
end;


//=== { TJvUrlGrabberDefaultPropertiesEditor } ===============================

function TJvUrlGrabberDefaultPropertiesEditor.GetName: string;
var
  EditorTrick: TJvUrlGrabberDefPropEdTrick;
begin
  // get Supported URL name from the real default properties
  EditorTrick := TJvUrlGrabberDefPropEdTrick(GetComponent(0));
  Result := EditorTrick.DefaultProperties.SupportedURLName;
end;

{ TJvUrlGrabberIndexProperty }

function TJvUrlGrabberIndexProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TJvUrlGrabberIndexProperty.GetValue: string;
var
  Index: Integer;
begin
  Index := GetOrdValue;
  if (Index < 0) or (Index >= JvUrlGrabberClassList.Count) then
    Result := '-1 - ' + None
  else
    Result := IntToStr(Index) + ' - ' + JvUrlGrabberClassList[Index].GetSupportedURLName;
end;

procedure TJvUrlGrabberIndexProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  Proc('-1 - ' + None);
  for I := 0 to JvUrlGrabberClassList.Count - 1 do
    Proc(IntToStr(I) + ' - ' + JvUrlGrabberClassList[I].GetSupportedURLName);
end;

procedure TJvUrlGrabberIndexProperty.SetValue(const Value: string);
var
  I: Integer;
  OrdValue : Integer;
  CleanValue: string;
begin
  // The value may be a simple text or may contain a number in front
  // of it. So we clean it to only contain the text
  CleanValue := Value;
  if Pos('-', Value) > 0 then
    CleanValue := Trim(Copy(Value, Pos('-', Value)+1, Length(Value)));

  if CleanValue = None then
    SetOrdValue(-1)
  else
  begin
    I := 0;
    OrdValue := -1;
    while (I < JvUrlGrabberClassList.Count) and (OrdValue = -1) do
    begin
      if JvUrlGrabberClassList[I].GetSupportedURLName = CleanValue then
        OrdValue := I;
      Inc(I);
    end;
    SetOrdValue(OrdValue);
  end;
end;

end.
