{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvQStdDsgnEditors.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 att users dott sourceforge dott net]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Added editors for JvFooter and JvGroupHeader

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQStdDsgnEditors;

interface

uses
  Classes, SysUtils,
  QForms, QControls, QGraphics, QExtCtrls, Tabs, QDialogs,
  QExtDlgs, QMenus, QStdCtrls, QImgList,
  DsnConst,
  RTLConsts, DesignIntf, DesignEditors, DesignMenus,
  ClxEditors;

type
  TJvDateTimeExProperty = class(TDateTimeProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvDateExProperty = class(TDateProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvTimeExProperty = class(TTimeProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


implementation

uses
  TypInfo, Math,
  JvQTypes, JvQDateTimeForm, JvQDsgnConsts;

//=== TJvDateTimeExProperty ==================================================

procedure TJvDateTimeExProperty.Edit;
var
  D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TFrmSelectDateTimeDlg.SelectDateTime(D, dstDateTime) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TJvDateTimeExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

//=== TJvDateExProperty ======================================================

procedure TJvDateExProperty.Edit;
var
  D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now;
  if TFrmSelectDateTimeDlg.SelectDateTime(D, dstDate) then
  begin
    SetFloatValue(D);
    Designer.Modified;
  end;
end;

function TJvDateExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

//=== TJvTimeExProperty ======================================================

procedure TJvTimeExProperty.Edit;
var
  D: TDateTime;
begin
  D := GetFloatValue;
  if D = 0.0 then
    D := Now
  else // (p3) we need the date part or we might get a "Must be in ShowCheckBox mode" error 
    D := SysUtils.Date + Frac(D);
  if TFrmSelectDateTimeDlg.SelectDateTime(D, dstTime) then
  begin
    SetFloatValue(Frac(D)); // (p3) only return the time portion
    Designer.Modified;
  end;
end;

function TJvTimeExProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

end.
