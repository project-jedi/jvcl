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

The Original Code is: JvPrgrss.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQProgressUtils;

{$I jvcl.inc}

interface

uses  
  QControls, QComCtrls, 
  SysUtils, Classes,
  JvQFinalize;


type
  TControlClass = class of TControl;


procedure RegisterProgressControl(AClass: TControlClass;
  const MaxPropName, MinPropName, ProgressPropName: string);
procedure UnRegisterProgressControl(AClass: TControlClass);
function SupportsProgressControl(Control: TControl): Boolean;

procedure SetProgressMax(Control: TControl; MaxValue: Longint);
procedure SetProgressMin(Control: TControl; MinValue: Longint);
procedure SetProgressValue(Control: TControl; ProgressValue: Longint);

implementation

uses
  TypInfo;

const
  sUnitName = 'JvProgressUtils';

type
  TProgressProp = (ppMax, ppMin, ppProgress);

  PProgressData = ^TProgressData;
  TProgressData = record
    ControlClass: TControlClass;
    MaxProperty: string[63];
    MinProperty: string[63];
    ProgressProperty: string[63];
  end;

  TJvProgressList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AClass: TControlClass;
      const MaxPropName, MinPropName, ProgressPropName: string);
    function FindClass(AClass: TControlClass): Integer;
    procedure Remove(AClass: TControlClass);
    function SetControlProperty(Control: TControl; Prop: TProgressProp;
      Value: Longint): Boolean;
  end;

constructor TJvProgressList.Create;
begin
  inherited Create;
  Add(TProgressBar, 'Max', 'Min', 'Position');
end;

destructor TJvProgressList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(PProgressData(Items[I]));
  inherited Destroy;
end;

procedure TJvProgressList.Add(AClass: TControlClass;
  const MaxPropName, MinPropName, ProgressPropName: string);
var
  NewRec: PProgressData;
begin
  New(NewRec);
  with NewRec^ do
  begin
    ControlClass := AClass;
    MaxProperty := MaxPropName;
    MinProperty := MinPropName;
    ProgressProperty := ProgressPropName;
  end;
  inherited Add(NewRec);
end;

function TJvProgressList.FindClass(AClass: TControlClass): Integer;
var
  P: PProgressData;
begin
  for Result := Count - 1 downto 0 do
  begin
    P := PProgressData(Items[Result]);
    if AClass.InheritsFrom(P^.ControlClass) then
      Exit;
  end;
  Result := -1;
end;

procedure TJvProgressList.Remove(AClass: TControlClass);
var
  I: Integer;
  P: PProgressData;
begin
  for I := Count - 1 downto 0 do
  begin
    P := PProgressData(Items[I]);
    if P^.ControlClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

function TJvProgressList.SetControlProperty(Control: TControl;
  Prop: TProgressProp; Value: Longint): Boolean;
var
  PropInfo: PPropInfo;
  I: Integer;
  PropName: string;
begin
  Result := False;
  if Control <> nil then
  begin
    I := FindClass(TControlClass(Control.ClassType));
    if I >= 0 then
    begin
      case Prop of
        ppMax:
          PropName := PProgressData(Items[I])^.MaxProperty;
        ppMin:
          PropName := PProgressData(Items[I])^.MinProperty;
      else {ppProgress}
        PropName := PProgressData(Items[I])^.ProgressProperty;
      end;
      PropInfo := GetPropInfo(Control.ClassInfo, PropName);
      if (PropInfo <> nil) and
        (PropInfo^.PropType^.Kind in [tkInteger, tkFloat, tkVariant]) then
      begin
        SetOrdProp(Control, PropInfo, Value);
        Result := True;
      end;
    end;
  end;
end;

// (rom) changed to var
var
  ProgressList: TJvProgressList = nil;

function GetProgressList: TJvProgressList;
begin
  if ProgressList = nil then
  begin
    ProgressList := TJvProgressList.Create;
    AddFinalizeObjectNil(sUnitName, TObject(ProgressList));
  end;
  Result := ProgressList;
end;

function SupportsProgressControl(Control: TControl): Boolean;
begin
  if Control <> nil then
    Result := GetProgressList.FindClass(TControlClass(Control.ClassType)) >= 0
  else
    Result := False;
end;

procedure RegisterProgressControl(AClass: TControlClass;
  const MaxPropName, MinPropName, ProgressPropName: string);
begin
  GetProgressList.Add(AClass, MaxPropName, MinPropName, ProgressPropName);
end;

procedure UnRegisterProgressControl(AClass: TControlClass);
begin
  GetProgressList.Remove(AClass);
end;

procedure SetProgressMax(Control: TControl; MaxValue: Longint);
begin
  GetProgressList.SetControlProperty(Control, ppMax, MaxValue);
end;

procedure SetProgressMin(Control: TControl; MinValue: Longint);
begin
  GetProgressList.SetControlProperty(Control, ppMin, MinValue);
end;

procedure SetProgressValue(Control: TControl; ProgressValue: Longint);
begin
  GetProgressList.SetControlProperty(Control, ppProgress, ProgressValue);
end;

initialization

finalization
  FinalizeUnit(sUnitName);

end.

