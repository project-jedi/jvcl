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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvPrgrss;

interface

uses SysUtils, Classes, Controls;

procedure RegisterProgressControl(AClass: TControlClass; const MaxPropName,
  MinPropName, ProgressPropName: string);
procedure UnRegisterProgressControl(AClass: TControlClass);
function SupportsProgressControl(Control: TControl): Boolean;

procedure SetProgressMax(Control: TControl; MaxValue: Longint);
procedure SetProgressMin(Control: TControl; MinValue: Longint);
procedure SetProgressValue(Control: TControl; ProgressValue: Longint);

implementation

{$DEFINE USE_GAUGE}
{$IFDEF WIN32}
  {$IFDEF USE_PROGRESSBAR}
    {$UNDEF USE_GAUGE}
  {$ENDIF}
{$ENDIF}

uses TypInfo, {$IFDEF WIN32} {$IFDEF USE_GAUGE} Gauges, {$ENDIF} ComCtrls;
  {$ELSE} Gauges; {$ENDIF}

{ TJvProgressList }

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
    procedure Add(AClass: TControlClass; const MaxPropName, MinPropName,
      ProgressPropName: string);
    function FindClass(AClass: TControlClass): Integer;
    procedure Remove(AClass: TControlClass);
    function SetControlProperty(Control: TControl; Prop: TProgressProp;
      Value: Longint): Boolean;
  end;

constructor TJvProgressList.Create;
begin
  inherited Create;
{$IFDEF WIN32}
  Add(TProgressBar, 'Max', 'Min', 'Position');
{$ENDIF}
{$IFDEF USE_GAUGE}
  Add(TGauge, 'MaxValue', 'MinValue', 'Progress');
{$ENDIF}
end;

destructor TJvProgressList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Dispose(PProgressData(Items[I]));
  inherited Destroy;
end;

procedure TJvProgressList.Add(AClass: TControlClass; const MaxPropName,
  MinPropName, ProgressPropName: string);
var
  NewRec: PProgressData;
begin
  New(NewRec);
  with NewRec^ do begin
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
  for Result := Count - 1 downto 0 do begin
    P := PProgressData(Items[Result]);
    if AClass.InheritsFrom(P^.ControlClass) then Exit;
  end;
  Result := -1;
end;

procedure TJvProgressList.Remove(AClass: TControlClass);
var
  I: Integer;
  P: PProgressData;
begin
  for I := Count - 1 downto 0 do begin
    P := PProgressData(Items[I]);
    if P^.ControlClass.InheritsFrom(AClass) then begin
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
  if (Control <> nil) then begin
    I := FindClass(TControlClass(Control.ClassType));
    if I >= 0 then begin
      case Prop of
        ppMax: PropName := PProgressData(Items[I])^.MaxProperty;
        ppMin: PropName := PProgressData(Items[I])^.MinProperty;
        else {ppProgress}
          PropName := PProgressData(Items[I])^.ProgressProperty;
      end;
      PropInfo := GetPropInfo(Control.ClassInfo, PropName);
      if (PropInfo <> nil) and (PropInfo^.PropType^.Kind in
        [tkInteger, tkFloat {$IFDEF WIN32}, tkVariant {$ENDIF}]) then
      begin
        SetOrdProp(Control, PropInfo, Value);
        Result := True;
      end;
    end;
  end;
end;

const
  ProgressList: TJvProgressList = nil;

function GeTJvProgressList: TJvProgressList;
begin
  if ProgressList = nil then ProgressList := TJvProgressList.Create;
  Result := ProgressList;
end;

function SupportsProgressControl(Control: TControl): Boolean;
begin
  if Control <> nil then
    Result := GeTJvProgressList.FindClass(TControlClass(Control.ClassType)) >= 0
  else Result := False;
end;

procedure RegisterProgressControl(AClass: TControlClass; const MaxPropName,
  MinPropName, ProgressPropName: string);
begin
  GeTJvProgressList.Add(AClass, MaxPropName, MinPropName, ProgressPropName);
end;

procedure UnRegisterProgressControl(AClass: TControlClass);
begin
  if ProgressList <> nil then ProgressList.Remove(AClass);
end;

procedure SetProgressMax(Control: TControl; MaxValue: Longint);
begin
  GeTJvProgressList.SetControlProperty(Control, ppMax, MaxValue);
end;

procedure SetProgressMin(Control: TControl; MinValue: Longint);
begin
  GeTJvProgressList.SetControlProperty(Control, ppMin, MinValue);
end;

procedure SetProgressValue(Control: TControl; ProgressValue: Longint);
begin
  GeTJvProgressList.SetControlProperty(Control, ppProgress, ProgressValue);
end;

{$IFNDEF WIN32}
procedure Finalize; far;
begin
  ProgressList.Free;
end;
{$ENDIF}

initialization
{$IFDEF WIN32}
finalization
  ProgressList.Free;
{$ELSE}
  AddExitProc(Finalize);
{$ENDIF}
end.
