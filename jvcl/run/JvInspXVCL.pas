{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Marcel Bestebroer
 <marcelb att zeelandnet dott nl>.
Portions created by Marcel Bestebroer are Copyright (C) 2000 - 2001 mbeSoft.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  JvInspector XVCL data layer. Provides access to TJvxNode and descendants.
  XVCL can be obtained from the XVCL home page, located at
  http://xvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvInspXVCL;

interface

uses
  JvInspector, JvxClasses;

type
  TJvInspectorxNodeData = class(TJvCustomInspectorData)
  private
    FJvxNode: TJvxNode;
  protected
    function GetAsFloat: Extended; override;
    function GetAsInt64: Int64; override;
    function GetAsMethod: TMethod; override;
    function GetAsOrdinal: Int64; override;
    function GetAsString: string; override;
    function GetJvxNode: TJvxNode; virtual;
    function IsEqualReference(const Ref: TJvCustomInspectorData): Boolean; override;
    procedure NodeNotifyEvent(Sender: TJvxNode; Operation: TJvxNodeOperation); virtual;
    procedure SetAsFloat(const Value: Extended); override;
    procedure SetAsInt64(const Value: Int64); override;
    procedure SetAsMethod(const Value: TMethod); override;
    procedure SetAsOrdinal(const Value: Int64); override;
    procedure SetAsString(const Value: string); override;
    procedure SetJvxNode(Value: TJvxNode); virtual;
  public
    procedure GetAsSet(var Buf); override;
    function HasValue: Boolean; override;
    function IsAssigned: Boolean; override;
    function IsInitialized: Boolean; override;
    class function New(const AParent: TJvCustomInspectorItem; const AName: string; const AJvxNode: TJvxNode): TJvCustomInspectorItem;
    procedure SetAsSet(const Buf); override;
    property JvxNode: TJvxNode read GetJvxNode write SetJvxNode;
  end;

implementation

uses
  {$IFDEF VCL}
  Consts,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QConsts,
  {$ENDIF VisualCLX}
  SysUtils, TypInfo,
  JvTypes, JvResources;

function TJvInspectorxNodeData.GetAsFloat: Extended;
begin
  CheckReadAccess;
  if JvxNode.TypeInfo^.Kind = tkFloat then
    Result := JvxNode.AsFloat
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

function TJvInspectorxNodeData.GetAsInt64: Int64;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

function TJvInspectorxNodeData.GetAsMethod: TMethod;
begin
  CheckReadAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

function TJvInspectorxNodeData.GetAsOrdinal: Int64;
begin
  CheckReadAccess;
  if JvxNode.TypeInfo^.Kind in
    [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar, tkClass] then
  begin
    if GetTypeData(JvxNode.TypeInfo).OrdType = otULong then
      Result := Cardinal(JvxNode.AsInteger)
    else
      Result := JvxNode.AsInteger;
  end
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

function TJvInspectorxNodeData.GetAsString: string;
begin
  CheckReadAccess;
  if JvxNode.TypeInfo^.Kind in [tkString, tkLString, tkWString] then
    Result := JvxNode.AsString
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

function TJvInspectorxNodeData.GetJvxNode: TJvxNode;
begin
  Result := FJvxNode;
end;

function TJvInspectorxNodeData.IsEqualReference(const Ref: TJvCustomInspectorData): Boolean;
begin
  Result := (Ref is TJvInspectorxNodeData) and (TJvInspectorxNodeData(Ref).JvxNode = JvxNode);
end;

procedure TJvInspectorxNodeData.NodeNotifyEvent(Sender: TJvxNode;
  Operation: TJvxNodeOperation);
begin
  if (Sender = JvxNode) and (Operation = noChange) then
  begin
    InvalidateData;
    Invalidate;
  end;
end;

procedure TJvInspectorxNodeData.SetAsFloat(const Value: Extended);
begin
  CheckWriteAccess;
  if JvxNode.TypeInfo^.Kind = tkFloat then
    JvxNode.AsFloat := Value
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorFloat]);
end;

procedure TJvInspectorxNodeData.SetAsInt64(const Value: Int64);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorInt64]);
end;

procedure TJvInspectorxNodeData.SetAsMethod(const Value: TMethod);
begin
  CheckWriteAccess;
  raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorTMethod]);
end;

procedure TJvInspectorxNodeData.SetAsOrdinal(const Value: Int64);
var
  MinValue: Int64;
  MaxValue: Int64;
begin
  CheckWriteAccess;
  if TypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
  begin
    case GetTypeData(TypeInfo).OrdType of
      otSByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          JvxNode.AsInteger := Shortint(Value)
        end;
      otUByte:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          JvxNode.AsInteger := Byte(Value)
        end;
      otSWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          JvxNode.AsInteger := Smallint(Value)
        end;
      otUWord:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          JvxNode.AsInteger := Word(Value)
        end;
      otSLong:
        begin
          MinValue := GetTypeData(TypeInfo).MinValue;
          MaxValue := GetTypeData(TypeInfo).MaxValue;
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          JvxNode.AsInteger := Integer(Value)
        end;
      otULong:
        begin
          MinValue := Longword(GetTypeData(TypeInfo).MinValue);
          MaxValue := Longword(GetTypeData(TypeInfo).MaxValue);
          if (Value < MinValue) or (Value > MaxValue) then
            raise ERangeError.CreateResFmt(@SOutOfRange, [MinValue, MaxValue]);
          JvxNode.AsInteger := Integer(Value)
        end;
    end;
  end
  else
  if TypeInfo.Kind = tkClass then
    JvxNode.AsInteger := Integer(Value)
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorOrdinal]);
end;

procedure TJvInspectorxNodeData.SetAsString(const Value: string);
begin
  CheckWriteAccess;
  if JvxNode.TypeInfo.Kind in [tkString, tkLString, tkWString] then
    JvxNode.AsString := Value
  else
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorString]);
end;

procedure TJvInspectorxNodeData.SetJvxNode(Value: TJvxNode);
begin
  if Value <> JvxNode then
  begin
    if JvxNode <> nil then
      JvxNode.OnNotifyEvents.Remove(NodeNotifyEvent);
    FJvxNode := Value;
    if JvxNode <> nil then
    begin
      JvxNode.OnNotifyEvents.Add(NodeNotifyEvent);
      TypeInfo := Value.TypeInfo;
    end
  end;
end;

procedure TJvInspectorxNodeData.GetAsSet(var Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  TmpInt: Integer;
begin
  CheckReadAccess;
  if JvxNode.TypeInfo.Kind <> tkSet then
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  if ResBytes > 4 then
    ResBytes := 4;
  TmpInt := JvxNode.AsInteger;
  Move(TmpInt, Buf, ResBytes);
end;

function TJvInspectorxNodeData.HasValue: Boolean;
begin
  Result := IsInitialized and (JvxNode.TypeInfo <> nil);
end;

function TJvInspectorxNodeData.IsAssigned: Boolean;
begin
  Result := IsInitialized and JvxNode.Assigned;
end;

function TJvInspectorxNodeData.IsInitialized: Boolean;
begin
  Result := (JvxNode <> nil);
end;

class function TJvInspectorxNodeData.New(const AParent: TJvCustomInspectorItem;
  const AName: string; const AJvxNode: TJvxNode): TJvCustomInspectorItem;
var
  Data: TJvInspectorxNodeData;
begin
  if AJvxNode = nil then
    raise EJVCLException.CreateRes(@RsENoNodeSpecified);
  if AJvxNode.NodeName <> '' then
    Data := TJvInspectorxNodeData.CreatePrim(AJvxNode.NodeName, AJvxNode.TypeInfo))
  else
    Data := TJvInspectorxNodeData.CreatePrim(AName, AJvxNode.TypeInfo));
  Data.JvxNode := AJvxNode;
  Data := TJvInspectorxNodeData(RegisterInstance(Data));
  if Data <> nil then
    Result := Data.NewItem(AParent)
  else
    Result := nil;
end;

procedure TJvInspectorxNodeData.SetAsSet(const Buf);
var
  CompType: PTypeInfo;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  TmpInt: Integer;
begin
  CheckWriteAccess;
  if JvxNode.TypeInfo.Kind <> tkSet then
    raise EJvInspectorData.CreateResFmt(@RsEJvInspDataNoAccessAs, [cJvInspectorSet]);
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  if ResBytes > 4 then
    ResBytes := 4;
  TmpInt := 0;
  Move(Buf, TmpInt, ResBytes);
  JvxNode.AsInteger := TmpInt;
end;

end.
