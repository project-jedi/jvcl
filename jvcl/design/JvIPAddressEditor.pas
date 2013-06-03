unit JvIPAddressEditor;

{$I jvcl.inc}

interface

uses
  JvComCtrls, DesignEditors, DesignIntf;

type
  TJvIPAddressProperty = class(TOrdinalProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
  end;

  TJvIPAddressAsTextProperty = class(TNestedProperty)
  public
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvIPAddressAsNumberProperty = class(TNestedProperty)
  public
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TJvIPAddressComponentProperty = class(TNestedProperty)
  protected
     Idx, ValIdx: Byte;
  public
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TJvIPAddressRangeProperty = class(TNestedProperty)
  protected
    Idx: Byte;
    IsMax: Boolean;
    function GetTheValue: byte;
  public
    function GetName: string; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    function GetAttributes: TPropertyAttributes; override;
    function GetIsDefault: Boolean; {$IFDEF DELPHI7_UP}override;{$ENDIF DELPHI7_UP}
  end;

implementation

uses
  SysUtils, JclSysUtils;

{ TJvIPAddressProperty }

function TJvIPAddressProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly{$IFDEF DELPHI2005_UP}, paDisplayReadOnly{$ENDIF}, paMultiSelect, paSubProperties, paRevertable, paVolatileSubProperties];
end;

procedure TJvIPAddressProperty.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  Vp: TJvIPAddressComponentProperty;
  E: IProperty; 
begin
  // outcome layout in OI:
  // byte values
  // x.x.x.x for easy editing (adjusting)
  //  12345 for easy editing (adjusting)

  for I := Low(TJvIPAddressComponentIndex) to High(TJvIPAddressComponentIndex) do
  begin
    Vp := TJvIPAddressComponentProperty.Create(Self);
    Vp.Idx := I;
    Vp.ValIdx := 5-I;

    E := Vp;
    Proc(E);
    E := nil;
  end;

  E := TJvIPAddressAsTextProperty.Create(Self);
  Proc(E);
  E := nil;

  E := TJvIPAddressAsNumberProperty.Create(Self);
  Proc(E);
  E := nil;
end;

function TJvIPAddressProperty.GetValue: string;
var
  Val: JvComCtrls.TJvIPAddressDual;
begin
  Val.Address := GetOrdValue;

  Result := Format('%u = %d.%d.%d.%d', [Val.Address, Val.Comps[4], Val.Comps[3], Val.Comps[2], Val.Comps[1]]);
end;

{ TJvIPAddressComponentProperty }

function TJvIPAddressComponentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
  if GetPropInfo.SetProc = nil then
     Result := Result + [paReadOnly{$IFDEF DELPHI2005_UP}, paDisplayReadOnly{$ENDIF}];

  if GetComponent(0) <> nil then
     if GetComponent(0) is TJvIPAddress then
        Include(Result, paSubProperties);
end;

function TJvIPAddressComponentProperty.GetName: string;
begin
  Result := ' .AddressValue['+Char(Idx+Ord('0')) +']';
end;

procedure TJvIPAddressComponentProperty.GetProperties(Proc: TGetPropProc);
var
  E: IProperty;
  Mx: Boolean;
  Vp: TJvIPAddressRangeProperty;
begin
  if GetComponent(0) <> nil then
    if GetComponent(0) is TJvIPAddress then
      for mx := false to true do
      begin
        Vp := TJvIPAddressRangeProperty.Create(self);
        Vp.Idx := Idx;
        Vp.IsMax := Mx;

        E := Vp;
        Proc(E);
        E := nil;
      end;
end;

function TJvIPAddressComponentProperty.GetValue: string;
var
  Val: JvComCtrls.TJvIPAddressDual;
begin
  Val.Address := GetOrdValue;
  Result := IntToStr(Val.Comps[ValIdx]);
end;

procedure TJvIPAddressComponentProperty.SetValue(const Value: string);
var
  Val: JvComCtrls.TJvIPAddressDual;
  I: integer;
begin
  if TryStrToInt(Value, I) then
    if (I >= 0) and (I <= 255) then
    begin
      Val.Address := GetOrdValue;
      Val.Comps[valIdx] := Byte(I);
      SetOrdValue(Val.Address);
    end;
end;

{ TJvIPAddressRangeProperty }

function TJvIPAddressRangeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable, paVolatileSubProperties];
end;

function TJvIPAddressRangeProperty.GetIsDefault: Boolean;
const DefVal: array[boolean] of byte = (0, 255);
begin
  Result := GetTheValue = DefVal[IsMax];
end;

function TJvIPAddressRangeProperty.GetName: string;
begin
  if IsMax then
    Result := 'Max'
  else
    Result := 'Min';

  Result := Result + ' ' + IntToStr(Idx);
end;

function TJvIPAddressRangeProperty.GetTheValue: byte;
var
  Range: TJvIPAddressRange;
  Tmp: Byte;
begin
  if IsMax then
    Tmp := 255
  else
    Tmp := 0;

  if GetComponent(0) <> nil then
    if GetComponent(0) is TJvIPAddress then
    begin
      Range := TJvIPAddress(GetComponent(0)).Range;
      if IsMax then
        Tmp := Range.Max[Idx]
      else
        Tmp := Range.Min[Idx];
     end;
     
  Result := Tmp;
end;

function TJvIPAddressRangeProperty.GetValue: string;
begin
  if GetComponent(0) <> nil then
    if GetComponent(0) is TJvIPAddress then
      Result := IntToStr(GetTheValue);
end;

procedure TJvIPAddressRangeProperty.SetValue(const Value: string);
var
  Range: TJvIPAddressRange;
  Tmp: integer;
begin
  if TryStrToInt(Value, Tmp) then
    if (Tmp>=0) and (Tmp <= 255)  then
      if GetComponent(0) <> nil then
        if GetComponent(0) is TJvIPAddress then
        begin
          Range := TJvIPAddress(GetComponent(0)).Range;
          if IsMax then
            Range.Max[Idx] := Tmp
          else
            Range.Min[Idx] := Tmp;
          Modified;
       end;
end;

{ TJvIPAddressAsNumberProperty }

function TJvIPAddressAsNumberProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
  if GetPropInfo.SetProc = nil then
     Result := Result + [paReadOnly{$IFDEF DELPHI2005_UP}, paDisplayReadOnly{$ENDIF}];
end;

function TJvIPAddressAsNumberProperty.GetName: string;
begin
   Result := ' as .Address';
end;

function TJvIPAddressAsNumberProperty.GetValue: string;
begin
  Result := Format('%u', [Cardinal(GetOrdValue)]);
end;

procedure TJvIPAddressAsNumberProperty.SetValue(const Value: string);
begin
  SetOrdValue(StrToUInt(Value));
end;

{ TJvIPAddressAsTextProperty }

function TJvIPAddressAsTextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
  if GetPropInfo.SetProc = nil then
     Result := Result + [paReadOnly{$IFDEF DELPHI2005_UP}, paDisplayReadOnly{$ENDIF}];
end;

function TJvIPAddressAsTextProperty.GetName: string;
begin
   Result := ' as .Text';
end;

function TJvIPAddressAsTextProperty.GetValue: string;
var
  Val: JvComCtrls.TJvIPAddressDual;
begin
  Val.Address := GetOrdValue;

  Result := Format('%d.%d.%d.%d', [Val.Comps[4], Val.Comps[3], Val.Comps[2], Val.Comps[1]]);
end;

procedure TJvIPAddressAsTextProperty.SetValue(const Value: string);
begin
  if GetComponent(0) <> nil then
    if GetComponent(0) is TJvIPAddress then
    begin
      TJvIPAddress(GetComponent(0)).Text := Value;
      Modified;
    end;
end;

end.
