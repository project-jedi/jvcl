unit JvInterpreterVariants;

interface

{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
uses
  Variants, SysUtils;

type
  TJvSimpleVariantType = class(TCustomVariantType)
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
  end;

  TJvRecordVariantType = class(TJvSimpleVariantType);
  TJvObjectVariantType = class(TJvSimpleVariantType);
  TJvClassVariantType = class(TJvSimpleVariantType);
  TJvPointerVariantType = class(TJvSimpleVariantType);
  TJvSetVariantType = class(TJvSimpleVariantType);
  TJvArrayVariantType = class(TJvSimpleVariantType);

var
  VariantRecordInstance: TJvRecordVariantType;
  VariantObjectInstance: TJvObjectVariantType;
  VariantClassInstance: TJvClassVariantType;
  VariantPointerInstance: TJvPointerVariantType;
  VariantSetInstance: TJvSetVariantType;
  VariantArrayInstance: TJvArrayVariantType;
{$ENDIF COMPILER6_UP}

implementation

{$IFDEF COMPILER6_UP}
{ TJvSimpleVariantType }

procedure TJvSimpleVariantType.Clear(var V: TVarData);
begin
  SimplisticClear(V);
end;

procedure TJvSimpleVariantType.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  SimplisticCopy(Dest, Source, Indirect);
end;

initialization
  VariantRecordInstance := TJvRecordVariantType.Create;
  VariantObjectInstance := TJvObjectVariantType.Create;
  VariantClassInstance := TJvClassVariantType.Create;
  VariantPointerInstance := TJvPointerVariantType.Create;
  VariantSetInstance := TJvSetVariantType.Create;
  VariantArrayInstance := TJvArrayVariantType.Create;

finalization
  FreeAndNil(VariantRecordInstance);
  FreeAndNil(VariantObjectInstance);
  FreeAndNil(VariantClassInstance);
  FreeAndNil(VariantPointerInstance);
  FreeAndNil(VariantSetInstance);
  FreeAndNil(VariantArrayInstance);
{$ENDIF COMPILER6_UP}

end.
