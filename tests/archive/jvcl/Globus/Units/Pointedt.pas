unit PointEdt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DsgnIntf, TypInfo;//FrTypes;

type
  TPointProperty = class(TPropertyEditor)

    function GetAttributes : TPropertyAttributes; override;
    function GetValue: string; override;
//    procedure Edit; override;
  end;

procedure Register;

implementation
var
  PPointTypeInfo : PTypeInfo;
  PointTypeInfo : TTypeInfo;

{== TPointPropertyEditor Methods ==}

function TPointProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [];// paSubProperties, paReadOnly ];
end;

function TPointProperty.GetValue: string;
var
  pPT: PPoint;
begin
//  pPT := PPoint(GetOrdValue);
//  Result := Format('(%d,%d)', [ ppt^.x, ppt^.y ]);
  Result := '[,]';
end;

procedure Register;
begin
  PointTypeInfo.Name := 'TPoint';
  PointTypeInfo.Kind := tkFloat;
  PPointTypeInfo := @PointTypeInfo;
  RegisterPropertyEditor( TypeInfo(TPoint), nil,
                          '', TPointProperty );

end;

end.
