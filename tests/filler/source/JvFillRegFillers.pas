unit JvFillRegFillers;

{$I JVCL.INC}

interface

procedure Register;

implementation

uses
  Classes, JvFillIntf, JvFillFontList, JvFillStringList;

procedure Register;
begin
  RegisterComponents('Jv Filler Providers', [TJvFontFiller, TJvStringsFiller]);
end;

end.
