unit JvFillRegFillers;

{$I JVCL.INC}

interface

procedure Register;

implementation

uses
  Classes, JvFillIntf, JvFillFontList;

procedure Register;
begin
  RegisterComponents('Jv Filler Providers', [TJvFontFiller]);
end;

end.
