unit ClassQueryDisplay;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvXmlDatabase;

type
  TQueryDisplayer = class
  private
    FParent: TObject;
  public
    constructor Create(AOwner: TObject);
    procedure Generate(const AQuery: TJvXmlQuery; ATemplate: string; AOutput: TStream);
  end;


implementation

uses
  JvSimpleXml, ClassScript;

{ TQueryDisplayer }

{**********************************************************************}
constructor TQueryDisplayer.Create(AOwner: TObject);
begin
  FParent := AOwner;
end;
{**********************************************************************}
procedure TQueryDisplayer.Generate(const AQuery: TJvXmlQuery;
  ATemplate: string; AOutput: TStream);
var
 st, st2, st3, lToken: string;
 i, j, k: Integer;
begin
  //Get Text for template
  with TStringList.Create do
  try
    LoadFromFile(ATemplate);
    st := Text;
  finally
    Free;
  end;

  for i:=0 to AQuery.Results.Items.Count-1 do
  begin
    st2 := '';
    k := 0;
    for j:=1 to Length(st) do
      case st[j] of
        '%':
          begin
            if k = 0 then
            begin
              k := 1;
              lToken := '';
            end
            else
            begin
              st3 := AQuery.Results.Items[i].Properties.Value(lToken);
              if st3 = '' then
                st3 := AQuery.Results.Items.Value(lToken);
              if (st3 = '') and (FParent<>nil) then
                st3 := TScriptHandler(FParent).GetParam(lToken);
              st2 := st2 + st3;
              k := 0;
            end;
          end;

        ' ', #9, #13, #10:
          begin
            if k <> 0 then
            begin
              st2 := st2 + '%' + lToken;
              k := 0;
            end;
            st2 := st2 + st[j];
          end;

        else
          if k = 0 then
            st2 := st2 + st[j]
          else
            lToken := lToken + st[j];
      end;

    AOutput.Write(st2[1], Length(st2));
  end;
end;
{**********************************************************************}

end.
