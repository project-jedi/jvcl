{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

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
