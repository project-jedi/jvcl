{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvExchListboxes.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvExchListboxes;

interface

uses
  SysUtils, Classes, Dialogs,
  JvFormLists, JvBaseDlg;

type
  TJvExchListboxes = class(TJvCommonDialog)
  private
    FSecondCaption: string;
    FFirstCaption: string;
    FFirstListbox: TStringList;
    FSecondListbox: TStringList;
    FTitle: string;
    procedure SetFirstListbox(const Value: TStringList);
    procedure SetSecondListbox(const Value: TStringList);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FirstListbox: TStringList read FFirstListbox write SetFirstListbox;
    property SecondListbox: TStringList read FSecondListbox write SetSecondListbox;
    property FirstCaption: string read FFirstCaption write FFirstCaption;
    property SecondCaption: string read FSecondCaption write FSecondCaption;
    property Title: string read FTitle write FTitle;
    function Execute: Boolean; override;
  end;

implementation

resourcestring
  RC_Column1Caption = 'First Column';
  RC_Column2Caption = 'Second Column';
  RC_FormCaption = 'Listbox Editor';

constructor TJvExchListboxes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFirstListbox := TStringList.Create;
  FSecondListbox := TStringList.Create;
  FFirstCaption := RC_Column1Caption;
  FSecondCaption := RC_Column2Caption;
  FTitle := RC_FormCaption;
end;

function TJvExchListboxes.Execute: Boolean;
begin
  Result := TFormListb.Execute(FTitle, FFirstCaption, FSecondCaption, FFirstListbox, FSecondListbox);
end;

procedure TJvExchListboxes.SetFirstListbox(const Value: TStringList);
begin
  FFirstListbox.Assign(Value);
end;

procedure TJvExchListboxes.SetSecondListbox(const Value: TStringList);
begin
  FSecondListbox.Assign(Value);
end;

end.

